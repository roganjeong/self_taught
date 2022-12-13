# Question 1
library(mice)
library(rms)
library(finalfit)

training.data <- read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/11주차/train.csv", header = TRUE)
test.data <- read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/11주차/test.csv", header = TRUE)
describe(training.data)
md.pattern(training.data)
missing_pairs(training.data,'Y',c('X1','X2','X3','X4') )
md.pairs(training.data)
# The missing values of variable X3 seems to be significant.
# The missing values of variable X1, X2, and X4 do not seem to affect Y much so it can be imputed by predictive mean matching.

fit = glm(is.na(X3) ~ X1 + X2 + X4, data = training.data, family=binomial )
summary(fit)
# The p-values of X1 and X2 are significant; there are patterns in missing values.
# The p-value of X4 is not significant but it is still very close to 0.05, which can be viewed as significant.
# the missing values of X1, X2, and X4 will be imputed using predictive mean matching first. Then, the missing values will be imputed using its relationship with the rest of the input variables.

set.seed(0)
imp1 = mice(training.data, m=10, method=c('pmm','pmm','pmm','pmm',''), print=F)
imp1$predictorMatrix

pred = imp1$predictorMatrix
pred[,'Y'] = 0


imp2 = mice(training.data, m=10, method=c('pmm','pmm','pmm','pmm',''), 
            predictorMatrix = pred, print=F)

plot(imp2, c('X1','X2','X3','X4','Y'))
# 평균과 분산이 서로 얽혀 있고, 패턴을 보이지 않기 때문에 converge했다고 볼 수 있다. 

stripplot(imp2, pch=20, cex=1.2)
densityplot(imp2, scales=list(relation='free'),layout=c(2,1))

#위 2개의 그래프로 실제 데이터와 imputed 데이터가 유의한 차이는 없다는 것으로 추정.

fit = with(imp2, lm(Y ~ X1 + X2 + X3 + X4))
summary(pool(fit))

# Predict test obs.
M = imp2$m # the number of imputed sets
imp2.dat = vector(mode='list',length=M)
for (m in 1:M) imp2.dat[[m]] = complete(imp2,m) # 10개의 imputed set을 imp.dat에 넣어 준다.

p.model = function(training.data) lm(Y ~ X1 + X2 + X3 + X4, data=training.data)

fit.imp2 = lapply(imp2.dat, p.model)

test.obs = data.frame(X1 = test.data[,1], X2 = test.data[,2], X3 = test.data[,3], X4 = test.data[,4])

yhat = lapply(fit.imp2, predict, newdata=test.obs)
yhat = matrix(unlist(yhat),nrow(test.obs),M)

predicted.mean <- apply(yhat,1,mean)
test.mse <- sum((test.data[,5] - predicted.mean)^2)/length(predicted.mean)
# 20.54701


##########################################
# Question 2
training.pm25 <- read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/11주차/pm25_tr.csv", header = TRUE)
test.pm25 <- read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/11주차/pm25_te.csv", header = TRUE)

# Y variable의 normality check
plot(density(training.pm25$pm25)) # right-skewed
plot(density(log(training.pm25$pm25))) # log-transformation seems to be appprpriate


# There are 4 variables related to time; the year, month, day, and hour. 
# The year stays the same across all observations so it may be excluded.
training.pm25 <- training.pm25[,2:10]
test.pm25 <- test.pm25[,2:10]

# The month, day, and hour need to be checked.
training.pm25$month = as.factor(training.pm25$month) 
plot(x=training.pm25$month, y=log(training.pm25$pm25))

training.pm25$day = as.factor(training.pm25$day) 
plot(x=training.pm25$day, y=log(training.pm25$pm25))

training.pm25$hour = as.factor(training.pm25$hour) 
plot(x=training.pm25$hour, y=log(training.pm25$pm25))

# The month and hour do not seem to have a particular pattern.The day seem to vary a lot by days. 
# However, it should be reminded that the data is collected hourly, which means there can be time-correlation among data.
# A possible pattern in the month or day can be seen from hourly data so excluding the month and day also seems appropriate.
training.pm25 <- training.pm25[,3:9]
test.pm25 <- test.pm25[,3:9]

# 'hour' variable needs to be adjusted to use them as time-series data
training.pm25$hour = as.numeric(training.pm25$hour) 
for (i in 1:80){
  training.pm25[(24*i+1):(24*(i+1)+1),1] <- c((24*i):(24*(i+1)))
}
#1945번째 행이 새로 생겨서 지워준다.
training.pm25 <- training.pm25[1:1944,]


#일단 regression으로 넘어가자

# Since we do not have any information about the distribution of x variables, GAM is appropriate.
# 'pm25' variable is transformed by log-transformation.
library(gam)
training.pm25[,8] <- log(training.pm25[,2])
colnames(training.pm25) <- c('hour', 'pm25', 'DEWP', 'TEMP', 'PRES', 'cbwd', 'Iws', 'log.pm25')
fit.gam <- gam(log.pm25 ~ s(DEWP,5) + s(TEMP,5) + s(PRES,5) + s(Iws,5), data = training.pm25)
par(mfrow=c(2,2))
plot(fit.gam)
par(mfrow=c(1,1))
summary(fit.gam)
# 4개의 변수 모두 유의한 p-value를 갖는다. Multicollinearity가 있는지 확인해보는 것이 필요하다.
cor(training.pm25[,c(3,4,5,7)])
# TEMP와 PRES 변수 사이에 negative correlation이 있으므로, 두 변수 중 하나만 선택한다. 
cor(training.pm25[,c(2,4,5)])
cor(training.pm25[,c(2,4,5)],method='spearman')
cor(training.pm25[,c(2,4,5)],method='kendall')
# 그 어떤 correlation method로도 TEMP-pm25의 상관계수가 PRES-pm25의 상관계수보다 낮기 때문에 둘 중 PRES변수를 선택한다. 

fit.gam.2 <- gam(log.pm25 ~ s(DEWP,5) + s(PRES,5) + s(Iws,5), data = training.pm25)
par(mfrow=c(3,1))
plot(fit.gam.2)
par(mfrow=c(1,1))
# There are possible nonlinear patterns for each variable. ANOVA test is needed to choose. 
# DEWP has either a linear pattern or cubic pattern.
dewp.gam1 <- gam(log.pm25 ~ DEWP + s(PRES,5) + s(Iws,5), data = training.pm25) #linear
dewp.gam2 <- gam(log.pm25 ~ s(DEWP,3) + s(PRES,5) + s(Iws,5), data = training.pm25) # cubic
anova(dewp.gam1, dewp.gam2)
# p-value가 매우 유의하게 나오므로 DEWP는 3차식이 적절하다.

# PRES는 2차 또는 4차 비선형 패턴을 보인다. 
pres.gam1 <- gam(log.pm25 ~ s(DEWP,3) + s(PRES,2) + s(Iws,5), data = training.pm25) #quadratic
pres.gam2 <- gam(log.pm25 ~ s(DEWP,3) + s(PRES,4) + s(Iws,5), data = training.pm25) #4th degree
anova(pres.gam1, pres.gam2)
# p-value가 매우 유의하게 나오므로 PRES는 4차식이 적절하다. 

# Iws는 2차 또는 4차 비선형 패턴을 보인다. 
iws.gam1 <- gam(log.pm25 ~ s(DEWP,3) + s(PRES,4) + s(Iws,2), data = training.pm25) #quadratic
iws.gam2 <- gam(log.pm25 ~ s(DEWP,3) + s(PRES,4) + s(Iws,4), data = training.pm25) #4th degree
anova(iws.gam1, iws.gam2)
# 역시 p-value가 유의하게 나오므로 Iws는 4차식이 적절하다. 

model.gam <- gam(log.pm25 ~ s(DEWP,3) + s(PRES,4) + s(Iws,4), data=training.pm25)
lmod <- lm(log.pm25 ~ DEWP + I(DEWP^2) + I(DEWP^3) + PRES + I(PRES^2) + I(PRES^3) + + I(PRES^4) + Iws + I(Iws^2) + I(Iws^3) + + I(Iws^4), data = training.pm25)
summary(lmod)
####
plot(table(training.pm25$cbwd))
#cbwd는 나중에 생각해보자
####
#time-correlation을 고려해보자
library(astsa)
acf2(model.gam$residuals)
# The acf decreases gradually, and the pacf disappears after lag 1. AR(1) seems reasonable.

# predicted model : y = beta0 + beta1*DEWP + beta2*DEWP^2 + beta3*DEWP^3 + beta4*PRES + beta5*PRES^2 + beta6*PRES^3 + beta7*PRES^4 + beta8*Iws + beta9*Iws^2 + beta10*Iws^3 + beta11*Iws^4
f = function(beta,X)
{
  X1 = X[,1]; X2 = X[,2]; X3 = X[,3]
  beta[1] + beta[2]*X1 + beta[3]*X1^2 + beta[4]*X1^3 + beta[5]*X2 + beta[6]*X2^2 + beta[7]*X2^3+ beta[8]*X2^4 + beta[9]*X3 + beta[10]*X3^2 + beta[11]*X3^3+ beta[12]*X3^4
}
RSS = function(beta,Y,X) sum((Y-f(beta,X))^2)
grv = function(beta,Y,X)
{
  X1 = X[,1]; X2 = X[,2]; X3 = X[,3]
  R = Y - f(beta,X)
  c(-2*sum(R), -2*sum(R*X1), -2*sum(R*X1^2), -2*sum(R*X1^3), -2*sum(R*X2), -2*sum(R*X2^2), -2*sum(R*X2^3), -2*sum(R*X2^4), -2*sum(R*X3), -2*sum(R*X3^2), -2*sum(R*X3^3), -2*sum(R*X3^4))
}

X = cbind(training.pm25[,3], training.pm25[,5], training.pm25[,7])
colnames(X) = c('X1', 'X2', 'X3')
Y = training.pm25$log.pm25
ml1 = optim(par = rep(0.1,4), fn = RSS, gr=grv, method='BFGS', X=X, Y=Y) 
ml1
beta.hat = ml1$par

obj.mean <- function(beta, Y, X, S) t(Y - f(beta,X)) %*% solve(S) %*% (Y-f(beta,X))
gr.mean <- function(beta, Y, X, S){
  sigma2 <- diag(S)
  R <- Y - f(beta, X)
  c(-2*sum(R/sigma2), -2*sum(R*X1/sigma2), -2*sum(R*X1^2/sigma2), -2*sum(R*X1^3/sigma2), -2*sum(R*X2/sigma2), -2*sum(R*X2^2/sigma2), -2*sum(R*X2^3/sigma2), -2*sum(R*X2^4/sigma2), -2*sum(R*X3/sigma2), -2*sum(R*X3^2/sigma2), -2*sum(R*X3^3/sigma2), -2*sum(R*X3^4/sigma2))
}

n <- length(Y)
S <- diag(rep(1,n))

mdif <- 1000000
beta.new <- ml$par
beta.old <- rep(1000000,3)

while(mdif > 0.0000001){
  ml1 <- optim(beta.new, obj.mean, gr = gr.mean, method = 'BFGS', Y=Y, X=X, S=S)
  beta.new <- ml1$par
  r <- as.vector(Y - f(beta.new, X))
  ar1 <- sarima(r, 1,0,0, no.constant=T, details=F)
  alpha <- ar1$fit$sigma2
  mdif <- max(abs(beta.new - beta.old))
  beta.old <- beta.new
  
  S <- matrix(nrow = n, ncol = n)
  for (i in 1:n){
    for(j in 1:n){
      if (i==j) S[i,j]=1
      if (i!=j) S[i,j]=alpha^(abs(i-j))
    }
  }
  S <- (sigma2 / (1-alpha^2)) * S
}

round(beta.new, 4)
