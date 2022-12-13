#time series data
tsdat <- read.table('Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/5주차/hw/tsdat.txt')

fit <- lm(y ~ x, data = tsdat)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))
# 등분산, normality를 만족하는 것으로 보인다. 
#하지만 time series이기 때문에 linear test를 해봐야 한다. Durbin-Watson Test
library(lmtest)
dwtest(fit) # time correlation이 있음이 명확하다. 

# Check ACF & PACF
# install.packages('astsa')
library(astsa)

# AR(p): ACF: Exponentially decreasing; PACF: Non-zero values at first p lags. > # MA(q): ACF: Non-zero values at first q lags; PACF: Exponentially decreasing. > # ARMA(p,q): ACF: Similar to ACF of AR(p); PACF: Similar to PACF of MA(q).

acf2(residuals(fit)) # acf는 exponentially decreasing하다는 것을 알 수 있고, pacf는 1 이후로 correlation이 0에 가깝다는 것을 알 수 있다. 
# lag = 1 이면 어지간한 time correlation을 잡을 수 있겠다는 추정이 가능 => AR(1)모델 사용
ar1=sarima(residuals(fit),p = 1,d = 0,q = 0,no.constant=T) #AR(1). p = AR모델의 차수, q = MA모델의 차수, no.constant : mean = 0으로 설정
ar1$fit
#time dependency가 사라짐. 
# 그럼 이걸 이용해서 어떻게 beta를 추정할 것인가의 문제를 풀면 된다. 

# MLE: Multivariate normal distribution
X = cbind(1,tsdat$x)
Y = tsdat$y
n = length(Y)
S = diag(rep(1,n))

mdif = 1000000
beta.old = rep(100000,2) 
I=0

while(mdif > 0.0000001){
  beta.new=as.vector(solve(t(X)%*%solve(S)%*%X)%*%t(X)%*%solve(S)%*%Y)
  r=as.vector(Y-(X%*%beta.new))
  ar1=sarima(r,1,0,0,no.constant=T,details=F)
  alpha=ar1$fit$coef
  sigma2=ar1$fit$sigma2 
  mdif=max(abs(beta.new-beta.old))
  beta.old=beta.new
  I=I+1
  #Constructcovariancematrix
  S=matrix(nrow=n,ncol=n)
  for(i in 1:n){
    for (j in 1:n){
      if (i == j) S[i,j] = 1
      if (i != j) S[i,j] = alpha^(abs(i-j))
    }
  }
  S=(sigma2/(1-alpha^2))*S
}
alpha
round(beta.new,4)

#########################################################
# Modelling Example 3:  Model with spatial correlations #
#########################################################

# install.packages('spdep')
library(spdep)

data(oldcol)

?COL.OLD
# 'COL.nb' has the neighbors list.

# 2-D Coordinates of observations
crds = cbind(COL.OLD$X, COL.OLD$Y)  

# Compute the maximum distance 
mdist = sqrt(sum(diff(apply(crds, 2, range))^2))   #neighborhood를 사용하기 위해서  

# All obs. between 0 and mdist are identified as neighborhoods.
dnb = dnearneigh(crds, 0, mdist)

# Compute Euclidean distance between obs.
dists = nbdists(dnb, crds) #각 obs간의 모든 거리를 구함

# Compute Power distance weight d^(-2)
glst = lapply(dists, function(d) d^(-2)) # 리스트별로 공간적 영향을 줄여주는 것ㄱ

# Construct weight matrix with normalization 
# style='C': global normalization; 'W': row normalization
lw = nb2listw(dnb, glist=glst, style='C')

# Spatial Autoregressive Model
fit = lagsarlm(CRIME ~ HOVAL + INC, data=COL.OLD, listw=lw)
summary(fit)

# install.packages('spatialreg')
library(spatialreg)

# Fitted values
predict(fit)

###################################################
# Modelling Example 4:  Generalized Linear Models #
###################################################

########## Cumulative logit model ##########
# install.packages('ordinal')
library(ordinal)

?wine

?clm

fit = clm(rating ~ temp + contact, data=wine, link = 'logit')
summary(fit)


########## Poisson regression model ##########

# For data
# install.packages('lme4')
library(lme4)

data(grouseticks)

?grouseticks

head(grouseticks)

hist(grouseticks$TICKS,breaks=0:90)

fit = glm(TICKS ~ HEIGHT*YEAR, data = grouseticks, family=poisson)
summary(fit)


########## Negative binomial regression model ##########
library(MASS)

fit1 = glm.nb(TICKS ~ HEIGHT*YEAR, data = grouseticks, link=log)
summary(fit1)

########## Proportional hazard model ##########
library(survival)

# For data
# install.packages('carData')
library(carData)

?Rossi

Surv(Rossi$week, Rossi$arrest)

fit = coxph(Surv(week,arrest) ~ fin + age+ race + wexp + mar + prio, 
            data=Rossi)
summary(fit)

# Estimated survival function
plot(survfit(fit),ylim=c(0.6,1),xlab="Weeks", ylab="Prop.of Not Rearrested")


# Estimated survival functions for financial aid
Rossi.fin = with(Rossi, data.frame(fin=c(0, 1), age=rep(mean(age), 2), 
                                   race=rep(mean(race=='other'),2), 
                                   wexp=rep(mean(wexp=="yes"),2), 
                                   mar=rep(mean(mar=="not married"),2),
                                   prio=rep(mean(prio),2)))

plot(survfit(fit,newdata=Rossi.fin), conf.int=TRUE,
     lty=c(1, 2), ylim=c(0.6, 1), col=c('red','blue'), 
     xlab="Weeks", ylab="Prop. of Not Rearrested")

legend("bottomleft", legend=c("fin = no","fin = yes"), 
       lty=c(1 ,2),col=c('red','blue'))



