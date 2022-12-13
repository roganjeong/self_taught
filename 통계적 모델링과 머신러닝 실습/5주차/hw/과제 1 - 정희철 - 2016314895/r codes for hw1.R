#3 - constructing the spatial weight matrix
library(spdep)
z1 <- c(5,1,2,4)
z2 <- c(3,4,2,1)
data <- cbind(z1,z2)
mdist <- sqrt(sum(diff(apply(data, 2, range))^2))
dnb = dnearneigh(data, 0, mdist)
dists = nbdists(dnb, data)
glst = lapply(dists, function(d) exp(-d))
#spatial weight matrix
glst
#---------------------------------------------------------------------------------------------------

#5
# 5-1
q5 <- read.csv('Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/5주차/Q5.csv')
summary(q5)
head(q5)

model1 <- lm(Y ~ X1 + X2 + X3, data = q5)
summary(model1)

model2 <- lm(Y ~ X2, data = q5)
summary(model2)


model3 <- lm(Y ~ X1 + X3, data = q5)
summary(model3)
residuals <- q5$Y - model3$fitted.values
plot(residuals)
#The residuals plot shows that the mean is roughly 0, but it seems to have a polynomial pattern.
#Since we do not have any information about the population distribution of the data, the use of GAM will be appropriate.

library(gam)
gam1 <- gam(Y ~ s(X1,2) + s(X3,2), data = q5)
gam1 <- gam(Y ~ s(X1,5) + s(X3,5), data = q5)
par(mfrow=c(1,2))
plot(gam1)
par(mfrow=c(1,1))
# It is evident that the residuals of X1 variable are linear and those of the X3 variable are quadratic.
# So we run a quadratic regression model. 
library(matrixcalc)
f = function(beta,X)
{
  X1 = X[,1]; X3 = X[,2]  
  beta[1] + beta[2]*X1 + beta[3]*X3 + beta[4]*X3^2
}
RSS = function(beta,Y,X) sum((Y-f(beta,X))^2)
grv = function(beta,Y,X)
{
  X1 = X[,1]; X3 = X[,2]
  R = Y - f(beta,X)
  c(-2*sum(R), -2*sum(R*X1), -2*sum(R*X3), -2*sum(R*X3^2)) 
}

X = cbind(q5$X1,q5$X3)
colnames(X) = c('X1', 'X3')
Y = q5$Y
ml1 = optim(par = rep(0.1,4), fn = RSS, gr=grv, method='BFGS', X=X, Y=Y) #  par = initial Beta's, fn = optimize할 objective function, gr = gradient vector를 지정하는 함수
ml1
beta.hat = ml1$par
beta.hat

obj.mean = function(beta,Y,X,S) t(Y-f(beta,X)) %*% solve(S) %*% (Y-f(beta,X))
gr.mean = function(beta,Y,X,S)
{
  sigma2 = diag(S)
  X1 = X[,1]; X3 = X[,2]
  R = Y - f(beta,X)
  c(-2*sum(R), -2*sum(R*X1), -2*sum(R*X3), -2*sum(R*X3^2))  
}

beta.new = ml1$par
W = diag(rep(1,length(Y)))
mdif = 100000

while(mdif > 0.000001)
{
  Yhat = f(beta.new,X)
  r = Y - Yhat 
  Z = cbind(1,Yhat)
  gam.hat = solve(t(Z) %*% W %*% Z) %*% t(Z) %*% W %*% abs(r)
  sigma = Z %*% gam.hat
  S = diag(as.vector(sigma^2))
  
  if (is.non.singular.matrix(S)) W = solve(S)
  else W = solve(S + 0.000000001*diag(rep(1,nrow(S)))) #variance function구할 때의 weights
  
  ml2 = optim(beta.new, obj.mean, gr=gr.mean,method='BFGS', Y=Y, X=X, S=S)
  beta.old = beta.new
  beta.new = ml2$par
  mdif = max(abs(beta.new - beta.old))
}

beta.new

Yhat = f(beta.new,X)
sigma = Z %*% gam.hat
r = (Y - Yhat)/sigma

# Residual plot
plot(Yhat,r)
#The residuals seem to be centered around 0 and have a roughly uniform variance. 