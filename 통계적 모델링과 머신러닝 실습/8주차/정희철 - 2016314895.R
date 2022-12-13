Q1 <- read.csv('Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/Q1.csv')
Q2 <- read.csv('Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/Q2.csv')
Q3 <- read.csv('Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/Q3.csv')

# 1-1
model123 <- lm(Y ~ X1 + X2 + X3, data = Q1)
summary(model123)
model1 <- lm(Y ~ X1, data = Q1)
summary(model1)
# X2 and X3 are irrelevant because the p-values of the full model are not significant. In addition, the model with only X1 variable has roughly the same coefficient of determination.

# 1-2
par(mfrow = c(2,2))
plot(model1)
# the residual plot has a cubic pattern.
library(gam)
gam1 = gam(Y ~ s(X1,5) + s(X2,5) + s(X3,5),data=Q1) 
par(mfrow = c(2,2))
plot(gam1)

f = function(beta,X)
{
  X1 = X[,1]; X2 = X[,2]; X3 = X[,3]
  beta[1] + beta[2]*X1 + beta[3]*X1^2 + beta[4]*X1^3 + beta[5]*X2 + beta[6]*X2^2 + beta[7]*X2^3 + beta[8]*X3 + beta[9]*X3^2 + beta[10]*X3^3 + beta[11]*X3^4 + beta[12]*X3^5
}

# Objective function: RSS
RSS = function(beta,Y,X) sum((Y-f(beta,X))^2)

# Gradient vector of the objective function
# RSS를 각 parameter마다 편미분
grv = function(beta,Y,X){
  X1 = X[,1] ; X2 = X[,2] ; X3 = X[,3] 
  R = Y - f(beta,X)
  c(-2*sum(R), -2*sum(R*X1), -2*sum(R*X1^2), -2*sum(R*X1^3), -2*sum(R*X2), -2*sum(R*X2^2), -2*sum(R*X2^3),-2*sum(R*X3), -2*sum(R*X3^2), -2*sum(R*X3^3), -2*sum(R*X3^4), -2*sum(R*X3^5))
}

# Optimization
X = cbind(Q1$X1,Q1$X2,Q1$X3)
colnames(X) = c('X1', 'X2','X3')
Y = Q1$Y
ml1 = optim(par = rep(0.1,12), fn = RSS, gr=grv, method='BFGS', X=X, Y=Y) #  par = initial Beta's, fn = optimize할 objective function, gr = gradient vector를 지정하는 함수. "BFGS" is a quasi-Newton method.

ml1
# ml1으로 새로운 beta coefficients들을 구하였으므로 이것에 대한 residual check 역시 해야 한다. 

beta.hat = ml1$par
beta.hat

# Fitted value
Yhat = f(beta.hat,X)
r = Y - Yhat
par(mfrow=c(1,1))
plot(Yhat,r)
#linearly increasing residuals

library(matrixcalc)
obj.mean = function(beta,Y,X,S) t(Y-f(beta,X)) %*% solve(S) %*% (Y-f(beta,X))
gr.mean = function(beta,Y,X,S)
{
  sigma2 = diag(S)
  X1 = X[,1]; X2 = X[,2]; X3 = X[,3]
  R = Y - f(beta,X)
  c(-2*sum(R/sigma2), -2*sum(R*X1/sigma2), -2*sum(R*X1^2/sigma2), -2*sum(R*X1^3/sigma2), -2*sum(R*X2/sigma2), -2*sum(R*X2^2/sigma2), -2*sum(R*X2^3/sigma2), -2*sum(R*X3/sigma2), -2*sum(R*X3^2/sigma2), -2*sum(R*X3^3/sigma2), -2*sum(R*X3^4/sigma2), -2*sum(R*X3^5/sigma2))  
}
beta.new = ml1$par      # initial parameter.
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
  else W = solve(S + 0.000000001*diag(rep(1,nrow(S)))) 
  
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
# random errors

# 1-2
beta.new

# 1-3
plot(Yhat,r)
# the model from (2) is the best model because it fits to the data well and has a random errors with mean of 0 and variance of 1.

# 2-1
lmod <- lm(Y ~ X, data = Q2)
lmod$coefficients

# 2-2
par(mfrow=c(2,2))
plot(lmod)
par(mfrow=c(1,1))
library(lmtest)
dwtest(lmod)
# the residuals seem to increase non-linearly

# 2-3
library(gam)
gam2 = gam(Y ~ s(X,5),data=Q2) 
par(mfrow = c(2,2))
plot(gam2)

f = function(beta,X)
{
  X1 = X
  beta[1] + beta[2]*X1 + beta[3]*X1^2 + beta[4]*X1^3
}

# Objective function: RSS
RSS = function(beta,Y,X) sum((Y-f(beta,X))^2)

grv = function(beta,Y,X){
  X1 = X
  R = Y - f(beta,X)
  c(-2*sum(R), -2*sum(R*X1), -2*sum(R*X1^2), -2*sum(R*X1^3))
}

# Optimization
X = Q2$X
colnames(X) = c('X1')
Y = Q2$Y
ml1 = optim(par = rep(0.1,4), fn = RSS, gr=grv, method='BFGS', X=X, Y=Y) #  par = initial Beta's, fn = optimize할 objective function, gr = gradient vector를 지정하는 함수. "BFGS" is a quasi-Newton method.

ml1


beta.hat = ml1$par
beta.hat

# Fitted value
Yhat = f(beta.hat,X)
r = Y - Yhat
par(mfrow=c(1,1))
plot(Yhat,r)
#linearly increasing residuals

library(matrixcalc)
obj.mean = function(beta,Y,X,S) t(Y-f(beta,X)) %*% solve(S) %*% (Y-f(beta,X))
gr.mean = function(beta,Y,X,S)
{
  sigma2 = diag(S)
  X1 = X
  R = Y - f(beta,X)
  c(-2*sum(R/sigma2), -2*sum(R*X1/sigma2), -2*sum(R*X1^2/sigma2), -2*sum(R*X1^3/sigma2))
}
beta.new = ml1$par      # initial parameter.
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
  else W = solve(S + 0.000000001*diag(rep(1,nrow(S)))) 
  
  ml2 = optim(beta.new, obj.mean, gr=gr.mean,method='BFGS', Y=Y, X=X, S=S)
  beta.old = beta.new
  beta.new = ml2$par
  mdif = max(abs(beta.new - beta.old))
}

Yhat = f(beta.new,X)
sigma = Z %*% gam.hat
r = (Y - Yhat)/sigma

# Residual plot
plot(Yhat,r)

#estimated parameters
beta.new

#2-4
#estimated covariance matrix of the error terms
r

#3
#stepAIC()
library(gam)
gam3 = gam(Y ~ s(X1,3) + s(X2,3) + s(X3,3) + s(X4,3) + s(X4,3),data=Q3) 
par(mfrow = c(2,2))
plot(gam3)

f = function(beta,X)
{
  X1 = X
  beta[1] + beta[2]*X1 + beta[3]*X2 + beta[4]*X3 + beta[5]*X4 + beta[6]*X5
}

# Objective function: RSS
RSS = function(beta,Y,X) sum((Y-f(beta,X))^2)

grv = function(beta,Y,X){
  X1 = X[,1]; X2 = X[,2]; X3 = X[,3]; X4 = X[,4]; X5 = X[,5]
  R = Y - f(beta,X)
  c(-2*sum(R), -2*sum(R*X1), -2*sum(R*X2), -2*sum(R*X3), -2*sum(R*X4), -2*sum(R*X5))
}

# Optimization
X = cbind(Q3$X1, Q3$X2, Q3$X3, Q3$X4, Q3$X5)
colnames(X) = c('X1', 'X2', 'X3', 'X4', 'X5')
Y = Q3$Y
ml1 = optim(par = rep(0.1,6), fn = RSS, gr=grv, method='BFGS', X=X, Y=Y) #  par = initial Beta's, fn = optimize할 objective function, gr = gradient vector를 지정하는 함수. "BFGS" is a quasi-Newton method.

ml1


beta.hat = ml1$par
beta.hat

# Fitted value
Yhat = f(beta.hat,X)
r = Y - Yhat
par(mfrow=c(1,1))
plot(Yhat,r)
#linearly increasing residuals

library(matrixcalc)
obj.mean = function(beta,Y,X,S) t(Y-f(beta,X)) %*% solve(S) %*% (Y-f(beta,X))
gr.mean = function(beta,Y,X,S)
{
  sigma2 = diag(S)
  X1 = X
  R = Y - f(beta,X)
  c(-2*sum(R/sigma2), -2*sum(R*X1/sigma2), -2*sum(R*X2/sigma2), -2*sum(R*X3/sigma2), -2*sum(R*X4/sigma2), -2*sum(R*X5/sigma2))
}
beta.new = ml1$par      # initial parameter.
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
  else W = solve(S + 0.000000001*diag(rep(1,nrow(S)))) 
  
  ml2 = optim(beta.new, obj.mean, gr=gr.mean,method='BFGS', Y=Y, X=X, S=S)
  beta.old = beta.new
  beta.new = ml2$par
  mdif = max(abs(beta.new - beta.old))
}

Yhat = f(beta.new,X)
sigma = Z %*% gam.hat
r = (Y - Yhat)/sigma

# Residual plot
plot(Yhat,r)

#estimated parameters
beta.new
