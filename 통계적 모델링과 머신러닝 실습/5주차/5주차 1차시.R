library(datasets)
?attenu
dim(attenu)

# event, mag, station number, station-hypocenter distance를 이용해서  accel을 예측하는 문제 
# mag, dist, accel만 continuous, 나머지는 categorical
# accel이 Y변수
head(attenu)

#먼저 대략적으로 어떻게 분석할지 감을 잡기 위해 ordinary linear regression을 돌려준다.
fit1 <- lm(accel ~ mag + dist,data = attenu)
summary(fit1)
# mag, dist 둘다 유의하다. coefficient of determination 자체는 별로 높지 않다. 
plot(fit1$residuals)
par(mfrow=c(2,2))
plot(fit1)
par(mfrow=c(1,1))
# residual에 패턴이 존재한다. 이분산이 존재하는 것으로 추정.
#선형이 아닌 회귀모형이 필요한데 아무런 정보가 없는 상태이다. 이렇게 정보가 아무것도 없을 때 사용할 수 있는 것은 GAM이다. 
library(gam)
fit2 <- gam(accel ~ s(mag,df = 5) + s(dist,df = 5), data = attenu) #smoothing splines
par(mfrow = c(1,2))
plot(fit2) # dist는 exponential하게 감소됨이 명확하다. mag는 linear한지 nonlinear한지 명확하지 않기 때문에 test를 해봐야 한다. anova를 해서 F-test를 해볼 수 있다. 
par(mfrow = c(1,1))

fit2_1 <- gam(accel ~ mag + s(dist, 5), data = attenu) #mag를 비선형으로 가정한 fit2와 선형으로 가정한 fit2_1 비교
anova(fit2, fit2_1) # p-value가 유의하게 나왔으므로 비선형으로 가정하는게 적절하다.

########## Nonlinear Model with constant variance ##########
# Y = accel, X1 = mag, X2 = dist.
# Nonlinear model: Y = beta1 + beta2*X1 + beta3*X1^2 + beta4*exp(-beta5*X2)
# beta4는 decreasing scale을 잡아주는 역할, beat5는 decreasing rate을 조절하는 역할
# mag에 대해서는 polynomial regression, dist에 대해서는 exponential regression을 취한다. 
# dist가 비선형이라 LSE와 같은 잣대로 결정할 수 없다. 

f = function(beta,X){
  X1=X[,1];X2=X[,2]
  beta[1]+beta[2]*X1+beta[3]*X1^2+beta[4]*exp(-beta[5]*X2)}

# Objective function: RSS
RSS = function(beta,Y,X) sum((Y-f(beta,X))^2)
# Gradient vector of the objective function
grv = function(beta,Y,X){
  X1=X[,1];X2=X[,2]
  R=Y-f(beta,X)
  c(-2*sum(R),-2*sum(R*X1),-2*sum(R*X1^2),-2*sum(R*exp(-beta[5]*X2)), + 2*beta[4]*sum(R*X2*exp(-beta[5]*X2)))}

# Optimization
X = cbind(attenu$mag,attenu$dist)
colnames(X) = c('mag', 'dist')
Y = attenu$accel
ml1 = optim(par = rep(0.1,5), fn = RSS, gr=grv, method='BFGS', X=X, Y=Y) # par = initial beta for estimation, fn = objective function for optimization, gr = gradient vector를 지정해주는 함수 
ml1
# ml1으로 새로운 beta coefficients들을 구하였으므로 이것에 대한 residual check 역시 해야 한다. 

beta.hat <- ml1$par 
Yhat <- f(beta.hat, X)

r <- Y - Yhat
plot(Yhat, r, ylim = c(-0.5, 0.5))
lines(c(0,10),c(0,0), col = 'red')
#residual의 분산이 점점 커지는 패턴을 가짐을 확인할 수 있음. 등분산 X
# 이럴 경우 Linear Variance Function을 사용하는게 적절하다.


# Objective function for mean function: Genearalized least square method.
obj.mean = function(beta,Y,X,S) t(Y-f(beta,X)) %*% solve(S) %*% (Y-f(beta,X))
# S: Covariance matrix
#5주차 1차시 52분 19초에 나오는 식을 구현한 것

# Gradient vector of the objective function
gr.mean = function(beta,Y,X,S){
  sigma2=diag(S)
  X1=X[,1];X2=X[,2]
  R=Y-f(beta,X)
  c(-2*sum(R/sigma2),-2*sum(R*X1/sigma2),-2*sum(R*X1^2/sigma2),
  -2*sum(R*exp(-beta[4]*X2)/sigma2),
  -2*beta[3]*sum(R*X2*exp(-beta[4]*X2)/sigma2))}


# Linear variance function: |r| = gam1 + gam2*Yhat.
# For linear variance function, we can consider absolute residuals, > # instead of squared residuals.
# gam.hat = (Z^T W Z)^(-1) Z^T W |r|.

beta.new = ml1$par # initial parameter.
W = diag(rep(1,length(Y)))
mdif = 100000

while(mdif > 0.000001){
  Yhat=f(beta.new,X)
  r=Y-Yhat
  Z=cbind(1,Yhat)
  gam.hat=solve(t(Z)%*%W%*%Z)%*%t(Z)%*%W%*%abs(r)
  sigma=Z%*%gam.hat
  S=diag(as.vector(sigma^2)) #objective mean function에 사용될 sigma
  W=solve(S) #variance function구할 때의 weights
  
  ml2=optim(beta.new,obj.mean,gr=gr.mean,method='BFGS',Y=Y,X=X,S=S)
  beta.old=beta.new
  beta.new=ml2$par
  mdif=max(abs(beta.new-beta.old))}
 
Yhat = f(beta.new,X)
sigma = Z %*% gam.hat
r = (Y - Yhat)/sigma

# Residual plot
plot(Yhat,r,ylim=c(-4,4))
lines(c(0,10),c(0,0),col='red')
#random error의 형태를 띔.
# 최종 모델은 이 variance function을 포함한 모델이다. 