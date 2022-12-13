q1 = read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/기말족보/dataset/Q1.csv", header = T)
library(gam)
#1-1
lin1 <- lm(Y~., data = q1)
summary(lin1)
gam1 <- gam(Y ~ s(X1,df = 5) + s(X2,df = 5) + s(X3,df = 5), data = q1) #smoothing splines
summary(gam1)
par(mfrow = c(2,2))
plot(gam1)
par(mfrow = c(1,1))
# X1 : linear
# X2 : 5th degree polynomial regression
# X3 : quadratic
# they are all significant

#1-2
f = function(beta,X){
  X1 = X[,1]
  X2 = X[,2]
  X3 = X[,3]
  beta[1] + beta[2]*X1 + beta[3]*X2 + beta[4]*X2^2 + beta[5]*X2^3 + beta[6]*X2^4 + beta[7]*X2^5 + beta[8]*X3 + beta[9]*X3^2}

# RSS
RSS = function(beta,Y,X) sum((Y-f(beta,X))^2)
# Gradient vector
grv = function(beta,Y,X){
  X1=X[,1]
  X2=X[,2]
  X3=X[,3]
  R=Y-f(beta,X)
  c(-2*sum(R) ,-2*sum(R*X1) ,-2*sum(R*X2) ,-2*sum(R*X2^2) ,-2*sum(R*X2^3) ,-2*sum(R*X2^4) ,-2*sum(R*X2^5) ,-2*sum(R*X3) ,-2*sum(R*X3^2)) }

# Optimization
X = cbind(q1$X1, q1$X2, q1$X3)
colnames(X) = c('X1', 'X2', 'X3')
Y = q1$Y
ml1 = optim(par = rep(0.1,9), fn = RSS, gr=grv, method='BFGS', X=X, Y=Y) 
ml1


beta.hat <- ml1$par 
Yhat <- f(beta.hat, X)

r <- Y - Yhat
plot(Yhat, r)

#residual의 분산이 점점 커지는 패턴을 가짐을 확인할 수 있음. 등분산 X
# 이럴 경우 Linear Variance Function을 사용하는게 적절하다.

obj.mean = function(beta,Y,X,S) t(Y-f(beta,X)) %*% solve(S) %*% (Y-f(beta,X))

gr.mean = function(beta,Y,X,S){
  sigma2=diag(S)
  X1=X[,1]
  X2=X[,2]
  X3=X[,3]
  R=Y-f(beta,X)
  c(-2*sum(R/sigma2) ,-2*sum(R*X1/sigma2) ,-2*sum(R*X2/sigma2) ,-2*sum(R*X2^2/sigma2) ,-2*sum(R*X2^3/sigma2) ,-2*sum(R*X2^4/sigma2) ,-2*sum(R*X2^5/sigma2) ,-2*sum(R*X3/sigma2) ,-2*sum(R*X3^2/sigma2)) }

beta.new = ml1$par # initial parameter.
W = diag(rep(1,length(Y)))
mdif = 100000

while(mdif > 0.000001)
{
  Yhat = f(beta.new,X)
  r = Y - Yhat #보통 squared residuals를 사용하는데 이 예제에서는 absolute residuals를 사용하였다. 
  Z = cbind(1,Yhat)
  gam.hat = solve(t(Z) %*% W %*% Z) %*% t(Z) %*% W %*% abs(r)
  sigma = Z %*% gam.hat
  S = diag(as.vector(sigma^2))#objective mean function에 사용될 sigma
  
  #if (is.non.singular.matrix(S)) W = solve(S)
  #else W = solve(S + 0.000000001*diag(rep(1,nrow(S)))) #variance function구할 때의 weights
  
  ml2 = optim(beta.new, obj.mean, gr=gr.mean,method='BFGS', Y=Y, X=X, S=S)
  beta.old = beta.new
  beta.new = ml2$par
  mdif = max(abs(beta.new - beta.old))
}

Yhat = f(beta.new,X)
sigma = Z %*% gam.hat
r = (Y - Yhat)/sigma
#1-3
# Residual plot
plot(Yhat,r)

#1-4
#######
#2
q2 = read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/기말족보/dataset/Q2.csv", header = T)

pr = prcomp(q2[,-1], center = TRUE, scale = TRUE)
summary(pr)
# 변수마다 설명하는 정도가 고만고만함
#install.packages('GGally')
library(GGally)
ggpairs(q2[,-1])
head(q2)
library(ordinal)
fit <- clm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = q2, link = 'logit' )
summary(fit)
# X3 and X5 seems to have the greatest contribution. X3 is significant in terms of p-value but X5 is not.
#kernel PCA가 좋을 것 같은걸
library(kernlab)
x <- q2[,-1]
y <- as.factor(q2[,1])

fit <- kpca(~., data = x,kernel='rbfdot', kpar=list(sigma=15), features=2)
pc = pcv(fit)

one = pc[y=='1',]
zero = pc[y=='0',]

par(mfrow=c(1,1))
plot(one, col='red', xlab='KPC1',ylab='KPC2')
points(zero, col='blue')
# 안되네... principal curve나 principal surface를 해봐야 겠다.

library(princurve)
library(boot)
fit <- principal_curve(as.matrix(q2[,-1]))
par(mfrow=c(1,2))
plot(density(fit$lambda[q2$Y == '1']),col='red',main='Principal Curve')
lines(density(fit$lambda[q2$Y == '0']),col='blue')

plot(density(pc.dat[q2$Y == '1',2]),col='red', main='PC1')
lines(density(pc.dat[q2$Y == '0',2]),col='blue')

dat1 = cbind(q2, pcurve=fit$lambda)

fit2 = glm(Y~pcurve, data=dat1, family=binomial)
cv.glm(dat1, fit2, K=5)$delta # 위에 line 67에 principal component로 구한 것보다 안좋다. 

# Projection onto Principal curve
#새로운 데이터를 principal curve에 projection해서 lambda값을 구하는 단계
new.obs = as.matrix(dat[31:40,2:31])
project_to_curve(new.obs,fit$s)$lambda 

#일단 다른거 먼저 하자

#######################
#3-1
library(CORElearn)
q3 = read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/기말족보/dataset/Q3.csv", header = T)
set.seed(1)
# Relief algorithm
RE = attrEval(Y ~ ., data=q3, estimator='Relief',ReliefIterations=30)
SRE = sort(RE, decreasing = T) #내림차순으로 정리
SRE
par(mfrow=c(1,1))
plot(1:length(SRE),SRE, type='b', ylab='Separability',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SRE), labels=names(SRE), cex.axis=0.8,las=2)
head(SRE,10) # X54, X205, X1173, X936, X827, X1376, X1087, X1939, X1588, X911

# ReliefF algorithm
# 여러개를 랜덤하게 뽑고 한꺼번에 계산하는 것
REF = attrEval(Y ~ ., data=q3, estimator='ReliefFequalK', ReliefIterations=30)

SREF = sort(REF, decreasing = T)
SREF

plot(1:length(SREF),SREF, type='b', ylab='Separability',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SREF), labels=names(SREF), cex.axis=0.8,las=2)
head(SREF, 10) # X1055, X956, X1979, X1400, X749, X698, X1207, X199, X948, X1973
#Relief algorithm의 값들이 상대적으로 더 높게 나왔으므로 Relief Algorithm의 답을 사용.

#3-2
library(SIS)
# interpretation이 중요하다 했으니까
q3 = read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/기말족보/dataset/Q3.csv", header = T)
head(q3)
x
# logistic regression
# ISIS with regularization
model21=SIS(as.matrix(q3[,-1]), as.vector(q3[,1]), family='binomial', tune='bic', penalty='SCAD', perm=T, q=0.9)
model21$ix

model22=SIS(as.matrix(q3[,-1]), as.vector(q3[,1]), family='binomial', tune='bic', varISIS='aggr', seed=21)
model22$ix
# answer : X95,  X833, X979, X1055, X1058
#3-3
# Variable selection under the variable importance heavily relies on the correlation coefficients, which may cause multicollinearity. 
# Variable selection with variable importance only measures the relationship between the response variable and each x variables individually, which does not include interactions among x variables. 

####################
q4.train = read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/기말족보/dataset/Q4train.csv", header = T)
q4.test = read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/기말족보/dataset/Q4test.csv", header = T)

# pc는 안됨
nrow(q4.train)
nrow(q4.train[q4.train[,1]==1,])
nrow(q4.train[q4.train[,1]==0,])
# number of 0's : 3781
# number of 1's : 219
# anomaly detection method maybe useful
library(DMwR)
library(caret)
library(e1071)
train.x0 = q4.train[q4.train$Y == 0,]

train.cv1 <- q4.train[c(1:800),]
train.cv1 <- train.cv1[train.cv1$Y == 0, -1]
test.cv1 <- q4.train[1:800,]
train.cv2 <- q4.train[c(801:1600),]
train.cv2 <- train.cv2[train.cv2$Y == 0, -1]
test.cv2 <- q4.train[801:1600,]
train.cv3 <- q4.train[c(1601:2400),]
train.cv3 <- train.cv3[train.cv3$Y == 0, -1]
test.cv3 <- q4.train[1601:2400,]
train.cv4 <- q4.train[c(2401:3200),]
train.cv4 <- train.cv4[train.cv4$Y == 0, -1]
test.cv4 <- q4.train[2401:3200,]
train.cv5 <- q4.train[c(3201:4000),]
train.cv5 <- train.cv5[train.cv5$Y == 0, -1]
test.cv5 <- q4.train[3201:4000,]

result1 = NULL
for (nu in c(0.001,0.01,0.1,0.3,0.5,0.7,0.9))
{
  for (gamma in c(0.01,0.1,0.5,1,2,3,5))
  {
    svddfit = svm(x=train.cv1, type='one-classification', kernel='radial',
                  nu=nu, gamma=gamma)
    
    minor = predict(svddfit,test.cv1[,-1])                         #엄밀히 말해선 이 부분을 5-fold cv로 돌려야 한다. 
    # predict: TRUE: small class(outlier), FALSE: large class  #
    yhat.te = numeric(nrow(test.cv1))                              #
    yhat.te[minor==TRUE] = 1                                   #
    cm = table(true = test.cv1$Y, predict=yhat.te)
    result1 = rbind(result1, c(nu,gamma,fmeasure(cm)))
  }
}
names(result1) <- c('nu','gamma','F-measure')
result1[which.max(result1[,3]),]

result2 = NULL
for (nu in c(0.001,0.01,0.1,0.3,0.5,0.7,0.9))
{
  for (gamma in c(0.01,0.1,0.5,1,2,3,5))
  {
    svddfit = svm(x=train.cv2, type='one-classification', kernel='radial',
                  nu=nu, gamma=gamma)
    
    minor = predict(svddfit,test.cv2[,-1])                         #엄밀히 말해선 이 부분을 5-fold cv로 돌려야 한다. 
    # predict: TRUE: small class(outlier), FALSE: large class  #
    yhat.te = numeric(nrow(test.cv2))                              #
    yhat.te[minor==TRUE] = 1                                   #
    cm = table(true = test.cv2$Y, predict=yhat.te)
    result2 = rbind(result2, c(nu,gamma,fmeasure(cm)))
  }
}
names(result2) <- c('nu','gamma','F-measure')
result2[which.max(result2[,3]),]


result3 = NULL
for (nu in c(0.001,0.01,0.1,0.3,0.5,0.7,0.9))
{
  for (gamma in c(0.01,0.1,0.5,1,2,3,5))
  {
    svddfit = svm(x=train.cv3, type='one-classification', kernel='radial',
                  nu=nu, gamma=gamma)
    
    minor = predict(svddfit,test.cv3[,-1])                         #엄밀히 말해선 이 부분을 5-fold cv로 돌려야 한다. 
    # predict: TRUE: small class(outlier), FALSE: large class  #
    yhat.te = numeric(nrow(test.cv3))                              #
    yhat.te[minor==TRUE] = 1                                   #
    cm = table(true = test.cv3$Y, predict=yhat.te)
    result3 = rbind(result3, c(nu,gamma,fmeasure(cm)))
  }
}
names(result3) <- c('nu','gamma','F-measure')
result3[which.max(result3[,3]),]

result4 = NULL
for (nu in c(0.001,0.01,0.1,0.3,0.5,0.7,0.9))
{
  for (gamma in c(0.01,0.1,0.5,1,2,3,5))
  {
    svddfit = svm(x=train.cv4, type='one-classification', kernel='radial',
                  nu=nu, gamma=gamma)
    
    minor = predict(svddfit,test.cv4[,-1])                         #엄밀히 말해선 이 부분을 5-fold cv로 돌려야 한다. 
    # predict: TRUE: small class(outlier), FALSE: large class  #
    yhat.te = numeric(nrow(test.cv4))                              #
    yhat.te[minor==TRUE] = 1                                   #
    cm = table(true = test.cv4$Y, predict=yhat.te)
    result4 = rbind(result4, c(nu,gamma,fmeasure(cm)))
  }
}
names(result4) <- c('nu','gamma','F-measure')
result4[which.max(result4[,3]),]


result5 = NULL
for (nu in c(0.001,0.01,0.1,0.3,0.5,0.7,0.9))
{
  for (gamma in c(0.01,0.1,0.5,1,2,3,5))
  {
    svddfit = svm(x=train.cv5, type='one-classification', kernel='radial',
                  nu=nu, gamma=gamma)
    
    minor = predict(svddfit,test.cv5[,-1])                         #엄밀히 말해선 이 부분을 5-fold cv로 돌려야 한다. 
    # predict: TRUE: small class(outlier), FALSE: large class  #
    yhat.te = numeric(nrow(test.cv5))                              #
    yhat.te[minor==TRUE] = 1                                   #
    cm = table(true = test.cv5$Y, predict=yhat.te)
    result5 = rbind(result5, c(nu,gamma,fmeasure(cm)))
  }
}
names(result5) <- c('nu','gamma','F-measure')
result5[which.max(result5[,3]),]
### 이상치 탐지 존나 못하는데...?


#SMOTE
library(DMwR)
nrow(q4.train[q4.train[,1]==1,]) #219 + 1781 = 2000
nrow(q4.train[q4.train[,1]==0,]) #3781 - 1781 = 2000
q4.train$Y <- as.factor(q4.train$Y)
set.seed(1)
smtrain = SMOTE(Y~., data=q4.train, perc.over=1781, k = 5, perc.under=1781)

dim(smtrain)
table(smtrain$Y)

'''
# Scatter plot for the training data from SMOTE
plot(smtrain$X1,smtrain$X2,
     col=(3-as.numeric(smtrain$y)),xlab='X1',ylab='X2')
'''


# SVM for data from SMOTE.
set.seed(1)
cv.fit3 = tune(svm, Y~., data=smtrain, kernel='radial', 
               ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),
                           gamma=c(0.01,0.1,0.5,1,2,3,4)))
summary(cv.fit3)

# SVM with the best parameters
best.fit3 = cv.fit3$best.model

# Prediction for test data
yhat.te3 = predict(best.fit3, q4.test[,-1])
cm3 = table(true = q4.test$Y, predict=yhat.te3)

misclass(cm3)

fmeasure(cm3)
cm3
