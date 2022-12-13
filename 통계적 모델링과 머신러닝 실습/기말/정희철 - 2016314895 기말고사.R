q1 = read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/기말/SMQ1.csv", header = T)

#1-1
library(mice)
library(rms)
library(finalfit)
describe(q1)
md.pattern(q1)
missing_pairs(q1,'wage',c('year','age','maritl','race','education','jobclass') )
# the diagram shows there may be patterns in missing values in race,  because the difference in the distribution of the complete data and incomplete data seems significant, in terms of dispersion and the center.


#1-2
# the workers with missing value in race tend to have higher wage than the rest.

#1-3
# Missing values in wage should be excluded from the analysis because it might distort the actual data if we impute them, which leads to a bad performance of the model.

#########################
#2-1
q2.train = read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/기말/SMQ2train.csv", header = T)
q2.test = read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/기말/SMQ2test.csv", header = T)
q2.train[,1] <- as.factor(q2.train[,1])
q2.test[,1] <- as.factor(q2.test[,1])
library(lattice)
library(SIS)
# Relief algorithm을 통해 가장 contribution이 높은 변수 2개를 찾아본다
# Relief algorithm
library(CORElearn)

RE = attrEval(Y ~ ., data=q2.train, estimator='Relief',ReliefIterations=30)
SRE = sort(RE, decreasing = T) #내림차순으로 정리
SRE
par(mfrow=c(1,1))
plot(1:length(SRE),SRE, type='b', ylab='Separability',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SRE), labels=names(SRE), cex.axis=0.8,las=2)
head(SRE)
# X23, X66 have the greatest contribution

plot(q2.train[,'X23'], q2.train[,'X66'], col = q2.train[,'Y'])


#2-2
head(SRE) # X23       X66       X34        X6       X29       X79 

q2.train <- q2.train[,c('Y','X23','X66','X34','X6','X29','X79')]
q2.test <- q2.test[,c('Y','X23','X66','X34','X6','X29','X79')]

misclass = function(cm) 1 - sum(diag(cm))/sum(cm)
fmeasure = function(cm)
{
  TPR = cm[2,2]/sum(cm[2,])
  PPV = cm[2,2]/sum(cm[,2])
  return((2*TPR*PPV)/(TPR + PPV)) 
}

fit = glm(Y~., family=binomial, data=q2.train)
phat.test = predict(fit, q2.test, type='response')
yhat.test = ifelse(phat.test >0.5, 1, 0)
cm  = table(true = q2.test$Y, predict=yhat.test)
cm
misclass(cm) # 0.6632997



######################
#3
q3.train = read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/기말/SMQ3train.csv", header = T)
q3.test = read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/기말/SMQ3test.csv", header = T)
q3.train[,'Class'] <- as.factor(q3.train[,'Class']) 
q3.test[,'Class'] <- q3.test[,'Class']

# 먼저 Class분류를 하기에 가장 contribution이 높은 변수를 추려내기 위해 Relief Algorithm을 사용한다. 

# Relief algorithm
library(CORElearn)

RE = attrEval(Class ~ ., data=q3.train, estimator='Relief',ReliefIterations=30)
SRE = sort(RE, decreasing = T) #내림차순으로 정리
SRE
par(mfrow=c(1,1))
plot(1:length(SRE),SRE, type='b', ylab='Separability',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SRE), labels=names(SRE), cex.axis=0.8,las=2)
head(SRE,10)
# Elbowpoint를 보아 V4, V13, V19가 Class를 분류하기에 적절한 변수들이다.

q3.train <- q3.train[,c('V4','V13','V19','Class')]
q3.test <- q3.test[,c('V4','V13','V19','Class')]

misclass = function(cm) 1 - sum(diag(cm))/sum(cm)
fmeasure = function(cm)
{
  TPR = cm[2,2]/sum(cm[2,])
  PPV = cm[2,2]/sum(cm[,2])
  return((2*TPR*PPV)/(TPR + PPV)) 
}

# Binary classification이기 때문에 glm에서 binomial을 선택하고 모델을 만들어 본다. 그리고 test set으로 모델의 성능을 구한다.

fit = glm(Class~., family=binomial, data=q3.train)
phat.test = predict(fit, q3.test, type='response')
yhat.test = ifelse(phat.test > 0.5, 1, 0)
cm  = table(true = q3.test$Class, predict=yhat.test)
cm
misclass(cm)
fmeasure(cm) # 0.731405
