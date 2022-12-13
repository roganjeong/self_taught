############################################
# Statistical Modelling & Machine Learning #
#               R Example5                 #
############################################

options(warn = -1)  # Turn off warning message

# Data generation
install.packages('mvtnorm')
library(mvtnorm)

set.seed(10)
n1 = 500; n2 = 50 # imbalanced data를 만들기 위해. 만약 이 두 그룹이 겹치지 않고 완전히 separable하다면 imbalanced data는 decision boundary를 형성하는데 문제가 되지 않는다. 하지만 Seperability가 imbalanced data의 문제를 심화시킬 수 있다. 
# imbalanced data를 단순히 모델링하게 되면 모델은 데이터 사이즈가 큰 변수에 최적화되는 경향이 있기 때문이다. 그리하여 데이터 사이즈가 작은 데이터를 잘 포착하게 최적화가 된다면 전체적인 error rate는 올라가기 마련이다. 
mean1 = c(6,6)
mean2 = c(4,5)
sig1 = matrix(c(2,0,0,2),2,2)
sig2 = matrix(c(2,-0.8,-0.8,2),2,2)

x = rbind(rmvnorm(n1,mean1,sig1),rmvnorm(n2,mean2,sig2))
y = as.factor(c(rep(0,n1),rep(1,n2)))
train = data.frame(y,x)

x = rbind(rmvnorm(n1,mean1,sig1),rmvnorm(n2,mean2,sig2))
y = as.factor(c(rep(0,n1),rep(1,n2)))
test = data.frame(y,x)


# Scatter plot for the training data
plot(cbind(train$X1,train$X2),
     col=(3-as.numeric(train$y)),xlab='X1',ylab='X2')
#상당 부분 겹치는 것을 확인할 수 있다.
head(train)
# 데이터 사이즈가 큰 변수를 보통 0으로 두고, 그렇지 않은걸 1로 두는 경우가 많다. 
# Classifier: Logistic regression 
fit = glm(y~., family=binomial, data=train)
phat.test = predict(fit, test, type='response')
yhat.test = ifelse(phat.test > 0.5, 1, 0) # 0.5가 decision boundary. 0.5보다 크면 1, 작으면 0으로 분류한다. 

cm  = table(true = test$y, predict=yhat.test)
cm # confusion matrix
# Error rate는 낮을테지만 F-measure기준으로는 매우 좋지 않을 결과가 나왔다. 

misclass = function(cm) 1 - sum(diag(cm))/sum(cm)
# diag(cm)은 올바르게 분류된 케이스들
# cm: confusion matrix (higher value = positive class)
# misclassification rate는 낮을수록 좋고, F-measure는 높을수록 좋다. 
fmeasure = function(cm)
{
  TPR = cm[2,2]/sum(cm[2,])
  PPV = cm[2,2]/sum(cm[,2])
  return((2*TPR*PPV)/(TPR + PPV)) 
}

# Test misclassification rate
misclass(cm)

# Test F-measure
fmeasure(cm)
# misclassification rate는 낮은데 F-measure도 낮은 모델을 생성한게 되었다. 이는 imbalanced data의 typical한 경우.
# F-measure가 0.2424242가 나왔다는 것은 우리가 실제로 제대로 분류하길 바라는 class에 대해서 실패했다는 의미이기에 좋은 모델이 될 수 없다. 
# 기본적으로 우리가 배울 모델들은 imbalaced data가 가진 문제점에 대해서만 보완이 가능하다. 즉, 데이터가 실제로 많이 겹쳐져 실제 decision boundary자체가 모호한 데이터 같은 경우는 더 발전시킬 수 있는 방법이 없다. 
# 즉, 주의할 점은 imbalanced data가 낮은 F-measure에 기여할 순 있지만 절대적인 기여는 하지 않는다. 

# ROC curve ------------------------------------------
#install.packages('pROC')

library(pROC)

phat.tr = predict(fit, train, type='response') # training set으로 cut-off point 만들기 
  
lr.roc = roc(train$y ~ phat.tr) # ROC curve 만들기, 실제 y값과 확률값들에 따라 cut-off point를 조정한다. 
plot(lr.roc)

# AUC
auc(lr.roc) # classifier의 performance measure, 하지만 misclassification rate와 비슷한 문제를 가진다. 

########## Alternate cut-off (using training dataset) ##########
  # Fird the closest point on ROC curve to (1,1).
th = coords(lr.roc,x='best', best.method = 'closest.topleft')
th # 0과 1을 분류해내는 새로운 cut-off value
# threshold = 0.069보다 작으면 0, 크면 1로 분류. 원래 threshold가 0.5인 것을 감안하면 매우 낮은 threshold이다. 

# Evaluation for the new cut-off
yhat.test1 = ifelse(phat.test > th$threshold, 1, 0) #새로운 threshold를 이용해서 다시 prediction을 진행

cm1 = table(true = test$y, predict=yhat.test1)
cm1

# Test misclassification rate
misclass(cm1)

# Test F-measure
fmeasure(cm1)
# misclassification rate 자체는 높아졌지만, F-measure는 높아졌다. 

################### Adjusting prior prob. ####################
library(MASS)
# prior probability : population에서 random하게 select했을 때 0그룹에 속할 확률과 1그룹에 속할 확률
# 사이즈가 작은 그룹은 태생적으로 적은 수이기 때문에 prior probability를 작게 줘야 하는 것이고, 사이즈가 큰 그룹은 prior probability를 크게 잡아줘야 하는 것이 맞다. 
# 하지만 이렇게 진행하면 imbalanced data의 문제가 더 심화될 수 있기 때문에 작은 그룹의 prior probability를 뻥튀기한다. 
fit = lda(y~., data=train)
yhat.te = predict(fit,test)$class

cm = table(true = test$y, predict=yhat.te)
cm

# Test misclassification rate
misclass(cm)

# Test F-measure
fmeasure(cm)

# Adjust prior prob.
fit1 = lda(train$y, x = as.matrix(train[,-1]), prior=c(0.6,0.4)) # 0그룹은 0.6, 1그룹은 0.4로 prior probability를 지정해줌
yhat.te1 = predict(fit1 ,as.matrix(test[,-1]))$class

cm1 = table(true = test$y, predict=yhat.te1)
cm1

# Test misclassification rate
misclass(cm1)

# Test F-measure
fmeasure(cm1)

#################### Sampling methods ####################
#install.packages('caret')
library(caret)

# SVM model from the original imbalaned data
#install.packages('e1071')
library(e1071)
?tune
cv.fit = tune(svm, y~., data=train, kernel='radial', 
              ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100), # support vector의 margin을 결정하는 값들
                          gamma=c(0.01,0.1,0.5,1,2,3,4))) # radial basis에 대한 parameter
# binary classification에 대해 flexible한 모델을 사용할 수 있는 svm을 이용하여 우리가 가지고 있는 imbalanced data에 대해 어떻게 반응하는지 확인하고, imbalanced data를 해결하기 위한 sampling methods를 적용했을 때는 또 어떻게 반응하는지 알아본다. 
summary(cv.fit)

# SVM with the best parameters
best.fit = cv.fit$best.model

# Prediction for test data
yhat.te = predict(best.fit, test)
cm = table(true = test$y, predict=yhat.te)

misclass(cm)

fmeasure(cm)
cm
# False Negative가 상당히 많다.

# Upsampling ---------------------------------------------------
# Sampling with replacement from the small class.
set.seed(10)
uptrain = upSample(x = train[,-1], y=train$y, yname='y')

dim(uptrain)

table(uptrain$y)
# 원래는 10:500, 1:50이었는데 뻥튀기되었다. 

# Scatter plot for the upsampled training data
plot(cbind(uptrain$X1,uptrain$X2),
     col=(3-as.numeric(uptrain$y)),xlab='X1',ylab='X2')
# 똑같은 곳의 점들을 그냥 더 뽑은 것이기 때문에 검은색 점이 찐해진 것을 확인할 수 있다. 

# SVM for upsampled data.
set.seed(10)
cv.fit1 = tune(svm, y~., data=uptrain, kernel='radial', 
              ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),
                          gamma=c(0.01,0.1,0.5,1,2,3,4)))
summary(cv.fit1)

# SVM with the best parameters
best.fit1 = cv.fit1$best.model

# Prediction for test data
yhat.te1 = predict(best.fit1, test)
cm1 = table(true = test$y, predict=yhat.te1)

misclass(cm1)

fmeasure(cm1)
# 같은 곳에서만 계속 뽑히기 때문에 model performance를 높이는데 별로 도움이 되지 않는다. 

# Downsampling --------------------------------------------
# Randomly remove obs. in the large class.
set.seed(1)
dntrain = downSample(x = train[,-1], y=train$y, yname='y')

dim(dntrain)

table(dntrain$y)

# Scatter plot for the downsampled training data
plot(cbind(dntrain$X1,dntrain$X2),
     col=(3-as.numeric(dntrain$y)),xlab='X1',ylab='X2')
#sample size가 줄어들었기 때문에 model variance는 줄어들 수 밖에 없다. 

# SVM for downsampled data.
set.seed(10)
cv.fit2 = tune(svm, y~., data=dntrain, kernel='radial', 
               ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),
                           gamma=c(0.01,0.1,0.5,1,2,3,4)))
summary(cv.fit2)

# SVM with the best parameters
best.fit2 = cv.fit2$best.model

# Prediction for test data
yhat.te2 = predict(best.fit2, test)
cm2 = table(true = test$y, predict=yhat.te2)

misclass(cm2)

fmeasure(cm2)

# SMOTE ----------------------------------------------
# install.packages('DMwR')
# install.packages("/home/[파일명].tar.gz", repos = NULL, type="source")
# install.packages("https://cran.r-project.org/src/contrib/Archive/DMwR/0.4.1.tar.gz", repos = NULL, type="source")
library(DMwR)

set.seed(1)
smtrain = SMOTE(y~., data=train, perc.over=200, k = 5, perc.under=200)

dim(smtrain)
table(smtrain$y)


# Scatter plot for the training data from SMOTE
plot(cbind(smtrain$X1,smtrain$X2),
     col=(3-as.numeric(smtrain$y)),xlab='X1',ylab='X2')



# SVM for data from SMOTE.
set.seed(10)
cv.fit3 = tune(svm, y~., data=smtrain, kernel='radial', 
               ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),
                           gamma=c(0.01,0.1,0.5,1,2,3,4)))
summary(cv.fit3)

# SVM with the best parameters
best.fit3 = cv.fit3$best.model

# Prediction for test data
yhat.te3 = predict(best.fit3, test)
cm3 = table(true = test$y, predict=yhat.te3)

misclass(cm3)

fmeasure(cm3)
cm3
################# One-class learning ##################
# Support vector data description (SVDD) -----------------
# 이상치 탐지에 좋음
# Training data for the large class.
train.x0 = train[train$y == 0,-1] # large class에 대한 boundary를 구하는 것이기 때문에 large class에 속하는 데이터만 포함해서 training이 이루어져야 한다. 

result = NULL
#교수님이 편의상 만드신 코드
for (nu in c(0.001,0.01,0.1,0.3,0.5,0.7,0.9))
{
  for (gamma in c(0.01,0.1,0.5,1,2,3,5))
  {
    svddfit = svm(x=train.x0, type='one-classification', kernel='radial',
                  nu=nu, gamma=gamma)
    
    minor = predict(svddfit,test[,-1])                         #엄밀히 말해선 이 부분을 5-fold cv로 돌려야 한다. 
    # predict: TRUE: small class(outlier), FALSE: large class  #
    yhat.te = numeric(nrow(test))                              #
    yhat.te[minor==TRUE] = 1                                   #
    cm = table(true = test$y, predict=yhat.te)
    result = rbind(result, c(nu,gamma,fmeasure(cm)))
  }
}
# 아...! 그냥 저거 전체를 5번 돌리면 안되나?
# train.x0의 5등분해서 test[,-1]를 그 나머지 한 부분씩 돌려가는걸로 5번 돌린 다음에 밑에 nu 값, gamma 값, F-measure 값을 각각 평균내면 되잖아?

# Best result for F-measure.
names(result) <- c('nu','gamma','F-measure')
result[which.max(result[,3]),]
# nu 값, gamma 값, F-measure 값 순

############## Cost-sensitive Learning ################

# Class weighted SVM -----------------------------
# svm function using 'class.weight' option

wts = 500 / table(train$y)
#large class에 해당하면 1의 가중치를 주고, small class에 해당하면 10의 가중치를 줬다. 

set.seed(10)
cv.fit = tune(svm, y~., data=train, kernel='radial', 
              class.weights=wts,
              ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),
                           gamma=c(0.01,0.1,0.5,1,2,3,4)))
summary(cv.fit)

# SVM with the best parameters
best.fit = cv.fit$best.model

# Prediction for test data
yhat.te = predict(best.fit, test)
cm = table(true = test$y, predict=yhat.te)

misclass(cm)

fmeasure(cm)
cm
################# Ensemble-based methods ###################
#install.packages('ebmc')
# install.packages("https://cran.r-project.org/src/contrib/Archive/ebmc/ebmc_1.0.0.tar.gz", repos = NULL, type="source")
library(ebmc)
# 먼저 smote를 이용해서 데이터 수의 균형을 맞춰준 다음에 boosting이 이용된다. 
# SMOTE Boost -----------------------------------------
set.seed(10)

fit1 = sbo(y~., data=train, size=200, alg='cart', over=300)
# y should be encoded by (0,1); 0 large class, 1 small class
# size: # of boosting iterations
# alg: weak learner
# over: oversampling rate (multiple of 100 is only acceptible)

yhat.te = predict(fit1, test, type='class')
cm = table(true = test$y, predict=yhat.te)

misclass(cm)

fmeasure(cm)
cm

# SMOTE Bagging --------------------------------------
set.seed(10)

fit2 = sbag(y~., data=train, size=300, alg='cart')
# y should be encoded by (0,1); 0 large class, 1 small class

yhat.te = predict(fit2, test, type='class')
cm = table(true = test$y, predict=yhat.te)

misclass(cm)

fmeasure(cm)

# 우리가 사용한 데이터는 normal 분포를 따른 데이터에서 나온 것이고, 두 class의 사이즈는 다르지만 dispersion이 제법 비슷하여 nonlinear모델보다는 linear한 모델이 더 효과적인 양상을 보인다. 