#############
# trial 1 : if unknowns were not missing values
training.data <- read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/13주차/train-1.csv", header = TRUE)
test.data <- read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/13주차/test-1.csv", header = TRUE)
# F-measure value = (2*tpr*ppv)/(tpr+ppv)
library(mice)
library(rms)
library(finalfit)

summary(training.data)
head(training.data) 

# The missing values need to be checked, if there is any.
md.pattern(training.data)
# It is confirmed that there is no missing data. 
# but "unknown" data are placed across the data.
# We will build a model with the unknowns as one of categories and another model with the unknowns as missing values.


################################## model with unknowns as a category
# setting the data
training.data[,1] <- as.numeric(training.data[,1])
training.data[,2] <- as.factor(training.data[,2])
training.data[,3] <- as.factor(training.data[,3])
training.data[,4] <- as.factor(training.data[,4])
training.data[,5] <- as.factor(training.data[,5])
training.data[,6] <- as.numeric(training.data[,6])
training.data[,7] <- as.factor(training.data[,7])
training.data[,8] <- as.factor(training.data[,8])
training.data[,9] <- as.factor(training.data[,9])
training.data[,10] <- as.factor(training.data[,10])
training.data[,11] <- as.factor(training.data[,11])
training.data[,12] <- as.numeric(training.data[,12])
training.data[,13] <- as.integer(training.data[,13])
training.data[,14] <- as.integer(training.data[,14])
training.data[,15] <- as.integer(training.data[,15])
training.data[,16] <- as.factor(training.data[,16])
training.data[,17] <- as.factor(training.data[,17])

test.data[,1] <- as.numeric(test.data[,1])
test.data[,2] <- as.factor(test.data[,2])
test.data[,3] <- as.factor(test.data[,3])
test.data[,4] <- as.factor(test.data[,4])
test.data[,5] <- as.factor(test.data[,5])
test.data[,6] <- as.numeric(test.data[,6])
test.data[,7] <- as.factor(test.data[,7])
test.data[,8] <- as.factor(test.data[,8])
test.data[,9] <- as.factor(test.data[,9])
test.data[,10] <- as.factor(test.data[,10])
test.data[,11] <- as.factor(test.data[,11])
test.data[,12] <- as.numeric(test.data[,12])
test.data[,13] <- as.integer(test.data[,13])
test.data[,14] <- as.integer(test.data[,14])
test.data[,15] <- as.integer(test.data[,15])
test.data[,16] <- as.factor(test.data[,16])
test.data[,17] <- as.factor(test.data[,17])
head(training.data)

# Y variable is categorical, and x variables have both continuous and categorical variables. 
# Relief and ReliefF Algorithm can be used

library(CORElearn)

# Relief algorithm
RE = attrEval(Y ~ ., data=training.data, estimator='Relief',ReliefIterations=30)
SRE = sort(RE, decreasing = T)
SRE

plot(1:length(SRE),SRE, type='b', ylab='Separability',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SRE), labels=names(SRE), cex.axis=0.8,las=2)
# X1, X11 and X12 can be significant

# ReliefF algorithm
REF = attrEval(Y ~ ., data=training.data, estimator='ReliefFequalK', ReliefIterations=30)
SREF = sort(REF, decreasing = T)
SREF

plot(1:length(SREF),SREF, type='b', ylab='Purchase',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SREF), labels=names(SREF), cex.axis=0.8,las=2)
# X1, X11, and X12 can be significant

fit = glm(Y ~ X1 + X11 + X12, data=training.data, family=binomial )
summary(fit)

new.obs <- test.data[,c('X1','X11','X12')]
predicted <- predict(fit, new.obs)
predicted[predicted>=0] <- 'yes'
predicted[predicted<0] <- 'no'
actual <- test.data[,'Y']

# F-measure
cm  = table(true = actual, predict=predicted)
cm
fmeasure = function(cm)
{
  TPR = cm[2,2]/sum(cm[2,])
  PPV = cm[2,2]/sum(cm[,2])
  return((2*TPR*PPV)/(TPR + PPV)) 
}
fmeasure(cm)

# Can we improve the model by adjusting the cut-off value?




############################# model with the unknowns as missing values

training.data <- read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/13주차/train-1.csv", header = TRUE)
test.data <- read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/13주차/test-1.csv", header = TRUE)
training.data[,1] <- as.numeric(training.data[,1])
training.data[,2] <- as.factor(training.data[,2])
training.data[,3] <- as.factor(training.data[,3])
training.data[,4] <- as.factor(training.data[,4])
training.data[,5] <- as.factor(training.data[,5])
training.data[,6] <- as.numeric(training.data[,6])
training.data[,7] <- as.factor(training.data[,7])
training.data[,8] <- as.factor(training.data[,8])
training.data[,9] <- as.factor(training.data[,9])
training.data[,10] <- as.factor(training.data[,10])
training.data[,11] <- as.factor(training.data[,11])
training.data[,12] <- as.numeric(training.data[,12])
training.data[,13] <- as.integer(training.data[,13])
training.data[,14] <- as.integer(training.data[,14])
training.data[,15] <- as.integer(training.data[,15])
training.data[,16] <- as.factor(training.data[,16])
training.data[,17] <- as.factor(training.data[,17])

head(training.data)
# Making unknown data as missing values
training.data[training.data=='unknown'] <- NA
head(training.data)

# checking the possible patterns of missing values 
md.pattern(training.data) # missing values are placed in X2, X4, X9, X16
missing_pairs(training.data,'Y',c('X2','X4','X9','X16') )
md.pairs(training.data)

# As for the variable X16, almost 4000 out of 5000 observations are missing so it is wise to exclude X16.
# Additionally, 20% of X9 observations is missing so X9 should also be excluded.
training.data <- training.data[,c(-9,-16)]
md.pairs(training.data)
missing_pairs(training.data,'Y',c('X2','X4') )
# There seems to have no clear pattern in missing values within other variables. X2, which is client's job, is an unordered categorical variable so polyreg can be used for imputation. X4, which is an educational level, is an ordered categorical variable so polr can be used for imputation.
set.seed(0)

imp = mice(training.data, m=10, method=c('','polyreg','','polr','','','','','','','','','','',''), print=F)
pred = imp$predictorMatrix
pred[,'Y'] = 0
imp1 = mice(training.data, m=10, method=c('','polyreg','','polr','','','','','','','','','','',''), predictorMatrix = pred, print=F)
imp1$predictorMatrix
colnames(training.data)
stripplot(imp1, pch=20, cex=1.2)
# 딱히 패턴이 보이지 않는다.
training.set <- complete(imp)

# Filtering the variables
library(CORElearn)

# Relief algorithm
RE = attrEval(Y ~ ., data=training.set, estimator='Relief',ReliefIterations=30)
SRE = sort(RE, decreasing = T)
SRE

plot(1:length(SRE),SRE, type='b', ylab='Separability',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SRE), labels=names(SRE), cex.axis=0.8,las=2)
# X11, X10, and X12 can be significant

# ReliefF algorithm
REF = attrEval(Y ~ ., data=training.set, estimator='ReliefFequalK', ReliefIterations=30)
SREF = sort(REF, decreasing = T)
SREF

plot(1:length(SREF),SREF, type='b', ylab='Purchase',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SREF), labels=names(SREF), cex.axis=0.8,las=2)
# X12, X11, and X10 can be significant

# Building a model 
fit = glm(Y ~ X10 + X11 + X12, data=training.set, family=binomial )
summary(fit)

new.obs <- test.data[,c('X10','X11','X12')]
predicted <- predict(fit, new.obs)
predicted[predicted>=0] <- 'yes'
predicted[predicted<0] <- 'no'
actual <- test.data[,'Y']

# F-measure
cm  = table(true = actual, predict=predicted)
cm
misclass = function(cm) 1 - sum(diag(cm))/sum(cm)
fmeasure = function(cm)
{
  TPR = cm[2,2]/sum(cm[2,])
  PPV = cm[2,2]/sum(cm[,2])
  return((2*TPR*PPV)/(TPR + PPV)) 
}
misclass(cm)
fmeasure(cm)

# Can we improve the model by adjusting the cut-off value?

library(pROC)
phat.tr = predict(fit, training.set, type='response')
lr.roc = roc(training.set$Y ~ phat.tr)
plot(lr.roc)
auc(lr.roc)
th = coords(lr.roc,x='best', best.method = 'closest.topleft')
th # new threshold = 0.2928077

predicted <- predict(fit, new.obs)
predicted[predicted>=0.2928077] <- 'yes'
predicted[predicted<0.2928077] <- 'no'
actual <- test.data[,'Y']

# F-measure
cm  = table(true = actual, predict=predicted)
cm
fmeasure = function(cm)
{
  TPR = cm[2,2]/sum(cm[2,])
  PPV = cm[2,2]/sum(cm[,2])
  return((2*TPR*PPV)/(TPR + PPV)) 
}
misclass(cm)
fmeasure(cm)
# No, the overall performance degraded with the adjusted cut-off value.
