 ############################################
# Statistical Modelling & Machine Learning #
#               R Example2                 #
############################################

#################
# Missing value #
#################

#밑에 패키지들을 사용하기 위해서는 missing values들이 NA로 있어야 한다. 

# mice package: Multivariate imputation by chained equation.
# install.packages('mice')
library(mice)

# install.packages('rms')
library(rms)

# install.packages('finalfit')
library(finalfit)

# Data
head(nhanes) 
#missing이 매우 많음  

nhanes$age = as.factor(nhanes$age)
nhanes$hyp = as.factor(nhanes$hyp)
# age와 hyp은 categorical 변수로 사용되어야 하기 때문에 factor로 변환해준다. 

# Description of data
describe(nhanes)

# Missing pattern
md.pattern(nhanes)
# 1 : 관측 
# 0 : 결측 

missing_pairs(nhanes,'chl',c('age','bmi','hyp') )
# chl : y variable
# age, bmi, hyp : x variable
# y variable의 missing pattern이 x variable들 간에 차이가 있는지 그래프로 보여줌
# 이상적으로는 complete set과 missing value가 포함된 set의 차이가 별로 없어야 한다. 

# Clustering for variables with missing values for the same obs.

# Missing pattern between variables 
# (rr: both observed, mm: both missing, 
#  mr: row variable is missing & column variable is observed)
# 동일한 observation에서 missing된 변수들을 clustering하는 것
md.pairs(nhanes)

missing.clus = naclus(nhanes,method='average') 
# method : clustering method, average linkage method
# hierarchical clustering
missing.clus
plot(missing.clus)

# Check missing pattern of Y variable.
fit = glm(is.na(chl) ~ age + bmi + hyp, data=nhanes, family=binomial )
summary(fit)
# 모든 variable들에 대해서 p-value가 유의하지 않으므로, missing values에 대한 뚜렷한 패턴이 없다. 

# MICE

?mice

# Missing values should be coded as NA.
# m: The number of imputed datasets
# method: Imputation methods for each column.
# predictorMatrix: A matrix containing 0/1 data specifying the set of predictors to be used for each target column.
# 기본으로는 어떤 특정 x variable을 impute하기 위해서 다른 모든 x variable들을 이용하는 것으로 되어 있다. predictormatrix로 특정 x variable들을 지정해줄 수 있다. 

set.seed(0)

?mice
imp = mice(nhanes, m=10, method=c('','pmm','logreg','pmm'), print=F)
# method=c('','pmm','logreg','pmm')
# age - '' : missing value가 없기 때문에 아무것도 지정하지 않음
# bmi - 'pmm' : predictive mean matching
# hyp - 'logreg' : hyp이 binary라서 logistic regression
# chl - 'pmm' : predictive mean matching
# 결과로는 10개의 imputed set들이 imp에 할당되었다. 

# predictors for imputation of each column
imp$predictorMatrix
# 1 : 사용
# 0 : 미사용
#결국 변수 자기 자신을 제외하곤 나머지 모두가 impute하는데 사용되었다. 
## 그런데 만약,
# If you don't want to use 'chl' variable to predict other variables,
pred = imp$predictorMatrix
pred[,'chl'] = 0
pred

imp1 = mice(nhanes, m=10, method=c('','pmm','logreg','pmm'), 
            predictorMatrix = pred, print=F)

imp1$predictorMatrix
##  ^이렇게 사용해도 좋다. 


# list the actual imputations for BMI
#만약 bmi변수에 imputed value들을 보고 싶다면,
imp$imp$bmi
# row : missing order
# column : imputed sets의 개수


# The first imputed dataset
complete(imp,1)

# The second imputed dataset
complete(imp,2)
# mice()함수에서 m=10으로 설정했기 때문에 10개의 imputed set이 있다. 

# Checking the convergence of MICE.
plot(imp, c('bmi','hyp','chl')) # c('bmi','hyp','chl') : convergence를 보고 싶은 변수들을 지정
# 평균과 분산이 서로 얽혀 있고, 패턴을 보이지 않기 때문에 converge했다고 볼 수 있다. 

# Compare the imputed data and observed data.

stripplot(imp, pch=20, cex=1.2)
# blue point: observed, red point: imputed

xyplot(imp,chl ~ bmi | .imp)
# The first plot: original complete set.

densityplot(imp, scales=list(relation='free'),layout=c(2,1))
# blue line: density for observed data, red line: density for imputed data

#위 3개의 그래프로 실제 데이터와 imputed 데이터가 유의한 차이는 없다는 것으로 추정.


# Prediction model with MICE
# Goal: predict chl based on age, hyp, bmi variables
# y variable에서 결측치가 발생했는데 y variable이 imputation에 사용될 경우 데이터가 왜곡될 수 있다. 
# 때문에 y variable에서 결측치가 발생하면 제거하는게 나은 선택이다.
set.seed(0)

imp = mice(nhanes, m=10, method=c('','pmm','logreg','pmm'), print=F)
# In mice, all variables with missing values should be imputed, 
# even if it is Y variable.

# To delete obs with missing Y value, imputed Y is replaced with NA.
md = dim(imp$imp$chl)
# chl에서 10개의 결측치가 있다. 
iy = as.data.frame(matrix(NA,md[1],md[2]))
colnames(iy) = colnames(imp$imp$chl)
rownames(iy) = rownames(imp$imp$chl)
# iy는 10x10인데 모두 NA로 채워져 있음

imp$imp$chl = iy # 이것 imputed set에 집어넣는다. 
complete(imp,1)
# NA로 채워진 것을 확인할 수 있다. 
# 모델은 complete set에 대해서만 돌아가기 때문에 NA가 포함된 observation은 알아서 제외된다. 

# Apply prediction model to each imputed dataset.
# E.g., prediction model => linear regression model

fit = with(imp, lm(chl ~ age + bmi + hyp))
# 10개의 imputed set이 있으므로 10개의 regression model들이 만들어진다. 

# Model averaging. 10개의 모델의 평균
summary(pool(fit))
# estimate : parameter에 대한 10개 모델의 평균
# standard error : intercept -> var(within), X's -> var(between)

# pool함수에는 pred함수를 사용할 수 없기 때문에 분석가가 estimated parameter로 manually 계산해줘야 한다.
 

# Checking imputation effect for significant X variables 
# E.g., to impute bmi variable, we used chl (Y) variable.

comp.dat = na.omit(nhanes)
fit1 = lm(chl ~ age + bmi + hyp, data=comp.dat)
summary(fit1)


# Predict test obs.
M = imp$m # the number of imputed sets
imp.dat = vector(mode='list',length=M)
for (m in 1:M) imp.dat[[m]] = complete(imp,m) # 10개의 imputed set을 imp.dat에 넣어 준다.

p.model = function(dat) lm(chl ~ age + bmi + hyp, data=dat)

fit.imp = lapply(imp.dat, p.model) # fit.imp에는 10개의 regression model이 저장되어 있음 

test.obs = data.frame(age=c('2','1'),bmi = c(23.3,21.5),hyp=c('1','1'))

yhat = lapply(fit.imp, predict, newdata=test.obs)
yhat = matrix(unlist(yhat),nrow(test.obs),M)

apply(yhat,1,mean) # 2개의 observation에 대한 10개의 모델의 각 예측치들의 평균 예측치

#######################
# Data transformation #
#######################

# install.packages('rms')
library(rms)
library(e1071)

getHdata(titanic3)
dat = titanic3[,c('survived','pclass','age','sex','sibsp','parch')]

describe(dat)

md.pattern(dat)
# age변수에만 263개의 missing values들이 있음

imp = mice(dat,m=1,method='pmm')
imp.dat = complete(imp)
#transformation에 대해 알아보는 것이기 때문에 imputation은 rough하게 해준다. 

par(mfrow=c(1,3))
for (j in c('age','sibsp','parch')) 
{
  hist(imp.dat[,j], main=j,xlab = skewness(imp.dat[,j]))  
}
# 3개의 변수 모두 굉장히 skewed된 상태라는 것을 알 수 있고, transformation이 필요하다. 

# Standardization or centering: Use 'scale()' function.
# Yeo-Johnson transformation
# install.packages('bestNormalize')
library(bestNormalize)

imp.dat1 = imp.dat
imp.dat1$sibsp = yeojohnson(imp.dat$sibsp)$x.t
imp.dat1$parch = yeojohnson(imp.dat$parch)$x.t

par(mfrow=c(1,2))
for (j in c('sibsp','parch')) 
{
  hist(imp.dat1[,j], main=j,xlab = skewness(imp.dat1[,j]))  
}

# Discretization of continuous variable

imp.dat2 = transform(imp.dat,
    agec = ifelse(age < 21, 'child','adult'),
    sibsp = ifelse(sibsp==0, 'no sibsp','sibsp'),
    parch = ifelse(parch==0, 'no parch','parch'))

head(imp.dat2)

