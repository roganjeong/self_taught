############################################
# Statistical Modelling & Machine Learning #
#               R Example4                 #
############################################

options(warn = -1)  # Turn off warning message
library(corrplot)
######### Variable Importance: Regression problem ##########

# Building data
# 90 economic variables and sales variable (output)
dat = read.table('Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/12주차/building.csv', sep=',', header=T)

# Correlation coefficient ---------------------------
VI = cor(dat)[,'price']
SVI = sort(abs(VI), decreasing = T)[-1]# 자기 자신인 price는 제외하고
SVI

plot(1:length(SVI),SVI, type='b', ylab='Size of correlation',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SVI), labels=names(SVI), cex.axis=0.3,las=2)
#주의해야 할 점 : correlation coefficients를 독립적으로 나열한 것이기 때문에 marginal한 영향만 보여준다.
#그러므로 hidden interaction effect는 해당 그래프로 확인할 수 없고, 때문에 correlation coefficient가 낮은 변수에서 중요한 변수가 나올 수 있다. 

# Spearman rank correlation coefficient -------------
# outlier에 robust함 
SP = cor(dat, method='spearman')[,'price']
SPI = sort(abs(SP), decreasing = T)[-1] # 자기 자신인 price는 제외하고
SPI

#오름차순으로 나열 
plot(1:length(SPI),SPI, type='b', ylab='Size of correlation',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SPI), labels=names(SPI), cex.axis=0.3,las=2)

# Pseudo R^2 ----------------------------------------
p = 90
PR2 = numeric(p)
names(PR2) = colnames(dat[,-91])
for (j in 1:p)
{
  fit = loess(price ~ dat[,j], data=dat)  # Local linear regression : local kernel, weighted least sqaure
  yhat = predict(fit, dat[,j]) #training data에 대해서 yhat을 구함
  PR2[j] = 1-(sum((dat$price - yhat)^2)/sum((dat$price - mean(dat$price))^2))
}

SPR2 = sort(PR2, decreasing = T)
SPR2

plot(1:length(SPR2),SPR2, type='b', ylab='Pseudo R2',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SPR2), labels=names(SPR2), cex.axis=0.3,las=2)


# Maximal information coefficient (MIC) -------------
#신뢰할만하지 않음, accurate하고 엄밀한 모델을 만들려면 beaning을 잘해야 하는데 
#install.packages('minerva')
library(minerva)

MIC = mine(dat)
MIC = MIC$MIC[,'price']

SMIC = sort(MIC, decreasing = T)[-1]
SMIC

plot(1:length(SMIC),SMIC, type='b', ylab='MIC',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SMIC), labels=names(SMIC), cex.axis=0.3,las=2)


#####################여기까지가 변수가 정말 아주 많을 때 시도해볼만한 방법들

######### Variable Importance: Classification problem ##########

# Data
#install.packages('mlbench')
library(mlbench)
data(BreastCancer)
dat = BreastCancer[,-1]

# Relief algorithm ------------------------------
# hit와 miss를 찾는 것
# training obs 하나를 랜덤하게 뽑고, 그 obs에 한해 각 class마다 제일 가까운 거리에 있는 obs들과의 거리를 계산. 
#install.packages('CORElearn')
library(CORElearn)

# Relief algorithm
RE = attrEval(Class ~ ., data=dat, estimator='Relief',ReliefIterations=30)
# class이외의 모든 변수는 다 predictive variable로 사용
# 30번 해라
# 30번보다 훨씬 많이 하면 좀 일관되게 나오려나?
SRE = sort(RE, decreasing = T) #내림차순으로 정리
SRE

plot(1:length(SRE),SRE, type='b', ylab='Separability',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SRE), labels=names(SRE), cex.axis=0.8,las=2)
#할 때마다 달라지네...?
#training obs을 랜덤하게 뽑아서 그런가?
# 30번보다 훨씬 많이 하면 좀 일관되게 나오려나?

# ReliefF algorithm
# 여러개를 랜덤하게 뽑고 한꺼번에 계산하는 것
REF = attrEval(Class ~ ., data=dat, estimator='ReliefFequalK', ReliefIterations=30)

SREF = sort(REF, decreasing = T)
SREF

plot(1:length(SREF),SREF, type='b', ylab='Separability',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SREF), labels=names(SREF), cex.axis=0.8,las=2)


########## Variable Selection: Simulated Annealing ##########
#확률에 근거하여 optimized value를 찾는 과정, 초기값에 영향을 많이 받는다.
# 추천 x
#install.packages('mvtnorm')
library(mvtnorm)

# Data generation
set.seed(10)

n = 500
p = 20
S = matrix(0.3, nrow=p, ncol=p) # variance-covariance matrix 초기값
diag(S) = 1
X = rmvnorm(n, mean=rep(0,p), sigma=S) #random vector generation

XN = NULL
for (j in 1:p) XN = c(XN,paste('X',j,sep=''))
colnames(X) = XN # x변수에 이름 붙여주기

Y = 2 + 0.5*X[,1] - 0.3*X[,2] + 1.2*X[,3] + rnorm(n,sd=0.1) # y variable generation

# Simulated Annealing ---------------------------

#install.packages('caret')
library(caret)

ctrl = safsControl(functions=caretSA, method='cv', number=5) # 5 fold cv를 하겠다
obj = safs(x=X, y=Y, iters=20, safsControl=ctrl, method='lm') # linear regression을 사용하겠다
obj
# Y variable generation에 쓰인 x변수는 3개이지만, 해당 데이터에 simulated annealing을 사용하여 구한 변수 개수는 10개가 잡힘. 
obj$fit
obj$internel
#p가 많지 않다면 굳이 사용하지 말자!

#################### ISIS ####################

#install.packages('SIS')
library(SIS)

?SIS

# Data generation
set.seed(0)
n = 400; p = 50; rho = 0.5
corrmat = diag(rep(1-rho, p)) + matrix(rho, p, p)
corrmat[,4] = sqrt(rho)
corrmat[4, ] = sqrt(rho)
corrmat[4,4] = 1
corrmat[,5] = 0
corrmat[5, ] = 0
corrmat[5,5] = 1
cholmat = chol(corrmat)
x = matrix(rnorm(n*p, mean=0, sd=1), n, p)
x = x%*%cholmat

# Linear regression
set.seed(1)
b = c(4,4,4,-6*sqrt(2),4/3)
y=x[, 1:5]%*%b + rnorm(n) # 6부터 50번째 x변수는 유의하지 않은 y variable만들어주기


# ISIS with regularization
model11=SIS(x, y, family='gaussian', tune='bic') #p가 아주 크지 않기 때문에 bic가 적당하다
model11$ix

model12=SIS(x, y, family='gaussian', tune='bic', varISIS='aggr', seed=11)
model12$ix

# logistic regression
set.seed(2)
feta = x[, 1:5]%*%b
fprob = exp(feta)/(1+exp(feta))
y = rbinom(n, 1, fprob)

# ISIS with regularization
model21=SIS(x, y, family='binomial', tune='bic', penalty='SCAD', perm=T, q=0.9) # permutation방법을 이용하고, 90% quantile값을 threshold로 사용하여 screening해라
model21$ix

model22=SIS(x, y, family='binomial', tune='bic', varISIS='aggr', seed=21)
model22$ix


#########만약 p가 1000이라면

# Data generation
set.seed(0)
n = 400; p = 1000; rho = 0.5
corrmat = diag(rep(1-rho, p)) + matrix(rho, p, p)
corrmat[,4] = sqrt(rho)
corrmat[4, ] = sqrt(rho)
corrmat[4,4] = 1
corrmat[,5] = 0
corrmat[5, ] = 0
corrmat[5,5] = 1
cholmat = chol(corrmat)
x = matrix(rnorm(n*p, mean=0, sd=1), n, p)
x = x%*%cholmat

# Linear regression
set.seed(1)
b = c(4,4,4,-6*sqrt(2),4/3)
y=x[, 1:5]%*%b + rnorm(n) # 6부터 1000번째 x변수는 유의하지 않은 y variable만들어주기


# ISIS with regularization
model11=SIS(x, y, family='gaussian', tune='bic') 
model11$ix

model12=SIS(x, y, family='gaussian', tune='bic', varISIS='aggr', seed=11)
model12$ix

# logistic regression
set.seed(2)
feta = x[, 1:5]%*%b; fprob = exp(feta)/(1+exp(feta))
y = rbinom(n, 1, fprob)

# ISIS with regularization
model21=SIS(x, y, family='binomial', tune='bic', penalty='SCAD', perm=T, q=0.9) # permutation방법을 이용하고, 90% quantile값을 threshold로 사용하여 screening해라
model21$ix

model22=SIS(x, y, family='binomial', tune='bic', varISIS='aggr', seed=21)
model22$ix


# 일단 여기서 주어진 예제들로는 모델비교에 쓰인 모델 2개중 첫번째 모델의 결과가 더 좋았다. 
# 이렇게 p가 너무 크면 false positive한 변수가 포함될 수 있기 때문에 다양한 방법을 여러번 시도해봐야 한다. 
