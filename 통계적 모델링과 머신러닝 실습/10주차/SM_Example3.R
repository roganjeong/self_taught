############################################
# Statistical Modelling & Machine Learning #
#               R Example3                 #
############################################

options(warn = -1)  # Turn off warning message

# Data: Breast Cancer Wisconsin Data

dat = read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/10주차/wdbc.csv", header = F)

x.name = c("radius", "texture", "perimeter", "area", "smoothness", 
            "compactness", "concavity", "concave_points", "symmetry", 
            "fractal_dimension")

names(dat) = c("id", "diagnosis", paste0(x.name,"_mean"), 
               paste0(x.name,"_se"), paste0(x.name,"_worst"))

dat = dat[,-1] #id는 분석에 필요없으므로


head(dat) #diagnosis : 2개의 유방암 종류
          #나머지는 암종양에 관한 30개의 변수

# Principal Component Analysis (PCA) ----------------------------

pr = prcomp(dat[,2:31], center = TRUE, scale = TRUE) #diagnosis를 제외한 30개의 연속형 변수들의 principal components들을 뽑음
summary(pr)

# Scree plot
screeplot(pr, type = 'l', npcs = 15, main = 'Scree plot') #Gradually decreasing하는 패턴이 아니라 elbow point가 있어 보인다.(4 또는 7에서) -> x변수끼리 correlation이 있다.
# 시각화를 위하여 3번째 pc까지만 포함하기로 한다. (elbow point 직전)

#데이터의 구조 파악
# Visulalization: Scatter plot matrix

library(lattice)

pc.dat = data.frame(type = dat$diagnosis,pr$x[,1:3]) #diagnosis변수를 type이라는 이름으로 바꾸고, 1~3 pc들과 묶어 하나의 데이터프레임으로 만든다. 

pc.dat$type = as.factor(pc.dat$type) #factor로 다시 인식시켜주는 단계

splom(~pc.dat[,2:4], groups=type, data=pc.dat, panel=panel.superpose) #scatterplot matrix를 만드는 단계. 
# groups=type : type변수별로 구분을 해줌 (type은 2개로 이루어져 있음). 
# pc1과 pc2는 구별이 잘 되어 있다. pc1과 pc3는 pc1축에서는 구분이 되지만 pc3축에서는 구분이 되지 않는다. pc2와 pc3는 구분이 전체적으로 잘 안되긴 하나 pc2축에서 살짝 구분이 되어 보인다. 
## pc1과 pc2가 classification에 중요할 것으로 예상된다. 

#install.packages('car')
library(car)
scatterplotMatrix(~PC1+PC2+PC3|type, data=pc.dat) #각 그룹의 pdf를 pc1, pc2, pc3에 대해서 보여줌. 
# pc1축에서는 m그룹과 b그룹의 pdf가 굉장히 잘 구분되어 있지만, pc2와 pc3에서는 2개의 pdf가 상당 부분 겹쳐져 있다. (잘 구분이 안된다.)
# 각 그래프에서 나타나는 선들은 각 그룹의 fitted line.

# Application to logistic regression:
#install.packages('boot') # pc를 새로운 x변수로 놓고 modelling해보기 
library(boot)


dat$diagnosis = as.factor(dat$diagnosis)

# Comparison of CV statistics:
set.seed(1)
fit = glm(diagnosis~., data=dat, family=binomial) #original data를 사용해서 logistics model을 돌림
cv.glm(dat, fit, K=5)$delta # 5-fold cv statistics를 구함.첫번째 숫자는 순수한 cv statistics, 두번째는 adjusted cv statistics

fit1 = glm(type~., data=pc.dat, family=binomial) #같은 모델인데 데이터만 pc로 바꿈
cv.glm(pc.dat, fit1, K=5)$delta # 더 낮은 숫자를 가지기 때문에 더 좋은 모델

#이 데이터는 elbow point가 잘보이는 편이라 x변수들 간의 선형관계가 있다는 것이고, 그렇기 때문에 nonlinear한 dimension reduction을 사용할 필요가 없다. 

# Principal Curve ------------------------------------------------
#install.packages('princurve')
library(princurve)

 # Simple example
set.seed(1)
n=100
x1 = runif(n,-1,1)
x2 = x1^2 + rnorm(n, sd = 0.1)
z = cbind(x1,x2)

cor(x1,x2) #correlation은 선형관계를 나타내기 때문에 quadratic relation을 가진 x1과 x2의 관계를 설명하지 못하므로 coefficient가 낮게 나온다. 

fit = principal_curve(z) #principal curve를 구해주는 내장함수

plot(x1,x2)
lines(sort(x1),(sort(x1))^2,col='red',lty=2) #error term이 없이 완전한 비선형성을 보여줌
lines(fit,col='blue') #principal curve로 그린 line
whiskers(z, fit$s) # 데이터들이 principal curve에 projection된 모습을 보여줌. 각 projection의 길이(arc length)가 lambda값이 된다. 


# WDBC data application:

fit = principal_curve(as.matrix(dat[,2:31])) #principal curve는 matrix형태의 데이터만 받기 때문에 변환해준다. 

# Density of two groups in principal curve and PC1
par(mfrow=c(1,2)) # principal curve로 구한 선(축)과 pc1으로 구한 축의 performance 비교
# density()는 데이터에 근거한 pdf를 찾아준다. 
plot(density(fit$lambda[dat$diagnosis == 'B']),col='red',
     xlim=c(500,5500),main='Principal Curve')
lines(density(fit$lambda[dat$diagnosis == 'M']),col='blue')

plot(density(pc.dat[dat$diagnosis == 'B',2]),col='red',
     xlim=c(-15,7),main='PC1')
lines(density(pc.dat[dat$diagnosis == 'M',2]),col='blue')

#위에 이미 우리는 해당 데이터는 x변수들끼리 선형관계를 가지고 있기 때문에 pca로 충분할 것이라는 결론을 내렸고, 실제로 그래프도 pc1은 m그룹과 b그룹의 pdf가 명확한 봉우리를 가지지만, principal curve는 그렇지 못한 모습을 보이는 것으로 이를 확인할 수 있다. 

# Density for principal curve and PC1
# 두 그룹을 분류하지 않고 합쳐서 그려보기 
par(mfrow=c(1,2))
plot(density(fit$lambda),col='red',main='Principal Curve')
plot(density(pc.dat[,2]),col='red',main='PC1')
#이거 역시 pc1은 2개의 그룹이 명확하게 보인다는 것을 확인할 수 있다. principal curve는 2개의 그룹이 아니라 skewed된 pdf로 볼 수 도 있기 때문에.

#principal curve를 통해서 구한 lambda값을 새로운 x변수로 활용
dat1 = cbind(dat, pcurve=fit$lambda)
dat1$diagnosis = as.factor(dat1$diagnosis)

fit2 = glm(diagnosis~pcurve, data=dat1, family=binomial)
cv.glm(dat1, fit2, K=5)$delta # 위에 line 67에 principal component로 구한 것보다 안좋다. 

# Projection onto Principal curve
#새로운 데이터를 principal curve에 projection해서 lambda값을 구하는 단계
new.obs = as.matrix(dat[31:40,2:31])
project_to_curve(new.obs,fit$s)$lambda  # fit$s : smooth curve object
# arc-length along the curve.

# Kernel PCA ---------------------------------------------------------
#install.packages('kernlab')
library(kernlab)

x = dat[,2:31]
fit = kpca(~., data=x, kernel='rbfdot', kpar=list(sigma=3), features=2) # rbfdot : radial basis function, sigma : radial basis의 tuning parameter, features : principal component의 갯수
# feature: # of PC's

# Kernel PC's
pc = pcv(fit)

B = pc[dat$diagnosis=='B',]
M = pc[dat$diagnosis=='M',]

par(mfrow=c(1,1))
plot(B, col='red', xlab='KPC1',ylab='KPC2')
points(M, col='blue')

# New observations
predict(fit, new.obs)


# Non-negative matrix factorization ------------------------------------

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Biobase")

install.packages('NMF')
library(NMF)

?esGolub

data(esGolub)

dim(esGolub) #변수 수가 데이터보다 훨씬 많아서 dimension reduction이 필수이다. 

res = nmf(esGolub, rank = 3, seed=123456) # 'rank = 3' decomposition되었을 때 W의 column갯수, H의 row갯수

#W와 H를 따로 뽑고 싶을 때
# W matrix
W = basis(res)
dim(W)

# H matrix
H = coef(res)
dim(H)

#rank를 몇을 써야하는지 알고 싶을 때 (2~6)
if(requireNamespace("Biobase", quietly=TRUE))
{
  estim.r = nmf(esGolub, 2:6, nrun=10, seed=123456)
  plot(estim.r)
}
#cophenetic그래프에서 어디서 급격한 감소가 있는지 체크 (5)
# ==> rank 5가 적절하다.


res = nmf(esGolub, rank = 5, seed=123456)

# Visualization
dev.new()
basismap(res, subsetRow=TRUE)
dev.off()

dev.new()
coefmap(res)
dev.off()

# Independent component analysis -----------------------------------

install.packages('fastICA')
library(fastICA)


# Ex1:

S = matrix(runif(10000), 5000, 2) #source matrix를 uniform random number로 설정
A = matrix(c(1, 1, -1, 3), 2, 2, byrow = TRUE) # mixing matrix
X = S %*% A #얘를 우리가 가지고 있는 데이터 원본이라고 가정하자. 이제 얘를 S와 A로 나누는 과정이 Independent component analysis
# 2개의 source로 뽑아내겠다. alg.typ : computing하는 알고리즘 타입, fun : 어떤 function을 사용하여 independent component를 구하겠는가
# maxit : maximum iteration, tol : tolerance
a = fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "C", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)
par(mfrow = c(1, 3))
plot(a$X, main = "Pre-processed data") # 정규화된 원본 데이터 
plot(a$X %*% a$K, main = "PCA components")
plot(a$S, main = "ICA components")
par(mfrow = c(1, 1))

# Ex2:

S = cbind(sin((1:1000)/20), rep((((1:200)-100)/100), 5))
A = matrix(c(0.291, 0.6557, -0.5439, 0.5572), 2, 2)
X = S %*% A
a = fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "R", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)

par(mfcol = c(2, 3))
plot(1:1000, S[,1], type = "l", main = "Original Signals",
     xlab = "", ylab = "")
plot(1:1000, S[,2], type = "l", xlab = "", ylab = "")
plot(1:1000, X[,1], type = "l", main = "Mixed Signals",
     xlab = "", ylab = "")
plot(1:1000, X[,2], type = "l", xlab = "", ylab = "")
plot(1:1000, a$S[,1], type = "l", main = "ICA source estimates",
     xlab = "", ylab = "")
plot(1:1000, a$S[, 2], type = "l", xlab = "", ylab = "")


