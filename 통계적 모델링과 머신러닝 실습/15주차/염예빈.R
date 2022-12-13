## 2018312343 염예빈
## 통계적모델링과 머신러닝 과제 2

library(tidyverse)
library(mice)
library(rms)
library(finalfit)
library(ggplot2)
library(scoringutils)
library(data.table)
library(caret)
library(randomForest)
library(progress)
library(gam)
library(bestNormalize)
library(moments)
library(corrplot)


#######################
######### Q1 ##########
#######################
data1 = read.csv("C:/Users/User/Desktop/통계적모델링과머신러닝/과제2/train.csv")

## 1. EDA ##
data1 %>% head(10)
data1 %>% str() # 1000 4
describe(data1) ## 모두 수치형 데이터

#### Missing Pattern 확인
md.pattern(data1)

missing_pairs(data1, 'Y', c('X1', 'X2', 'X3'))

## 2. 전처리 ##
#### MICE 
set.seed(990706)
imp = mice(data1, m = 10 , method = c('pmm', 'pmm', 'pmm','pmm', ''))
pred = imp$predictorMatrix
pred[,'Y'] = 0
pred
imp1 = mice(data1, m = 10 , method = c('pmm', 'pmm', 'pmm','pmm', ''), predictorMatrix = pred)
plot(imp1, c('X1', 'X2', 'X3', 'X4'))
stripplot(imp1, pch = 20, cex=1.2)
densityplot(imp1, scales=list(relation = 'free'), layout = c(2,2))

## 3. 모델링 ##
set.seed(990706)

M = imp1$m
imp.dat = vector(mode='list', length = M)
for (m in 1:M) imp.dat[[m]] = complete(imp1, m)


test = read.csv("C:/Users/User/Desktop/통계적모델링과머신러닝/과제2/test.csv")
test.obs = test[,c(1,2,3,4)]

#### RandomForest
tune_rf <- expand.grid(mtry = 2:4, ntree = c(200, 300, 400, 500, 600))
tune_rf$MSE <- NA
tune_rf

#### 하이퍼파라미터 튜닝
set.seed(990706)
train_dat = vector(mode='list', length = M)
val_dat = vector(mode='list', length = M)
for (i in 1:M){
  train_index <- createDataPartition(imp.dat[[i]]$Y, p = 0.7)
  train_dat[[i]] <- imp.dat[[i]][train_index$Resample1,]
  val_dat[[i]] <- imp.dat[[i]][-train_index$Resample1,]
}

pb <- progress_bar$new(total = nrow(tune_rf))


val_dat[[2]] %>% dim()

for (i in 1:nrow(tune_rf)){
  m_mse = NULL
  for (j in 1:M){
    RF_model = randomForest(Y~X1+X2+X3+X4, train_dat[[j]], mtry = tune_rf[i,'mtry'], ntree = tune_rf[i, 'ntree'])
    RF_pred = predict(RF_model, newdata = val_dat[[j]][,-5])
    mse.j = mse(RF_pred, val_dat[[j]]$Y)
    m_mse = c(m_mse, mse.j)
  }
  tune_rf[i, 'MSE'] <- mean(m_mse)
  pb$tick()
}

#### 튜닝결과 확인
tune_rf
tune_rf[which(tune_rf$MSE == min(tune_rf$MSE)), ]


#### Residuals Plot
yhat_list = vector(mode='list', length = M)
for (j in 1:M){
  set.seed(990706)
  RF_model = randomForest(Y~X1+X2+X3+X4, imp.dat[[j]], mtry = 2, ntree = 400)
  yhat_list[[j]] = predict(RF_model, newdata = imp.dat[[j]][,-5])
}

yhat = matrix(unlist(yhat_list), nrow(dat1$Y), M)
yhat = apply(yhat, 1, mean)

yhat %>% length()
r = data1$Y - yhat
plot(yhat, r, ylim= c(-7, 10))
lines(c(420, 500), c(0,0),col = 'red')
mse(data1$Y, yhat)

## 4. 결과 ##
yhat_list = vector(mode='list', length = M)
for (j in 1:M){
  set.seed(990706)
  RF_model = randomForest(Y~X1+X2+X3+X4, imp.dat[[j]], mtry = 2, ntree = 400)
  yhat_list[[j]] = predict(RF_model, newdata = test[,-5])
}

yhat = matrix(unlist(yhat_list), nrow(test.obs), M)
yhat = apply(yhat, 1, mean)

yhat %>% length()
r = test$Y - yhat
plot(yhat, r, ylim=c(-15, 15))
lines(c(420, 500), c(0,0),col = 'red')
mse(test$Y, yhat) # 17.42184


#######################
######### Q2 ##########
#######################

data2 = read.csv("C:/Users/User/Desktop/통계적모델링과머신러닝/과제2/pm25_tr.csv")
test2 = read.csv("C:/Users/User/Desktop/통계적모델링과머신러닝/과제2/pm25_te.csv")

data2 %>% head()
data2 %>% dim()

## 1. EDA ##
data2$year %>% unique() # year변수가 1개이기떄문에 삭제
data2$month %>% unique()
data2$day %>% unique()
data2$hour %>% unique()

data2 <- data2 %>% dplyr::select(-year)

data2 %>% head()

#### 히스토그램
par(mfrow = c(2,2))
hist(data2$DEWP)
hist(data2$TEMP)
hist(data2$PRES)
hist(data2$Iws)
skewness(data2$Iws)

#### 상관관계 그래프
data2 %>% head()
par(mfrow = c(1,1))
cor1 <- cor(data2[,c(4, 5, 6, 7, 9)])
corrplot(cor1)

#### 시계열 그래프
par(mfrow = c(1,1))
plot(data2$pm25, type = 'line')


## 2. 전처리 ##
#### YeoJohnson Transformation
dat2 <- data2
dat2$Iws <- yeojohnson(data2$Iws)$x.t
hist(dat2$Iws)

skewness(data2$Iws)
skewness(dat2$Iws) ## 나아짐

te2 <- test2
te2$Iws <- yeojohnson(test2$Iws)$x.t
te2$cbwd <- as.factor(te2.cbwd)
te2 <- te2 %>% select(-year)
te2$cbwd <- as.factor(te2$cbwd)

#### PCA
pca.x = dat2 %>% select(DEWP, TEMP, PRES)
pca.m <- prcomp(pca.x, scale = TRUE)
summary(pca.m)

pca.tr <- predict(pca.m, newdata = pca.x)[,1]
pca.x.te <- te2 %>% select(DEWP, TEMP, PRES)
pca.te <- predict(pca.m, newdata = pca.x.te)[,1]

dat2$PC <- pca.tr
te2$PC <- pca.te

## 3. 모델링 ##
dat2 %>% head()
dat2.tr <- dat2[,c(3, 4, 8, 9, 10)]
model = gam(pm25 ~ s(hour, 5) + s(Iws, 5) + s(PC, 5) , data = dat2.tr)
summary(model)
par(mfrow = c(1,3))
plot(model) ## hour : 3차식

model1 = gam(pm25 ~ s(hour, 3) + Iws + PC , data = dat2.tr)
summary(model1)
plot(model1)

model.b = lm(pm25 ~ hour + Iws + PC , data = dat2.tr )
anova(model.b, model1) ## 유의함

## 모델링
fit3 = lm(pm25 ~ Iws + poly(hour, 3) + PC , data = dat2.tr)
fit3 %>% summary()
par(mfrow = c(2,2))
fit3 %>% plot()


#### Var function
X = cbind(dat2$Iws, dat2$hour, dat2$PC) ## X변수 지정
colnames(X) = c('Iws', 'hour', 'PC')
Y = dat2$pm25

# function
f = function(beta,X)
{
  X1 = X[,1]; X2 = X[,2]; X3 = X[,3]
  beta[1] + beta[2]*X1 + beta[3]*X2 + beta[4]*X2^2 + beta[5]*X2^3+ beta[6]*X3
}

# Objective function for mean function: Genearalized least square method.
obj.mean = function(beta,Y,X,S) t(Y-f(beta,X)) %*% solve(S) %*% (Y-f(beta,X))
# S: Covariance matrix

# Gradient vector of the objective function
gr.mean = function(beta,Y,X,S){
  sigma2 = diag(S) ##sigma^2 설정!!! ##
  X1 = X[,1]; X2 = X[,2]; X3 = X[,3]
  R = Y - f(beta,X)
  c(-2*sum(R/sigma2), -2*sum(R*X1/sigma2), -2*sum(R*X2/sigma2), -2*sum(R*X2^2/sigma2), -2*sum(R*X2^3/sigma2), -2*sum(R*X3/sigma2))
}

# Linear variance function: |r| = gam1 + gam2*Yhat. ## Variance Fucntion 지정
# For linear variance function, we can consider absolute residuals,
# instead of squared residuals.
# gam.hat = (Z^T W Z)^(-1) Z^T W |r|. ##WLSE

library(matrixcalc)
beta.new = fit3$coefficients   # initial parameter.
W = diag(rep(1,length(Y))) ##초기값 설정 ##SIGMA^(-1)
mdif = 100000
iter = 0
while(mdif > 0.001 & iter < 1000){
  Yhat = f(beta.new,X)
  r = Y - Yhat
  Z = cbind(1,Yhat) 
  gam.hat = solve(t(Z) %*% W %*% Z) %*% t(Z) %*% W %*% abs(r) ##WLSE
  sigma = Z %*% gam.hat ## Sigma
  S = diag(as.vector(sigma^2)) ## SIGMA
  
  ## Inverse matrix
  if (is.non.singular.matrix(S)) W = solve(S)
  else W = solve(S + 0.000001*diag(rep(1,nrow(S))))
  print('inverseMatrix')
  ml2 = optim(beta.new, obj.mean, gr=gr.mean, method='BFGS', Y=Y, X=X, S=S) ## S : SIGMA
  beta.old = beta.new
  beta.new = ml2$par ## Beta 업데이트
  mdif = max(abs(beta.new - beta.old)) 
  iter = iter + 1
  print(iter)
}

beta.new ##최종 Beta


Yhat = f(beta.new, X)
sigma = Z %*% gam.hat ## 최종 SIGMA
r = (Y - Yhat)/sigma
par(mfrow = c(1,1))

#### Residuals Plot
plot(Yhat,r, ylim = c(-2, 4))
lines(c(0,200), c(0,0),col='red') 

## 4. 결과 ##
te.x <- te2 %>% select(Iws, hour, PC)
Yhat = f(beta.new,te.x)
mse(Yhat, te2$pm25)
