
# Q1.



## 데이터 로드 및 데이터 구조 파악

library("tidyverse")
library("dplyr")

train <- read.csv("C:/Users/10sop/Desktop/대학 폴더/2021-2/통머실/HW2/train.csv")
test <- read.csv("C:/Users/10sop/Desktop/대학 폴더/2021-2/통머실/HW2/test.csv")

test %>% head
train %>% head

train %>% dim


#해당 train data는 1000 x 5 데이터프레임이다


train %>% str



#모두 수치형 변수임을 알 수 있다


train %>% is.na %>% apply(2, sum)
test %>% is.na %>% apply(2, sum)


#1. test 데이터엔 NA가 존재하지 않는다.

#2. train 데이터에선 Y 변수엔 missing value가 없으며, X1~X4는 각각 missing value가 35~40개 존재한다.

#3. 총 관측값 개수는 1000개이므로 missing value 개수가 많지 않다고 판단하여, missing value를 imputation해도 무리가 없을 것이라 판단했다.


## missing value imputation ( 이하 NA imputation )


library("mice")
library("rms")
library("finalfit")

md.pattern(train)

#NA들 간의 패턴을 보기 위해 md.pattern 사용하였다.

#변수별로 NA가 발생한 패턴을 보면, 두 변수에서 동시에 NA가 발생한 경우는 각각 1, 2, 3개씩 밖에 없고
#NA가 발생한 경우 중 대다수는 두 변수 이상에서 동시에 발생한 경우가 아니라 각 변수 혼자서만 발생하는 경우임을 확인할 수 있다.

md.pairs(train)


#좀더 자세히 살펴보기 위해 md.pairs로 변수간 NA가 발생하는 패턴이 존재하는지를 확인해주었다.

#결과로 나온 matrix들의 비대각 성분값들이 비슷하기 때문에 어떤 변수가 NA됨에 따라 다른 변수가 NA되거나 NA되지 않는 패턴은 존재하지 않는다고 판단했다.


missing_pairs(train, "Y", c("X1", "X2", "X3", "X4"))


#이번엔 missing이 발생한 그룹과 발생하지 않은 그룹이 변수 별 유의미한 패턴의 차이가 존재하는지를 확인하기 위해 missing_pairs 사용하였다

#각 변수 별 NA가 생긴 경우와 생기지 않은 경우의 boxplot을 살펴보았을 때 유의미한 차이가 없어보인다



missing_cluster <- naclus(train, method = "average")
missing_cluster %>% plot



#NA에 대해 cluster analysis를 해봤을 때도 역시 마찬가지로, 어떤 변수가 missing됨에 따라 다른 변수가 missing되거나 missing되지 않는 패턴이 존재하지 않는다고 판단하였다. 한 변수의 NA를 대체할 때 다른 cluster를 사용해서 NA를 대체할 것인데, 유의미하게 같은 cluster라고 생각되는 변수들이 없으므로, 앞으로 mice를 통해 한 변수의 결측치를 대체할 때 다른 모든 변수를 사용하여 대체할 예정이다.


set.seed(1234)
imp = mice(train[, c("Y", "X1","X2", "X3", "X4")], m = 10,
           method = c("","pmm", "pmm","pmm","pmm"), print = F)


#mice를 통해 결측치를 대체하였으며, method는 predictve mean matching(PMM) 방법을 사용하였다.

#imputation set은 10개를 만들었다.

#Y 변수는 NA가 존재하지 않으므로, method를 공백("")으로 두었다



plot(imp)



#mean, sd 그래프를 볼 때 converge 했다고 판단하였다.


stripplot(imp, pch = 20, cex = 1.2)


#plot을 볼 때 거의 비슷한 값들로 대체됐음을 알 수 있기 때문에 imputation이 제대로 되었다고 판단하였다.



densityplot(imp, scales = list(relation = "free"))

#실제 관측된 데이터와 결측치가 대체된 값의 densityplot을 그려봤을 때 분포가 굉장히 비슷하게 나옴을 알 수 있다

#이를 통해서도 결측치 대체가 잘 되었음을 알 수 있었다.



## 데이터 EDA

library("corrplot")
library("car")
library("ggplot2")

imp_train <- complete(imp)
imp_train


# skewness 확인
library("e1071")
par(mfrow = c(1,4))
for (j in c("X1", "X2", "X3", "X4")){
  hist(imp_train[,j], main = j, xlab = skewness(imp_train[,j]))
}


#skewness는 심하지 않다고 판단되어, skewness를 해결하기 위한 transformation은 따로 해주지 않았다.




## 예측 모델 적합 - linear model


fit = with(imp, lm(Y ~ X1 + X2 + X3 + X4))
fit %>% pool() %>% summary



#모든 변수가 전부 유의하다고는 나오지만 mice를 통해 X의 missing value를 대체하는 과정에서 Y의 정보를 썼기 때문일 수도 있다. 따라서 이에 대해 체크해줘야 한다.


not_na_data = na.omit(train)
fit2 <- lm(Y~., data = not_na_data)
fit2 %>% summary


#imputation data가 아닌, original data에서도 모든 변수가 유의하다고 나오므로 모든 변수가 유의하다고 판단할 수 있다. 따라서 Y를 예측하기위한 predictive model 적합할 때 모든 변수를 사용할 것이다.


library("MLmetrics")

M = imp$m
imp_train <- vector(mode = "list", length = M)
for (m in 1:M){
  imp_train[[m]] = complete(imp, m)
}

linear_model <- function(data) {
  lm(Y~., data = data)
}
fit_imp <- lapply(imp_train, linear_model)
yhat <- lapply(fit_imp, predict, newdata = test)
yhat <- matrix(unlist(yhat),
               nrow(test), M)
final_yhat <- yhat %>% apply(1, mean)
# final_yhat : imputation uncertainty를 감안하여 구한 predicted value

MSE(final_yhat, test[, "Y"])



#최종 모델의 MSE는 20.528로 확인되었다.







# Q2



## 데이터 로드 및 데이터 구조 파악

train <- read.csv("C:/Users/10sop/Desktop/대학 폴더/2021-2/통머실/HW2/pm25_tr.csv")
test <- read.csv("C:/Users/10sop/Desktop/대학 폴더/2021-2/통머실/HW2/pm25_te.csv")


test %>% is.na %>% apply(2, sum)
train %>% is.na %>% apply(2, sum)


#train과 test 모두 결측치가 존재하지 않음을 알 수 있다



train %>% head

train %>% str


train[,"cbwd"] <- train[, "cbwd"] %>% as.factor
test[,"cbwd"] <- test[,"cbwd"] %>% as.factor
train %>% str



#cbwd(combined wind direction)은 범주형 변수이므로 factor로 바꿔주었다.



train %>% distinct(year)


#year 변수의 값은 1개밖에 없으므로 예측 모델링에 도움이 되지 않는 변수이다.

#따라서 삭제해주었다.


train <- train[, -which(colnames(train) == "year")]
test <- test[, -which(colnames(test) == "year")]
train %>% colnames


#year 변수가 삭제된 모습을 볼 수 있다.



## 데이터 EDA 및 파생변수 생성


#시계열 데이터인 Y변수와 X변수들의 값의 흐름에 어떠한 패턴이 있는지를 비교해서 보기 위해 plot을 그려주었다.


library("gridExtra")

train2 <- train
train2[,"index"] <- 1:nrow(train)
DEWP_gg <- ggplot(train2,
                  aes(x = index))+
  geom_line(aes(y= pm25,
                color = "blue"))+
  geom_line(aes(y= DEWP*-10,
                color = "red"))+ # 시계열 자료의 흐름을 시각적으로 비교하며 보기 좋게 -10을 곱해서 그려줌
  theme_bw()

TEMP_gg <- ggplot(train2,
                  aes(x = index))+
  geom_line(aes(y= pm25,
                color = "blue"))+
  geom_line(aes(y= TEMP*30,
                color = "red"))+
  theme_bw()

PRES_gg <- ggplot(train2,
                  aes(x = index))+
  geom_line(aes(y= pm25*10, # 시계열 자료의 흐름을 시각적으로 비교하며 보기 좋게 10을 곱해서 그려줌
                color = "blue"))+
  geom_line(aes(y= PRES,
                color = "red")) +
  theme_bw()

Iws_gg <- ggplot(train2,
                 aes(x = index))+
  geom_line(aes(y= pm25,
                color = "blue"))+
  geom_line(aes(y= Iws,
                color = "red"))+
  theme_bw()

grid.arrange(DEWP_gg, TEMP_gg, PRES_gg, Iws_gg)


#변동이 심해서 각 X변수들이 Y변수와 어떠한 관련이 있는지 육안으로 확인하기 어렵기 때문에 뒤에서 좀더 살펴볼 예정이다.


#다음으로, Y가 시계열 데이터이므로 시간별, 일별, 월별로 어떠한 패턴이 있는지를 파악하고자 plot을 그렸다.


by_time <- train %>%
  group_by(hour) %>% 
  dplyr::summarise(avg = mean(pm25)) %>%
  ggplot(aes(x = factor(hour), y = avg, fill = as.factor(hour))) + 
  geom_bar(stat = 'identity') + 
  labs(x = "시간", y = "미세먼지") + 
  ggtitle("시간별 미세먼지") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        legend.position = "none") +
  theme_bw()
by_time


#시간별 그래프를 그려보았을 때, 미세먼지 농도는 시간별로 차이가 있는 듯 보이긴 하지만 확실하진 않은 패턴이다.

by_day <- train %>%
  group_by(day) %>% 
  dplyr::summarise(avg = mean(pm25)) %>%
  ggplot(aes(x = factor(day), y = avg, fill = as.factor(day))) + 
  geom_bar(stat = 'identity') + 
  labs(x = "일", y = "미세먼지") + 
  ggtitle("일별 미세먼지") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        legend.position = "none") +
  theme_bw()
by_day



#일별 미세먼지 그래프를 그려보았을때, 상승했다가 하락하는 패턴이 반복되는 것 같아서 좀더 살펴볼 필요가 있어보인다.


by_month <- train %>%
  group_by(month) %>% 
  dplyr::summarise(avg = mean(pm25)) %>%
  ggplot(aes(x = factor(month), y = avg, fill = as.factor(month))) + 
  geom_bar(stat = 'identity') + 
  labs(x = "월", y = "미세먼지") + 
  ggtitle("월별 미세먼지") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        legend.position = "none") +
  theme_bw()
by_month


#월별 미세먼지 그래프는 3월에 유독 많은 경향이 있어보이는 것 같기도 하고 아닌 것 같기도 하다.
#따라서 뒤에서 linear model을 통해 좀더 검증을 해볼것이다.



#변수의 정보를 보았을 때 cbwd는 바람의 방향, Iws는 바람의 속도인데, 직관적으로는, 해당 지역의 미세먼지 농도는 어느 방향으로 얼마나 세게 바람이 부는지에 큰 관련이 있을 것 같다. 따라서 바람의 방향과 바람의 속도를 서로다른 변수로 볼 것이 아니라, 두 변수의 정보를 합쳐서 어느 방향으로 얼마나 세게 바람이 부는지를 나타낼 수 있는 변수를 생성한다면 질좋은 변수를 만들수 있을 것이라 생각했다. 따라서 spread 함수를 통해 각 바람의 방향 별 바람의 세기를 나타내는 변수를 만들었다. (CV, NE, NW, SE가 이 변수이다.)

train <- train %>% spread(key = "cbwd", value = "Iws")
test <- test %>% spread(key = "cbwd", value = "Iws")
train %>% head


#이러한 전처리를 진행한 후 NA가 발생한걸 볼수 있는데, NA가 발생한 경우는 그 변수의 방향으로 바람이 불지 않았을 경우이므로, 0으로 대체하는 것이 합당하다. 따라서 0으로 대체해준다.



train[train %>% is.na] <- 0
test[test %>% is.na] <- 0


## 변수선택 및 차원축소


#문제에서 5시점 후의 Y값을 예측하기 위해 5시점 후의 X값들을 안다고 가정했으므로 X변수의 시점을 미뤄주지 않고 유의한 변수를 선택해준다.


train3 <- train

par(mfrow = c(2,2))
lm(pm25~., data = train3) %>% plot



#정규성 가정이 만족하지 않은 것 같으므로 y값에 log를 취해서 정상성을 확보해준다


lm(log(pm25)~., data = train3) %>% plot


#정규성 가정을 만족하게 됐다고 판단했다. 또한, residuals vs fitted plot에서 red line이 0 근처에 존재하는 걸 보아 linear model 적합도 잘 되었다고 판단하였다. 따라서 해당 linear model을 신뢰할 수 있다
# 오차의 독립성 가정은 애매한데, 아마도 시계열 구조를 가지고 있는 데이터이기 때문이 아닐까 생각한다. 그 점에 대해서는 뒷 부분 예측 모델링 파트에서 생각해줄 예정이다.

lm(log(pm25)~., data = train3) %>% summary



#모든 변수가 유의하다고 판단내릴 수 있다.

#현재 독립변수는 총 10개로, 위의 linear model을 통해 변수선택이 되지 않았으므로, 날짜변수를 제외한 변수들끼리 PCA를 통해 차원을 축소해줄 순 없는지 확인해보기로 하였다. PCA를 진행하기 전에, 상관관계를 먼저 확인해주었다.


par(mfrow = c(1,1))
train[, c("DEWP", "TEMP", "PRES", "cv", "NW", "SE")] %>% cor(method = 'pearson') 
train[, c("DEWP", "TEMP", "PRES", "cv","NW", "SE")] %>% 
  cor(method = "pearson") %>% 
  corrplot(method = 'color',
           type = 'lower',
           addCoef.col = "black",
           tl.pos = "d",
           tl.col = "black",
           tl.cex  = 0.5,
           number.digits = 5,
           number.cex = 1)



#위는 선형적인 상관관계(pearson 상관관계)를 본 것이다.

train[, c("DEWP", "TEMP", "PRES", "cv","NW", "SE")] %>% cor(method = 'spearman')
train[, c("DEWP", "TEMP", "PRES", "cv", "NW", "SE")] %>% 
  cor(method = "spearman") %>% 
  corrplot(method = 'color',
           type = 'lower',
           addCoef.col = "black",
           tl.pos = "d",
           tl.col = "black",
           tl.cex  = 0.5,
           number.digits = 5,
           number.cex = 1)



#위는 비선형적인 상관관계(spearman 상관관계)를 본 것이다.


#상관관계가 충분히 크다면 상관관계가 큰 연속형변수끼리 PCA가 잘될 가능성이 크다 TEMP와 PRES는 -.69 정도로 어느정도 강한 상관관계가 있다고 할 수 있지만 그 외 변수들은 큰 상관관계가 있다고 보기는 어렵다고 판단하였다. CV, NE, NW, SE는 다른 변수들과 선형적, 비선형적인 상관관계가 둘다 모두 크지 않아서 PCA, Principal curve, Kernel PCA가 잘 될거 같지 않다. 또한, CV, NE, NW, SE와 같은, 상관관계가 매우 낮은 변수들을 포함시켜 PCA를 하게 될 경우 오히려 데이터에 왜곡이 생길 수도 있겠다는 판단을 하여, 어느정도 높은 수치를 보이는 DEWP, TEMP, PRES의 변수만 PCA를 해주었다.


pr <- prcomp(train[, c("DEWP", "TEMP", "PRES")], center = TRUE, scale = TRUE)
pr %>% summary



#PC2에서 Cumulative proportion이 0.9로, 차원 축소가 잘 된 것 같다.


screeplot(pr, type = "l", main = "scree plot")


#screeplot을 그려보았을 때도 PC2에서 elbow point가 형성됨을 볼 수 있다. 따라서 PC1, 2를 새로운 변수로 채택할 증거가 충분하다고 판단했다. 또한, PCA를 통해서도 차원 축소가 잘 되므로 굳이 비선형적인 방법인 Principal Curve나 Kernel PCA는 사용해줄 필요가 없다고 판단했다.

PC12 <- pr$x[, 1:2]
final_train <- train
final_train[, c("PC1", "PC2")] <- PC12
final_train <- final_train[, -which(colnames(final_train) %in% c("DEWP", "TEMP", "PRES"))]
final_train %>% head


#train에서 PCA를 적용해주었다.

PC12_test <- predict(pr, newdata = test[, c("DEWP", "TEMP", "PRES")])[, c("PC1", "PC2")]
final_test <- test
final_test[, c("PC1", "PC2")] <- PC12_test
final_test <- final_test[, -which(colnames(final_test) %in% c("DEWP", "TEMP", "PRES"))]
final_test %>% head

#test에서도 train에서 적용한 PCA를 적용해준다.





## 예측 모델 적합 - linear model

# 선택 이유 : 위에서 linear model의 plot을 그려보았을때 residuals vs fitted plot에서 red line이 0 근처에 존재했었다. 따라서 선형모델로도 충분히 설명이 된다고 생각했기 때문에 굳이 비선형적인 모델이나 algorithmic modelling을 사용하지 않았다. 



#5시점 후의 Y값을 예측할 때 5 시점 후의 X값을 안다고 가정하므로 X변수의 시점을 미루지 않고 linear model 적합해준다.


library("astsa")

fit_lm <- lm(pm25~., 
             data = final_train)
residuals(fit_lm) %>% acf2



#ACF 가 지수함수 형태로 감소하는 모양, PACF가 4차 이후 0이 되어 절단된 모양을 지닌다. 따라서 AR(3)모형이 적절하다고 판단했다. 하지만 AR(3) 모형이라는 것은, 3시차 전까지의 y값을 변수로 넣는다고 해석할 수 있는데 우리의 목적은 5 시점 후의 Y를 예측하는 것이므로 4, 3, 2, 1시점 후의 Y값은 모른다고 생각해야한다. 따라서 사용할 수 없다고 판단하였다. 따라서 AR모형을 적용해주지 않고 linear model만을 통해 예측해주었다.


pred_y <- predict(fit_lm, final_test)
pred_y %>% head(10)
final_test$pm25 %>% head(10)



MSE(pred_y, final_test$pm25)


#선택한 예측모델의 MSE는 1626.757로 나타났다.
























