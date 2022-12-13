library(tidyverse) 
library(magrittr) 
library(astsa) 
library(bestNormalize)
library(rms)

or.train = read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/11주차/pm25_tr.csv") 
or.test = read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/11주차/pm25_te.csv")

or.test2 = or.test 
or.test2$pm25 = NA
data = rbind(or.train,or.test2)

par(mfrow=c(1,3)) 
plot(density(or.train$pm25)) 
plot(density(yeojohnson(or.train$pm25)$x.t)) 
plot(density(log(or.train$pm25)))

data %<>% mutate(logpm25 = log(pm25)) %>% select(-pm25)
attach(data)
data$month = as.factor(month) 
data$day = as.factor(day) 
data$hour = as.factor(hour) 
detach(data)

par(mfrow=c(2,3)) 
for(i in 1:ncol(data)){
  if(is.numeric(data[,i])) plot(density(data[,i],na.rm=T),main = colnames(data)[i]) }

data %<>% mutate(Iws_lag = lag(Iws), cbwd_lag = lag(cbwd),
                 ws = ifelse(cbwd_lag == cbwd, Iws - Iws_lag, Iws)
) %>% select(-Iws_lag,-cbwd_lag)

#standardize function
standardize = function(x){ 
  s.dev = sd(x,na.rm=T) 
  xbar = mean(x,na.rm=T) 
  return((x-xbar)/s.dev)
}
#standardization b/c too much scale diff
data2 = data %>% select(-logpm25,-day,-hour) %>% select_if(is.numeric) %>% mutate_all(standardize)

attach(data)
data3 = cbind(logpm25,data2,month,day,hour,cbwd)
train = data3[1:round(nrow(or.train)*0.8),]
val = data3[(round(nrow(or.train)*0.8)+1):nrow(or.train),] 
test = data3[(nrow(or.train)+1):nrow(data),]
detach(data)

fit3_1 = gam(logpm25 ~ s(DEWP,5) + s(PRES,5) + s(Iws,5) + s(ws,5),data=train)
par(mfrow=c(2,2)) 
plot(fit3_1)

fit3_2 = gam(logpm25 ~ DEWP + s(PRES,4) + Iws + s(ws,2),data=train)
fit3_3 = gam(logpm25 ~ DEWP + s(PRES,2) + Iws + s(ws,2),data=train)
anova(fit3_2,fit3_3)
'''
fit <- lm(logpm25 ~ . - day - hour, data = train) 
y_hat = predict(fit,val) 
mean((val$logpm25-y_hat)^2)
'''
data4 = data3 %>% mutate(lag_ws = lag(ws),lag_Iws=lag(Iws),lag_DEWP=lag(DEWP),lag_PRES=lag(PRES)) 
train2 = data4[1:round(nrow(or.train)*0.8),]
val2 = data4[(round(nrow(or.train)*0.8)+1):nrow(or.train),]
test2 = data4[(nrow(or.train)+1):nrow(data),]
train2 %>% ggplot(aes(x=PRES,y=logpm25,col=cut(DEWP,breaks=4)))+geom_point()
