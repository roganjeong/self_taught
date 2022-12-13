library(dplyr)
library(magrittr)
library(ggplot2)

train <- read.csv('Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/11주차/pm25_tr.csv') 
test <- read.csv('Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/11주차/pm25_te.csv') 

data <- rbind(train, test)

library(data.table)
library(corrplot)
data %<>% mutate(Iws1 = shift(Iws,1,fill = 0),
                 cbwd1 = shift(as.character(cbwd),1,fill='cv'), dws = ifelse(cbwd == cbwd1, Iws-Iws1, Iws), HUM = (100 - 5*(TEMP-DEWP)))

train <- data %>% slice(2:1944)
test <- data %>% slice(1945:nrow(data))

par(mfrow=c(1,1))
plot(density(train$pm25))
plot(density(log(train$pm25)))

num_tr = train %>% select_if(is.numeric) %>%
  mutate(pm25 = log(pm25)) %>% dplyr::select(-month, -hour, -day, -Iws1)

corrplot::corrplot(cor(num_tr))


ggplot(train, aes(x=factor(month), y=log(pm25)))+
  geom_boxplot()
ggplot(train, aes(x=day, y=log(pm25)))+ geom_point()
ggplot(train, aes(x=hour, y=log(pm25)))+ geom_point()

# GAM
library(gam)
gam_fit = gam(log(pm25) ~ month + s(DEWP,5) + s(PRES,5) + s(Iws,5) + s(dws, 5) + s(HUM,5), data = train)
par(mfrow=c(2,3)) 
plot(gam_fit)
ggplot(train, aes(x=dws, y=log(pm25)))+geom_point()

gam_fit1 = gam(log(pm25) ~ month + DEWP + s(PRES,4) + s(Iws,5) + dws + s(HUM,5), data = train)
gam_fit2 = gam(log(pm25) ~ month + s(DEWP,3) + s(PRES,4) + s(Iws,5) + dws + s(HUM,5), data = train) 
anova(gam_fit1, gam_fit2) # DEWP

gam_fit3 = gam(log(pm25) ~ month + s(DEWP,3) + s(PRES,4) + Iws + dws + s(HUM,5), data = train)
gam_fit4 = gam(log(pm25) ~ month + s(DEWP,3) + s(PRES,4) + s(Iws,2) + dws + s(HUM,5), data = train) 
anova(gam_fit3, gam_fit4) # Iws

gam_fit5 = gam(log(pm25) ~ month + s(DEWP,3) + s(PRES,4) + s(Iws,2) + dws + s(HUM,3), data = train)
gam_fit6 = gam(log(pm25) ~ month + s(DEWP,3) + s(PRES,4) + s(Iws,2) + dws + s(HUM,5), data = train) 
anova(gam_fit5, gam_fit6) # HUM

lm_fit1 = lm(log(pm25)~ month + DEWP + I(DEWP^2) + I(DEWP^3) + PRES + I(PRES^2) + I(PRES^3) + I(PRES^4) + Iws + I(Iws^2) + dws + HUM + I(HUM^2) + I(HUM^3) + I(HUM^4) + I(HUM^5) + cbwd , data = train)
summary(lm_fit1)

data %<>% mutate(Iws1 = shift(Iws,1,fill = 0), DEWP1 = shift(DEWP,1,fill = 0), TEMP1 = shift(TEMP,1,fill = 0), PRES1 = shift(PRES,1,fill = 0),
                 dws = ifelse(cbwd == cbwd1, Iws-Iws1, Iws), HUM = (100 - 5*(TEMP-DEWP)))
train <- data %>% slice(2:1944)
test <- data %>% slice(1945:nrow(data))

pre_dat = train %>% dplyr::select(Iws1, DEWP1, TEMP1, PRES1, pm25) %>% mutate(pm25= log(pm25))
par(mfrow=c(1,1))
corrplot::corrplot(cor(pre_dat))
pairs(pre_dat)

ggplot(train) +
  geom_point(aes(x= dws * PRES, y= log(pm25)))
ggplot(train) +
  geom_point(aes(x= PRES * HUM, y= log(pm25)))

lm_fit2 = lm(log(pm25)~ month + DEWP + I(DEWP^2) + I(DEWP^3) + PRES + I(PRES^2) + I(PRES^3) + I(PRES^4) + Iws + I(Iws^2) + dws + HUM + I(HUM^2) + I(HUM^3) + I(HUM^4) + I(HUM^5) + cbwd + Iws1 + DEWP1 + PRES1 + dws*PRES + PRES * HUM, data = train)
summary(lm_fit2)

library(leaps)
fit.full = regsubsets(log(pm25)~ month + DEWP + I(DEWP^2) + I(DEWP^3) + PRES + I(PRES^2) + I(PRES^3) + I(PRES^4) + Iws + I(Iws^2) + dws + HUM + I(HUM^2) + I(HUM^3) + I(HUM^4) + I(HUM^5) + cbwd + Iws1 + DEWP1 + PRES1 + dws*PRES + PRES * HUM, data = train, nvmax=24, nbest=1)
bs = summary(fit.full)
which.max(bs$adjr2)
bs

lm_fit3 = lm(log(pm25)~ month + DEWP + I(DEWP^2) + PRES + I(PRES^2) + I(PRES^3) + I(PRES^4) + Iws + dws + HUM + I(HUM^2) + I(HUM^3) + I(HUM^4) + I(HUM^5) + cbwd + Iws1 + DEWP1 + PRES1 + dws*PRES + PRES * HUM, data = train)
summary(lm_fit3)

best_fit = lm(log(pm25)~ month + DEWP + I(DEWP^2) + PRES + I(PRES^2) + I(PRES^3) + I(PRES^4) + Iws + dws + HUM + I(HUM^2) + I(HUM^3) + I(HUM^4) + I(HUM^5) + cbwd + Iws1 + DEWP1 + PRES1 + dws*PRES + PRES * HUM, data = train)
test_mse = sum((test$pm25 - exp(predict(best_fit, test)))^2)/nrow(test) 
test_mse
