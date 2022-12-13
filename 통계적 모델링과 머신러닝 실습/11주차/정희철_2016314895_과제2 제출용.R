
#Question 2
training.2 <- read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/11주차/pm25_tr.csv", header = TRUE)
test.2 <- read.csv("Google Drive/My Drive/skku class files/2021년도 2학기/통계적 모델링과 머신러닝 실습/11주차/pm25_te.csv", header = TRUE)

library(data.table)
library(dplyr)
library(magrittr)
library(ggplot2)
library(corrplot)
library(gam)
library(astsa)

### Y variable
#일단 Y 변수의 분포를 알아보고 transformation이 필요한지 판단한다. 
plot(density(training.2$pm25)) # right-skewed
plot(density(log(training.2$pm25))) # log-transformation seems to be appprpriate

### time-related variables
# year, month, day, hour변수가 있는데 이 중 year변수는 모두 동일하게 출력이 되어있기 때문에 의미가 없다. 또한, month와 day 데이터 또한 만약 패턴이 존재한다면 hour변수의 시계열 분석을 통해 알아낼 수 있다고 생각하여 제외하였다. 
# 위 4개의 시간 관련 변수 중 hour만 남겨둔다.
training.2 <- training.2[,4:10]
test.2 <- test.2[,4:10]

### continuous variables
# 연속형 데이터로는 DEWP, TEMP, PRES, Iws가 있다. 먼저 4개 변수를 모두 활용한 모델을 간단히 만들어 Y값에 기여하지 못하는 변수를 제외한다. 
# X변수들의 대한 아무 정보도 없기 때문에 GAM을 사용하여 모델을 만든다. 

library(gam)
training.2[,8] <- log(training.2[,2])
test.2[,8] <- log(test.2[,2])
colnames(training.2) <- c('hour', 'pm25', 'DEWP', 'TEMP', 'PRES', 'cbwd', 'Iws', 'log.pm25')
colnames(test.2) <- c('hour', 'pm25', 'DEWP', 'TEMP', 'PRES', 'cbwd', 'Iws', 'log.pm25')
fit.gam <- gam(log.pm25 ~ s(DEWP,5) + s(TEMP,5) + s(PRES,5) + s(Iws,5), data = training.2)
summary(fit.gam)
# 4개의 변수 모두 유의한 p-value를 갖는다. Multicollinearity가 있는지 확인해보는 것이 필요하다.
cor(training.pm25[,c(3,4,5,7)])

cor(training.2[,c(2,4,5)])
cor(training.2[,c(2,4,5)],method='spearman')
cor(training.2[,c(2,4,5)],method='kendall')
# 그 어떤 correlation method로도 TEMP-pm25의 상관계수가 PRES-pm25의 상관계수보다 낮기 때문에 둘 중 PRES변수를 선택한다. 

fit.gam.2 <- gam(log.pm25 ~ s(DEWP,5) + s(PRES,5) + s(Iws,5), data = training.2)
par(mfrow=c(3,1))
plot(fit.gam.2)
par(mfrow=c(1,1))
# DEWP : 선형 또는 3차식 중 택1
# PRES : 2차식 또는 4차식 중 택1
# Iws : 2차식 또는 3차식 중 택1
# ANOVA test를 통해 어떤 모델이 더 적합할지 판단한다. 

dewp.gam1 <- gam(log.pm25 ~ DEWP + s(PRES,5) + s(Iws,5), data = training.2) #linear
dewp.gam2 <- gam(log.pm25 ~ s(DEWP,3) + s(PRES,5) + s(Iws,5), data = training.2) # cubic
anova(dewp.gam1, dewp.gam2)
# DEWP는 3차식이 적절하다.

pres.gam1 <- gam(log.pm25 ~ s(DEWP,3) + s(PRES,2) + s(Iws,5), data = training.2) #quadratic
pres.gam2 <- gam(log.pm25 ~ s(DEWP,3) + s(PRES,4) + s(Iws,5), data = training.2) #4th degree
anova(pres.gam1, pres.gam2)
# PRES는 4차식이 적절하다. 

iws.gam1 <- gam(log.pm25 ~ s(DEWP,3) + s(PRES,4) + s(Iws,2), data = training.2) #quadratic
iws.gam2 <- gam(log.pm25 ~ s(DEWP,3) + s(PRES,4) + s(Iws,3), data = training.2) #cubic
anova(iws.gam1, iws.gam2)
# Iws는 3차식이 적절하다. 

#지금까지의 model
# log.pm25 ~ DEWP + I(DEWP^2) + I(DEWP^3) + PRES + I(PRES^2) + I(PRES^3) + I(PRES^4) + Iws + I(Iws^2) + I(Iws^3) + cbwd

'''
# building a model
lmod <- lm(log.pm25 ~ DEWP + I(DEWP^2) + I(DEWP^3) + PRES + I(PRES^2) + I(PRES^3) + I(PRES^4) + Iws + I(Iws^2) + I(Iws^3) + cbwd, data = training.2)
summary(lmod)
'''
acf2(fit.gam$residuals)
#ACF는 점진적으로 감소하고 PACF는 1 이후 없어지는 것으로 보아 lag-1 time correlation이 존재한다. 때문에 lag-1 term을 만들어준다. 
data.with.lag = training.2 %>% mutate(Iws.lag1 = lag(Iws),DEWP.lag1 = lag(DEWP),PRES.lag1=lag(PRES))
data.with.lag.test = test.2 %>% mutate(Iws.lag1 = lag(Iws),DEWP.lag1 = lag(DEWP),PRES.lag1=lag(PRES))

# Interaction 찾기
without.inter.model1 <- lm(log.pm25 ~ DEWP + PRES + Iws + cbwd, data = training.2)
with.inter.model1 <- lm(log.pm25 ~ DEWP + PRES + Iws + cbwd + PRES*DEWP, data = training.2)
anova(without.inter.model1, with.inter.model1)

without.inter.model3 <- lm(log.pm25 ~ DEWP + PRES + Iws + cbwd, data = training.2)
with.inter.model3 <- lm(log.pm25 ~ DEWP + PRES + Iws + cbwd + cbwd*Iws, data = training.2)
anova(without.inter.model3, with.inter.model3)
# PRES*DEWP와 cbwd*Iws의 interaction term은 적절하다.

lmod <- lm(log.pm25 ~ . + I(DEWP^2) + I(DEWP^3) + I(PRES^2) + I(PRES^3) + I(PRES^4) + I(Iws^2) + I(Iws^3) + PRES*DEWP + cbwd*Iws, data = data.with.lag)
summary(lmod)
par(mfrow=c(1,1))
'''
plot(lmod$residuals)
qqnorm(lmod$residuals)
qqline(lmod$residuals)
'''
y_hat <- predict(lmod, data.with.lag.test)

mean(((data.with.lag.test$log.pm25)[2:120] - y_hat[2:120])^2)
mean(((data.with.lag.test$pm25)[2:120] - exp(y_hat[2:120]))^2)
# test.mse = 408.9258