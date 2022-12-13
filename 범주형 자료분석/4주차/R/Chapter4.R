#------------------------------------ ----------------------------------#
#                               Crab data                                        #
#------------------------------------ ----------------------------------#
setwd("~/Google Drive/My Drive/skku class files/2022년도 1학기/범주형 자료분석/4주차/R")
crabs <- read.table("crabs.dat",header=T)
crabs
attach(crabs)

y <- ifelse(satellites>0,1,0) # y=a binary indicator of satellites
weight <- crabs$weight/1000   # weight in kilograms rather than grams
color <- crabs$color-1              # now color takes values 1,2,3,4
color.f1 <- factor(color)
color.f <- relevel(color.f1,ref=4)
spine.f <- factor(spine)
dark <- as.numeric(color<4) # darkness

# normal model with identity link
fit0 <- glm(y~weight, family=gaussian(link=identity))  
summary(fit0)
#GOF가 좋게 나오고, p-value가 유의하게 나옴. 거느리는 수컷이 있으면 무게가 더 나간다...정도로 결론을 내는건가
#근데 얘는 가정이 틀린 모델이잖아. 

# logistic regression with weight
fit1 <- glm(y~weight, family=binomial(link=logit))
summary(fit1)
anova(fit1)

# logistic regression without covariate
fit2 <- glm(y~1, family=binomial(link=logit))
summary(fit2)
anova(fit2)

# logistic regression with weight and factor(color)
fit3 <- glm(y~weight+color.f, family=binomial(link=logit))
summary(fit3)
anova(fit3)

# logistic regression with weight and numeric(color)
fit4 <- glm(y~weight+color, family=binomial(link=logit))
summary(fit4)
anova(fit4)

# logistic regression with weight and dark
fit5 <- glm(y~weight+dark, family=binomial(link=logit))
summary(fit5)
anova(fit5)

# Probit Model
fit6 <- glm(y~weight,family=binomial(link=probit))
summary(fit6)
anova(fit6)

# LRT for models 1 & 2
anova(fit2,fit1)

# LRT for models 1 & 3
anova(fit1,fit3)

# LRT for models 3 & 5
anova(fit5,fit3)



#-----------------------------------------------------------------------#
#                               death data                                       #
#------------------------------------ ----------------------------------#

v <- c('b','b','w','w')
d <- c('b','w','b','w')
v.f1 <- factor(v)
v.f <- relevel(v.f1,ref="w")
d.f1 <- factor(d)
d.f <- relevel(d.f1,ref="w")

p <- c(4,0,11,53)
total <- c(143,16,48,467)
death <- data.frame(v.f,d.f,p,total)

fit.death1 <- glm(p/total ~ d.f+v.f, family=binomial(link=logit), weight=total, data=death)
summary(fit.death1)
confint(fit.death1) #  likelihood ratio 95% confidence interval

fit.death2 <- glm(p/total ~ v.f, family=binomial(link=logit), weight=total, data=death)
summary(fit.death2)
confint(fit.death2) #  likelihood ratio 95% confidence interval
anova(fit.death2, fit.death1)


#-----------------------------------------------------------------------#
#                      Probit model for  Bettle data                        #
#------------------------------------ ----------------------------------#
logdose <- c(1.691, 1.724, 1.755, 1.784, 1.811, 1.837, 1.861, 1.884)
number <- c(59,60,62,56,63,59,62,60)
died <- c(6,13,18,28,52,53,61,60)
lived <- number-died
beetle <- data.frame(logdose,number,died,lived)

fit.logit <- glm(died/number ~ logdose, family=binomial(link=logit),weight=number, data=beetle)
summary(fit.logit)
confint(fit.logit)

fit.probit <- glm(died/number ~ logdose, family=binomial(link=probit),weight=number, data=beetle)
summary(fit.probit)
confint(fit.probit)

fit.cloglog <- glm(died/number ~ logdose, family=binomial(link=cloglog),weight=number, data=beetle)
summary(fit.cloglog)
confint(fit.cloglog)

fit.loglog <- glm(lived/number ~ logdose, family=binomial(link=cloglog),weight=number, data=beetle)
summary(fit.loglog)
confint(fit.loglog)


