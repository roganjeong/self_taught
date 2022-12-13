#------------------------------------ ----------------------------------#
#                               Crab data                                        #
#------------------------------------ ----------------------------------#
 
setwd("~/Google Drive/My Drive/skku class files/2022년도 1학기/범주형 자료분석/4주차/R")
crabs <- read.table("crabs.dat",header=T)
attach(crabs)

y <- ifelse(satellites>0,1,0) # y=a binary indicator of satellites
weight <- crabs$weight/1000   # weight in kilograms rather than grams
width <- crabs$width              # width
color <- crabs$color-1              # now color takes values 1,2,3,4
color.f1 <- factor(color)
color.f <- relevel(color.f1,ref=4)
spine.f <- factor(spine)
dark <- as.numeric(color<4) # darkness

# logistic regression with color and width
fit1 <- glm(y~ color.f+width, family=binomial(link=logit))
summary(fit1)
anova(fit1)

# logistic regression with dark and width
fit2 <- glm(y~dark+width, family=binomial(link=logit))
summary(fit2)
anova(fit2)

# logistic regression with color, spline, and width
fit.full <- glm(y~weight+width+color.f+spine.f, family=binomial(link=logit))  # full model
fit.null <- glm(y~1, family=binomial(link=logit))  # null model

# forward selection
res.forward <- step(fit.null,scope=list(lower=fit.null,upper=fit.full),direction="forward")
summary(res.forward)

# backward selection
res.backward <- step(fit.full,scope=list(lower=fit.null,upper=fit.full),direction="backward")
summary(res.backward)

# both selection
res.both <- step(fit.null,scope=list(lower=fit.null,upper=fit.full),direction="both")
summary(res.both)

# stepwise backward selection using AIC
library(MASS)
stepAIC(fit.full)  



#-----------------------------------------------------------------------#
#                         ROC for crab data                                 #
#------------------------------------ ----------------------------------#
library(pROC)

crabs <- read.table("C:/이근백/수업/범주형자료분석/2021/SAS_R_code/R/crabs.dat",header=T)
crabs
attach(crabs)

y <- ifelse(satellites>0,1,0) # y=a binary indicator of satellites
weight <- crabs$weight/1000   # weight in kilograms rather than grams
width <- crabs$width              # width
color <- crabs$color-1              # now color takes values 1,2,3,4
color.f1 <- factor(color)
color.f <- relevel(color.f1,ref=4)
spine.f <- factor(spine)
dark <- as.numeric(color<4) # drakness

fit <- glm(y~ color.f+width, family=binomial(link=logit))
rocplot <- roc(y ~ fitted(fit))
plot.roc(rocplot, legacy.axes=TRUE) # Specificity on x axis if legacy.axes=T
auc(rocplot)  # area under ROC curve = condordance index

#-----------------------------------------------------------------------#
#                    Berkeley admission data                             #
#------------------------------------ ----------------------------------#

berkeley <- as.data.frame(UCBAdmissions) 
head(berkeley)
berkeley$Dept1 <- relevel(berkeley$Dept,ref="F")
berkeley$Gender1 <- relevel(berkeley$Gender,ref="Female")
berk.logit1 <- glm(Admit =="Admitted" ~Gender1+Dept1, data = berkeley, weights = Freq, family =binomial(link=logit)) 
summary(berk.logit1)

#cbind(rstandard(berk.logit1,type="pearson"),residuals(berk.logit1,type="pearson"),
#        residuals(berk.logit1,type="deviance"),rstandard(berk.logit1,type="deviance"))

berk.logit2 <- glm(Admit =="Admitted" ~ Dept1, data = berkeley, weights = Freq, family =binomial(link=logit)) 
summary(berk.logit2)


#-----------------------------------------------------------------------#
#                          Sparse data analysis                             #
#------------------------------------ ----------------------------------#

center <- c(rep(1,2),rep(2,2),rep(3,2),rep(4,2),rep(5,2))
treat <- rep(c(1:0),5)
success <- c(0,0,1,0,0,0,6,2,5,2)
n <- c(5,9,13,10,7,5,9,8,14,14)

treat.f <- factor(treat)
center1 <- factor(center)
center.f <- relevel(center1,ref=5)

sparse.fit <- glm(success/n ~ treat.f+center.f, family=binomial(link=logit))
summary(sparse.fit)



