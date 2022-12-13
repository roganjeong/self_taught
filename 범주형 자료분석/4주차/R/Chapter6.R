#---------------------------------------------------------------------#
#                               Job satisfaction                               #
#              using  Baseline Category Logit Model                  #
#---------------------------------------------------------------------#

# Baseline Category Logit Model for nominal data
# using VGAM library

job.sat <- factor(c("VD","LD","MS","VS"), levels=c("VD","LD","MS","VS"))
income <- c(3,10,20,30)
table.job <- expand.grid(income=income, job.sat=job.sat) # 각 만족도 범주마다 3, 10, 20, 30에 짝지어주는 것, 16행이 만들어짐.
temp <- c(2,2,0,0,4,6,1,3,13,22,15,13,3,4,8,8) # 각 행을 늘려주기 위한 벡터생성, 자리에 지정된 숫자만큼 늘려줌. 
table.job <- structure(.Data=table.job[rep(1:nrow(table.job),temp),], row.names=1:104 )

library(VGAM) # package for multivariate GLMs
fit1 <- vglm(job.sat ~ income, family=multinomial, data=table.job)
coef(fit1, matrix=TRUE)
summary(fit1)

fit0 <- vglm(job.sat ~ 1, family=multinomial, data=table.job)
coef(fit0, matrix=TRUE)
summary(fit0)

lrtest(fit1,fit0)  # LRT test

fitted(fit1)  # estimated response probabilities for outcome categories


# Baseline Category Logit Model for nominal data
# using nnet library

job.sat1 <- factor(c("VD","LD","MS","VS"), levels=c("VD","LD","MS","VS"))
job.sat <- relevel(job.sat1, ref="VS")
income <- c(3,10,20,30)
table.job <- expand.grid(income=income, job.sat=job.sat)
temp <- c(2,2,0,0,4,6,1,3,13,22,15,13,3,4,8,8)
table.job <- structure(.Data=table.job[rep(1:nrow(table.job),temp),], row.names=1:104 )

library(nnet)
options(contrasts=c("contr.treatment", "contr.poly"))

# Model 1
fit1.nnet <- multinom(job.sat ~ income, data=table.job)
summary(fit1.nnet)
deviance(fit1.nnet)
confint(fit1.nnet, method="profile")

# Model 0
fit0.nnet <- multinom(job.sat ~ 1, data=table.job)
summary(fit0.nnet)
deviance(fit0.nnet)

anova(fit1.nnet, fit0.nnet)  # LRT test for Model 1 vs Model0

fitted(fit1.nnet)  # estimated response probabilities for outcome categories


# Baseline Category Logit Model for nominal data
# using grouped data

VD <- c(2,2,0,0)
LD <- c(4,6,1,3)
MS <- c(13,22,15,13)
VS <- c(3,4,8,8)
no <- apply(cbind(VD,LD,MS,VS), 1, sum)
income <- c(3,10,20,30)

job.sat.data <- data.frame(income,VD,LD,MS,VS,no)

library(VGAM)
# Results are same to those in fit1
fit.grp <- vglm(cbind(VD,LD,MS,VS) ~ income, family=multinomial, data=job.sat.data)
summary(fit.grp)


#------------------------------------------------------------------#
#           Cumulative Logit Model for nominal data                #
#           using job satisfaction data                            #
#------------------------------------------------------------------#

VD <- c(2,2,0,0)
LD <- c(4,6,1,3)
MS <- c(13,22,15,13)
VS <- c(3,4,8,8)
no <- apply(cbind(VD,LD,MS,VS), 1, sum)
#income <- c(3,10,20,30)
job.sat.data <- data.frame(income,VD,LD,MS,VS)

library(VGAM)
fit2 <- vglm(cbind(VD,LD,MS,VS) ~ income, family=cumulative(parallel=TRUE), data=job.sat.data)
summary(fit2)
deviance(fit2)


#------------------------------------------------------------------#
#           Cumulative Logit Model for nominal data           #
#           using GSS politics data                                     #
#------------------------------------------------------------------#

gender0 <- factor(c(rep("Female",2),rep("Male",2)))
gender <- relevel(gender0,ref="Male")
party0 <- factor(rep(c("Demo","Rep"),2))
party <- relevel(party0,ref="Rep")

VL <- c(44,18,36,12)
SL <- c(47,28,34,18)
M <- c(118,86,53,62)
SC <- c(23,39,18,45)
VC <- c(32,48,23,51)

GSS <- data.frame(gender,party,VL,SL,M,SC,VC)
library(VGAM)
fit <- vglm(cbind(VL,SL,M,SC,VC) ~ gender+party, family=cumulative(parallel=TRUE), data=GSS)
summary(fit)
data.frame(gender, party, fitted(fit))
