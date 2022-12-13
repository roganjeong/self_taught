#----------------------------------------------------------#
#         Job satisfication using loglinear model        #
#----------------------------------------------------------#

sat <- factor(c(rep("VD",4),rep("LD",4),rep("MS",4),rep("VS",4)), levels=c("VD","LD","MS","VS"))
sat.f <- relevel(sat, ref="VS")
income <- factor(rep(c(3,10,20,30),4))
income.f <- relevel(income, ref="30")
count <- c(2,2,0,0,4,6,1,3,13,22,15,13,3,4,8,8)
job.sat.data <- data.frame(income,job.sat,count)

fit1 <- glm(count ~ income.f+sat.f, family=poisson, data=job.sat.data)
summary(fit1)

# Saturated model
fit2 <- glm(count ~ income.f*sat.f, family=poisson, data=job.sat.data)
summary(fit2)


#----------------------------------------------------------#
#         Berkley admission using loglinear model     #
#----------------------------------------------------------#

berkeley <- as.data.frame(UCBAdmissions) 
head(berkeley)
berkeley$Admit <- relevel(berkeley$Admit, ref="Rejected")
berkeley$Dept <- relevel(berkeley$Dept,ref="F")
berkeley$Gender <- relevel(berkeley$Gender,ref="Male")

#(A,G,D)
fit1 <- glm(Freq ~ Admit+Gender+Dept, family=poisson, data=berkeley)
summary(fit1)

#(AD,DG)
fit2 <- glm(Freq ~ Admit*Dept+Dept*Gender, family=poisson, data=berkeley)
summary(fit2)
cbind(berkeley,resid(fit2))

#(AG, AD, DG)
fit3 <- glm(Freq ~ Admit*Dept+Dept*Gender+Admit*Gender, family=poisson, data=berkeley)
summary(fit3)
cbind(berkeley,resid(fit3))


# (AD, DG) without department A
id.nonA <- berkeley$Dept!='A'
berkeley.nonA <- berkeley[id.nonA,]

fit.nonA1 <- glm(Freq ~ Admit*Dept+Dept*Gender, family=poisson, data=berkeley.nonA)
summary(fit.nonA1)
cbind(berkeley.nonA,resid(fit.nonA1))

# (AG, AD, DG) without department A
fit.nonA2 <- glm(Freq ~ Admit*Dept+Dept*Gender+Admit*Gender, family=poisson, data=berkeley.nonA)
summary(fit.nonA2)
cbind(berkeley.nonA,resid(fit.nonA2))


#------------------------------------------------------------#
#   Automobile accidents data using loglinear model  #
#-------------------------------------------------------------#

setwd('D:/course/SKKU/Category-Data-Analysis/Undergraduate/2021/SAS_R_code/R')
Accidents <- read.table('Accidents2.dat', header=T)
Accidents

G <- relevel(factor(Accidents$gender), ref='male')
L <- relevel(factor(Accidents$location), ref='urban')
S <- relevel(factor(Accidents$seatbelt), ref='yes')
I <-  relevel(factor(Accidents$injury), ref='yes')

fit1 <- glm(count ~ G*I+G*L+G*S+I*L+I*S+L*S, family=poisson, data=Accidents)
summary(fit1)
sum(abs(Accidents$count-fitted(fit1)))/(2*sum(Accidents$count))  # dissimilarity index

fit2 <- glm(count ~ G*L*S+G*I+I*L+I*S, family=poisson, data=Accidents)
summary(fit2)
sum(abs(Accidents$count-fitted(fit2)))/(2*sum(Accidents$count)) # dissimilarity index


#-----------------------------------------------------------------------#
#                               death data                                       #
#------------------------------------ ----------------------------------#

v1 <- c('w','w','b','b','w','w','b','b')
d1 <- c('w','w','w','w','b','b','b','b')
p1 <- rep(c('yes','no'),4)

v <- relevel(factor(v1),ref="w")
d <- relevel(factor(d1),ref="w")
p <- relevel(factor(p1), ref="yes")
count <- c(53,414,0,16,11,37,4,139)

death <- data.frame(v,d,p,count)

# (DP, PV, DV)
fit1 <- glm(count ~ d*v+d*p+v*p, family=poisson, data=death)
summary(fit1)
deviance(fit1)
cbind(count,fitted(fit1),resid(fit1),rstandard(fit1, type="pearson"))

# (DP, PV)
fit2 <- glm(count ~ d*v+v*p, family=poisson, data=death)
summary(fit2)
deviance(fit2)



#-----------------------------------------------------------------------#
#                              Heart valve data                                 #
#------------------------------------ ----------------------------------#

age <- relevel(factor(c(1,1,2,2)), ref="2")
valve <- relevel(factor(c(1,2,1,2)), ref="2")
deaths <- c(4,1,7,9)
exposure <- c(1259,2082,1417,1647)
log.exp <- log(exposure)

fit1 <- glm(deaths ~ age+valve, family=poisson, offset=log.exp )
summary(fit1)

fit2 <- glm(deaths ~ age, family=poisson, offset=log.exp )
summary(fit2)

