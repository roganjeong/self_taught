therapy0 <- factor(c(rep("Sequential",2),rep("Alternating",2)))
therapy <- relevel(therapy0,ref="Sequential")
gender0 <- factor(rep(c("Male","Female"),2))
gender <- relevel(gender0,ref="Male")

PD <- c(28,4,41,12)
NC <- c(45,12,44,7)
PR <- c(29,5,20,3)
CR <- c(26,2,20,1)
# without the interaction term
chemotherapy <- data.frame(therapy,gender,PD,NC,PR,CR)
library(VGAM)
fit <- vglm(cbind(PD,NC,PR,CR) ~ therapy+gender, family=cumulative(parallel=TRUE), data=chemotherapy)
summary(fit) # residual deviance = 5.5677 on 7 degrees of freedom, log-likelihood = -25.5417 on 7 degrees of freedom
data.frame(therapy, gender, fitted(fit))

# with the interaction term
fit1 <- vglm(cbind(PD,NC,PR,CR) ~ therapy+gender+(therapy*gender), family=cumulative(parallel=TRUE), data=chemotherapy)
summary(fit1) # residual deviance = 4.5209 on 6 degrees of freedom , log-likelihood = -25.0183 on 6 degrees of freedom


#####################
# log-linear
safety <- factor(c(rep("Seat Belt",2),rep("None",2),rep("Seat Belt",2),rep("None",2)), levels=c("Seat Belt","None"))
safety.f <- relevel(safety, ref="Seat Belt")
ejected <- factor(rep(c('Yes','No'),4))
ejected.f <- relevel(ejected, ref="Yes")
injury <- factor(c(rep('Nonfatal',4), rep('Fatal',4)), levels=c("Nonfatal","Fatal"))
injury.f <- relevel(injury, ref = 'Fatal')
count.severe <- c(1105,411111,4624,157342,14,483,497,1008)
accident.data <- data.frame(safety,ejected,injury,count.severe)

fit3 <- glm(count.severe ~ safety.f + ejected.f + injury.f, family=poisson, data=accident.data)
summary(fit3)

# logistic model
safety0 <- factor(c(rep("seat belt",2),rep("none",2)))
safety <- relevel(safety0,ref="none")
ejected0 <- factor(rep(c("Yes","No"),2))
ejected <- relevel(ejected0,ref="Yes")

Nonfatal <- c(1105,411111,4624,157342)
Fatal <- c(14,483,497,1008)
accident <- data.frame(safety, ejected, Nonfatal, Fatal)

fit4 <- glm(count.severe ~ safety.f + ejected.f + injury.f, family=poisson, data=accident.data)
summary(fit4)