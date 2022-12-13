crabs <- read.table("C:/course/Categorical_Data_Analysis/CDA_My-Course/SAS_R_code/crabs.dat",header=T)
crabs
attach(crabs)

y <- ifelse(satellites>0,1,0) # y=a binary indicator of satellites
weight <- crabs$weight/1000   # weight in kilograms rather than grams
color <- crabs$color-1              # now color takes values 1,2,3,4
color.f <- factor(color)
spine.f <- factor(spine)

fit0 <- glm(y~1, family=binomial(link=logit))  # null model
fit0

fit <- glm(y~weight+width+color.f+spine.f, family=binomial(link=logit))
summary(fit)


plot(weight,fitted(fit))


# Probit Model
fit.probit <- glm(y~weight,family=binomial(link=probit))
summary(fit.probit)

# Identity link
fit.linear <- glm(y~weight,family=gaussian())
summary(fit.linear)


color <- crabs$color-1 # color now takes values 1,2,3,4
color <- factor(color) # treat color as a factor
options(contrasts=c("contr.treatment", "contr.poly"))

fit2 <- glm(y~weight+color,family=binomial(link=logit))
summary(fit2)


linear <- as.integer(color)
fit3 <- glm(y~weight+linear,family=binomial(link=logit))
summary(fit3)

