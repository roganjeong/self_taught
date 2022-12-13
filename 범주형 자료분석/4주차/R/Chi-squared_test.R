chisq.test(matrix(c(279,73,225,165,47,191),ncol=3,byrow=T))
setwd('C:/course/Categorical_Data_Analysis/CDA_My-Course/SAS_R_code')
data <- read.table("gendergap.dat",header=T)  # read separate file
data

attach(data)

gendergap.fit <- glm(count~gender+party,family=poisson(link=log))
gendergap.fit

1-pchisq(7.0026,2) #P-value
expected <- fitted(gendergap.fit) # estimated expected frequencies
residual <- (count-expected)/sqrt(expected) # Pearson residuals

count; expected; residual

matrix(c(count,expected,residual),ncol=3)

count2 <- c(279,73,165,47)
party2 <- c('d','i','d','i')
gender2 <- c('f','f','m','m')

fit2 <- glm(count2~as.factor(party2)+as.factor(gender2),family=poisson(link=log))
fit2

count3 <- c(352,225,212,191)
fit3 <- glm(count3~as.factor(party2)+as.factor(gender2),family=poisson(link=log))
fit3
