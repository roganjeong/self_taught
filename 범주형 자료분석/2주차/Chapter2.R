# Aspirin and incidence of heart attacks
# uses the two success counts and n1 and n2, no continuity correction
prop.test(c(189,104),c(11034,11037),conf.level=0.95, correct=FALSE)

library(PropCIs) # uses binomial sucess count and n for each group
diffscoreci(189,11034,104,11037,conf.level=0.95) # score CI for difference of proportions
riskscoreci(189,11034,104,11037,conf.level=0.95) # score CI for relative risk

library(epitools) # uses the four cell counts
oddsratio(c(189,10845,104,10933),method="wald",conf=0.95,correct=FALSE) # Wald CI for odds ratio, no continuity correction

library(PropCIs) # uses success count and n for each binomial group
orscoreci(189,10845,104,10933,conf=0.95)


# Job satisfaction
job <- matrix(c(2,2,0,0,4,6,1,3,13,22,15,13,3,4,8,8),ncol=4)
job.res <- chisq.test(job)  # chi-squared independece test
job.res
stdres <- job.res$stdres
stdres

# Fisher exact test
tea <-matrix(c(3,1,1,3),ncol=2)
fisher.test(tea)
fisher.test(tea,alternative="greater")

