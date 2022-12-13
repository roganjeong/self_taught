# Aspirin and incidence of heart attacks
# uses the two success counts and n1 and n2, no continuity correction
# 2-sample test for equality of proportions without continuity correction
prop.test(c(189,104),c(11034,11037),conf.level=0.95, correct=FALSE)
# 11034 - 189가 플라시보 중 심정지가 오지 않는 사람으로 자연스럽게 할당되고, 11037 - 104가 아스피린 중 심정지가 오지 않는 사람으로 자연스럽게 할당된 듯. 
# 95% 신뢰도
# correst변수는 continuity correction이 필요한지 물어보는 것 (default가 TRUE)
# 95% CI 안에 0이 포함되지 않았으므로 null hypothesis기각

library(PropCIs) # uses binomial success count and n for each group
diffscoreci(189,11034,104,11037,conf.level=0.95) # score CI for difference of proportions ( diffscoreci()랑 prop.test()랑 같은 test result를 줌, 둘 다 비율이 다른지 보는 거니까)
riskscoreci(189,11034,104,11037,conf.level=0.95) # score CI for relative risk
# relative risk에 대한 test result는 95% CI안에 1이 없으므로 null hypothesis기각 

library(epitools) # uses the four cell counts
oddsratio(c(189,10845,104,10933),method="wald",conf=0.95,correct=FALSE) # Wald CI for odds ratio, no continuity correction
# row순서로 column을 먼저 채우는 느낌인 듯 
# measure에 exposed1의 신뢰구간이 안나오는데 이건 아마 exposed2의 upper limit이 2가 넘어서 exposed1의 lower limit이 0보다 작게 나오는데, odds는 양수이므로 NA로 나온 듯...? 물어볼까...?
# 검사결과는 null hypothesis기각

library(PropCIs) # uses success count and n for each binomial group
orscoreci(189,10845,104,10933,conf=0.95)
# 위에 odds ratio랑 똑같은데 검사 결과만 간단하게 나오는 것 
# data input하는 방법 동일

# Job satisfaction
job <- matrix(c(2,2,0,0,4,6,1,3,13,22,15,13,3,4,8,8),ncol=4)
job.res <- chisq.test(job)  # chi-squared independece test
job.res$residuals
stdres <- job.res$stdres # studentized residuals, 각 cell마다 chi-squared 값에 얼마나 기여했는지 볼 수 있는 듯 
stdres

# Fisher exact test
tea <-matrix(c(3,1,1,3),ncol=2) 
fisher.test(tea) # two-sided test
fisher.test(tea,alternative="greater") # greater로 하면 one-sided (upper), less로 하면 one-sided(lower)

