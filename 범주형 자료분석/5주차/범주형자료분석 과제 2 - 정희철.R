#1-a
diagnosis <- c('schizophrenia','affective disorder','neurosis','personality disorder','special symptoms') 
drugs <- c(105,12,18,47,0)
nodrugs <- c(8,2,19,52,13) 
patients = data.frame(drugs, nodrugs)
pat.res = chisq.test(patients)
pat.res$residuals

#g2 test
library(DescTools)
gtest = GTest(patients)

st.res = pat.res$stdres
pchisq(sum(st.res^2), (5-1)*(2-1), lower.tail = F )

#1-b
diagnosis <- c('schizophrenia','affective disorder')
drugs <- c(105,12)
nodrugs <- c(8,2)
patients.g1 = data.frame(drugs, nodrugs)
pat.res.g1 = chisq.test(patients.g1)
st.res.g1 = pat.res.g1$stdres
pchisq(sum(st.res.g1^2), (2-1)*(2-1), lower.tail = F )
gtest.g1 = GTest(patients.g1)

diagnosis <- c('neurosis','personality disorder')
drugs <- c(18,47)
nodrugs <- c(19,52)
patients.g2 = data.frame(drugs, nodrugs)
pat.res.g2 = chisq.test(patients.g2)
st.res.g2 = pat.res.g2$stdres
pchisq(sum(st.res.g2^2), (2-1)*(2-1), lower.tail = F )
gtest.g2 = GTest(patients.g2)

diagnosis <- c('schizophrenia + affective disorder','neurosis + personality disorder','special symptoms')
drugs <- c(117,65,0)
nodrugs <- c(10,71,13)
patients.g3 = data.frame(drugs, nodrugs)
pat.res.g3 = chisq.test(patients.g3)
st.res.g3 = pat.res.g3$stdres
pchisq(sum(st.res.g3^2), (3-1)*(2-1), lower.tail = F )
gtest.g3 = GTest(patients.g3)

gtest$statistic == gtest.g1$statistic + gtest.g2$statistic + gtest.g3$statistic

diagnosis <- c('s + a + n + p','special symptoms')
drugs <- c(117+65,0)
nodrugs <- c(10+71,13)
patients.g4 = data.frame(drugs, nodrugs)
pat.res.g4 = chisq.test(patients.g4)
st.res.g4 = pat.res.g4$stdres
pchisq(sum(st.res.g4^2), (2-1)*(2-1), lower.tail = F )
gtest.g4 = GTest(patients.g4)

#2
smoking <- c('both parents smoke','one parent smokes','neither parent smokes')
st.yes <- c(400,416,188)
st.no <- c(1380,1823,1168)
total <- st.yes+st.no
smoke <- data.frame(smoking,st.yes,st.no,total)
smoke
smoke$cig <- c(2, 1, 0)

fit2 <- glm(st.yes/total ~ cig, family=binomial (link=logit), weights=total,data=smoke)
summary(fit2)

