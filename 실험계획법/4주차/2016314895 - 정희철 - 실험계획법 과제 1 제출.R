# 2016314895 - 정희철, 통계학과
# 과제 1

#2-e
g1 <- c(41,46,56,56,69)
g2 <- c(36,7,53,31,76)
g3 <- c(103,75,84,82,80)
g4 <- c(53,30,70,80,42)
(full <- matrix(c(g1,g2,g3,g4),nrow=5))
(aligned <- data.frame('obs'=c(g1,g2,g3,g4),'treatment'=c('1','1','1','1','1','2','2','2','2','2','3','3','3','3','3','4','4','4','4','4')))

summary(aov(obs~treatment, data=aligned))

#2-c
g1 - mean(g1)
g2 - mean(g2)
g3 - mean(g3)
g4 - mean(g4)
mean(c(g1,g2,g3,g4))
t <- full - mean(c(g1,g2,g3,g4))
sum(t^2)/19
