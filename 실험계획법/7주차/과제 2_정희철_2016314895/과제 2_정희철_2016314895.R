# 1
x1 <- rep(0,10)
x2 <- rep(5,10)
x3 <- rep(10,10)
x4 <- rep(15,10)
groups <- c(x1,x2,x3,x4)

y1 <- c(6.7, 7.8, 5.5, 8.4, 7.0, 7.8, 8.6, 7.4, 5.8, 7.0)
y2 <- c(9.9, 8.4, 10.4, 9.3, 10.7, 11.9, 7.1, 6.4, 8.6, 10.6)
y3 <- c(10.4, 8.1, 10.6, 8.7, 10.7, 9.1, 8.8, 8.1, 7.8, 8.0)
y4 <- c(9.3, 9.3, 7.2, 7.8, 9.3, 10.2, 8.7, 8.6, 9.3, 7.2)
obs <- c(y1,y2,y3,y4)

hemo <- matrix(c(x1,x2,x3,x4,y1,y2,y3,y4), nrow=40)
colnames(hemo) <- c('sulfamerazine','hemoglobin')
hemo
plot(hemo[,1], hemo[,2])
hemo

boxplot(hemo[1:10,2], hemo[11:20,2], hemo[21:30,2], hemo[31:40,2])

# confidence interval
t.test(y1, conf.level = 0.95)



# ANOVA
x1 <- rep(0,10)
x2 <- rep(5,10)
x3 <- rep(10,10)
x4 <- rep(15,10)
groups <- c(x1,x2,x3,x4)

y1 <- c(6.7, 7.8, 5.5, 8.4, 7.0, 7.8, 8.6, 7.4, 5.8, 7.0)
y2 <- c(9.9, 8.4, 10.4, 9.3, 10.7, 11.9, 7.1, 6.4, 8.6, 10.6)
y3 <- c(10.4, 8.1, 10.6, 8.7, 10.7, 9.1, 8.8, 8.1, 7.8, 8.0)
y4 <- c(9.3, 9.3, 7.2, 7.8, 9.3, 10.2, 8.7, 8.6, 9.3, 7.2)
obs <- c(y1,y2,y3,y4)

hemo
hemo.df <- data.frame(obs,groups)
sapply(hemo.df[,2], class)
hemo.df <- transform(hemo.df, groups = factor(groups))

n1 <- aov(obs~groups,hemo.df)
summary(n1)

# bonferroni

# Use p.adjust
bonferroni_ex <- p.adjust(.005, method = "bonferroni", n = 6)
# Pairwise t-test
pairwise.t.test(obs, groups, p.adjust = "bonferroni")

# Tukey's HSD
TukeyHSD(n1)

sum((y1 - mean(y1))^2)
sum((y2 - mean(y2))^2)
sum((y3 - mean(y3))^2)
sum((y4 - mean(y4))^2)

sum(sum((mean(y1) - mean(obs))^2),
    sum((mean(y2) - mean(obs))^2),
    sum((mean(y3) - mean(obs))^2),
    sum((mean(y4) - mean(obs))^2))
sum((obs - mean(obs))^2)

# 4 - b 
(diet_fat    <- factor(rep(1:3, each = 5)))
(ages <- factor(rep(1:5, times = 3)))
y <- c(.73, .67, .15,
       .86, .75, .21,
       .94, .81, .26,
       1.4, 1.32, .75,
       1.62, 1.41, .78)
data <- data.frame(y, diet_fat, ages)

fit <- aov(y ~ ages + diet_fat)                 
summary(fit)


# 4 - c
data[1:5,1]
fat_means <- c(mean(data[1:5,1]), mean(data[6:10,1]), mean(data[11:15,1]))
age_means <- c( mean(data[c(1,6,11),1]), mean(data[c(2,7,12),1]), mean(data[c(3,8,13),1]), mean(data[c(4,9,14),1]), mean(data[c(5,10,15),1]))
data
data.frame(y, rep(fat_means, each = 5), rep(age_means, times = 3) )


resi <- y - rep(fat_means, each = 5) - rep(age_means, times = 3) + rep(mean(y), 15)
plot(resi)
qqnorm(resi)
qqline(resi, col = 2)
hist(resi)


# 4 - d 
library(asbio)
summary(tukey.add.test(y, diet_fat, ages))


