yields <- c(49,39,55,41,66,68,50,55,67,58,85,92,43,38,53,42,69,62,53,48,80,73,89,99)
pesticides <- as.factor(c('A1','A1','A1','A1','A1','A1','A2','A2','A2','A2','A2','A2','A3','A3','A3','A3','A3','A3','A4','A4','A4','A4','A4','A4'))
trees <- as.factor(c('B1','B1','B2','B2','B3','B3','B1','B1','B2','B2','B3','B3','B1','B1','B2','B2','B3','B3','B1','B1','B2','B2','B3','B3'))

data1 <- data.frame(yields, pesticides)
data2 <- data.frame(yields, trees)
c1 <- c(-1,1,-1,1)
c2 <- c(-1,-1,1,1)
c3 <- c(1,-1,-1,1)
con <- cbind(c1,c2,c3)
contrasts(data1$pesticides) <- con
model1 <- aov(yields ~ pesticides, data = data1)
summary.aov(model1, split=list(pesticides=list("c1"=1, "c2" = 2, "c3"=3))) 

d1 <- c(1,-1,0)
d2 <- c(1,0,-1)
con2 <- cbind(d1,d2)
contrasts(data2$trees) <- con2
model2 <- aov(yields ~ trees, data = data2)
summary.aov(model2, split=list(trees=list("d1"=1, "d2" = 2))) 



table(pesticides,trees)
summary(aov(yields ~ pesticides + trees + pesticides:trees))
model <- lm(data = data, yields ~ tree + pesticide + tree:pesticide)
anova(model)


qf(0.999 , 1, 8)
pf(3.03, 3, 8)


yield <- c(90.4,90.2,90.1,90.3,90.5,90.7,90.7,90.6,90.5,90.6,90.8,90.9,90.2,90.4,89.9,90.1,90.4,90.1)
temperature <- as.factor(c(150,150,160,160,170,170,150,150,160,160,170,170,150,150,160,160,170,170))
pressure <- as.factor(c(200,200,200,200,200,200,215,215,215,215,215,215,230,230,230,230,230,230))
summary(aov(yield ~ temperature + pressure + temperature:pressure))

temp1 <- mean(90.4,90.2,90.7,90.6,90.2,90.4)
temp2 <- mean(90.1,90.3,90.5,90.6,89.9,90.1)
temp3 <- mean(90.5,90.7,90.8,90.9,90.4,90.1)

press1 <- mean(90.4,90.2,90.1,90.3,90.5,90.7)
press2 <- mean(90.7,90.6,90.5,90.6,90.8,90.9)
press3 <- mean(90.2,90.4,89.9,90.1,90.4,90.1)

tp11 <- mean(90.4,90.2)
tp12 <- mean(90.7,90.6)
tp13 <- mean(90.2,90.4)
tp21 <- mean(90.1,90.3)
tp22 <- mean(90.5,90.6)
tp23 <- mean(89.9,90.1)
tp31 <- mean(90.5,90.7)
tp32 <- mean(90.8,90.9)
tp33 <- mean(90.4,90.1)
grand.mean <- mean(90.4,90.2,90.1,90.3,90.5,90.7,90.7,90.6,90.5,90.6,90.8,90.9,90.2,90.4,89.9,90.1,90.4,90.1)

grand.mean <- rep(grand.mean,18)
temp <- c(rep(temp1,2),rep(temp2,2),rep(temp3,2),rep(temp1,2),rep(temp2,2),rep(temp3,2),rep(temp1,2),rep(temp2,2),rep(temp3,2))
press <- c(rep(press1,6),rep(press2,6),rep(press3,6))
tp <- c(rep(tp11,2),rep(tp21,2),rep(tp31,2),rep(tp12,2),rep(tp22,2),rep(tp32,2),rep(tp13,2),rep(tp23,2),rep(tp33,2))
res <- yield - grand.mean - (temp - grand.mean) - (press - grand.mean) - (tp - grand.mean)
plot(as.vector(temperature),res)
plot(as.vector(pressure),res)
plot(tp,res)
#qqplot(yield,res)
qqnorm(res)
qqline(res)
