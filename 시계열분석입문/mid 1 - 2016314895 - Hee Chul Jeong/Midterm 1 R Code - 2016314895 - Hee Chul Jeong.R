library(itsmr)
library(nortest)
library(tseries)
library(robustHD)
library(QuantTools)


# 1
#1-a
data <- as.vector(read.csv('c:/users/rogan/desktop/2021practice1.csv', header = FALSE)[,1])
source('c:/users/rogan/desktop/TS-library.R')
par(mfrow=c(2,1))
plot.ts(data)
acf2(data,499)
par(mfrow=c(1,1))

#1-b
x <- 1:length(data)
x2 <- x^2
lmod <- lm(data~1+x)
lmod2 <- lm(data~1+x+x2)
summary(lmod)
summary(lmod2)
lines(lmod$fitted.values, col='red')
lines(lmod2$fitted.values, col='blue')
data2 <- data - lmod2$fitted.values
plot.ts(data2)
title('After Detrending with Quadratic Regression')

#1-c
# candidate 1 : harmonic regression
n <- length(data2)
t = 1:n
f1 = 9.615385   # f1 = n/d, fi = i*f1, lambda i = fi*(2*pi/n) = i*f1*(2*pi/n)
f2 = (9.615385)*2
f3 = (9.615385)*3
f4 = (9.615385)*4
costerm1 = cos(f1*2*pi/n*t)
sinterm1 = sin(f1*2*pi/n*t)
costerm2 = cos(f2*2*pi/n*t)
sinterm2 = sin(f2*2*pi/n*t)
costerm3 = cos(f3*2*pi/n*t)
sinterm3 = sin(f3*2*pi/n*t)
costerm4 = cos(f4*2*pi/n*t)
sinterm4 = sin(f4*2*pi/n*t)
harmonic.model = lm(data2 ~ 1 + costerm1 + sinterm1)
summary(harmonic.model)

harmonic.model2 = lm(data2 ~ 1 + costerm1 + sinterm1 + costerm2 + sinterm2)
summary(harmonic.model2)

harmonic.model3 = lm(data2 ~ 1 + costerm1 + sinterm1 + costerm2 + sinterm2+ costerm3 + sinterm3 + costerm4 + sinterm4)

plot.ts(data2)
lines(x, harmonic.model$fitted, col="blue")
lines(x, harmonic.model2$fitted, col="red")
lines(x, harmonic.model3$fitted, col="green")
legend(0, 500, lty=c(2,2), col=c("blue","red",'green'), c("k=1", "k=2",'k=3'))

data3 <- data2 - harmonic.model$fitted
plot.ts(data3)

data3 <- data2 - harmonic.model2$fitted
plot.ts(data3)

data3 <- data2 - harmonic.model3$fitted
plot.ts(data3)
hist(data3)
#not successful

# candidate 2 : lag differencing
diff12 = diff(data2, lag=10)
par(mfrow=c(1,2))
plot.ts(data2)
plot(x[11:500], diff12, type="l", col="red")
plot.ts(data2 - diff12)

#not successful

# candidate 3 : seasonal smoothing
season.avg = season(data2, d=71)
plot.ts(data2)
lines(x, season.avg + mean(data2), col="red")
data3 <- data2 - (season.avg + mean(data2))
plot.ts(data3)


# candidate 4 : exponential moving average
par(mfrow=c(1,1))
ema = smooth.exp(data2, .2)
plot.ts(data2)
lines(x,ema, col="red")
par(mfrow=c(2,1))
data3 <- data2 - ema
plot.ts(data3)
hist(data3)  # ~ normal distribution

test(data3)