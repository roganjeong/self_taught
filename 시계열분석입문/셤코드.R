library(itsmr)
library(nortest)
library(tseries)
library(robustHD)
data <- read.csv('c:/users/rogan/desktop/2020exam1.csv')
ts.data <- ts(data, start = 1932, end = 2010, freq = 12)
source('c:/users/rogan/desktop/TS-library.R')
par(mfrow=c(2,2))
plot.ts(ts.data)
acf2(data,465)
acf2(data,210)

#removing trend
x <- 1:935
x2 <- x^2
lmod <- lm(as.vector(data[,1])~1+x+x2)
summary(lmod)
plot.ts(data)
lines(lmod$fitted.values)

data2 <- as.vector(data[,1])
h.ma = optimize(f=ma.cv, interval=c(5, length(data2)/2), Y=data2, l=1, tol = .Machine$double.eps^0.25)
out.ma = smooth.ma(data2, 86)

par(mfrow=c(1,3))
plot.ts(data);
lines(out.ma, col="red")
title("Detrend- MA")
plot.ts(data-out.ma);
title("Residuals - MA")
acf2(data-out.ma)
title("SACF of Residuals - MA")


#removing seasonality
data3 <- as.vector((data-out.ma)[,1])

library(itsmr)
season.avg = season(data3, d=12)
plot.ts(data3);
title("data-out.ma")
lines(x, season.avg + mean(data3), col="red")

mean.data3 <- rep(mean(data3), 935)
linear <- lm(data3 ~ season.avg + mean.data3)
resi <- data3 - season.avg - mean(data3)
plot.ts(resi)
plot(resi)
acf(resi)
straight <- lm(resi~1+x)
lines(straight$fitted.values)

n <- length(resi)
plot(resi[1:(n-1)], resi[2:n], xlab="Y_{t-1}", ylab="Y_t")
cor(resi[1:(n-1)],resi[2:n])
test(linear$residuals)

#normality check
library(nortest)
lillie.test(linear$residuals)

library(tseries)
jarque.bera.test(linear$residuals)
#there are non-ignorable correlations

#harmonic regression
t = 1:n
m1 = 187   # f1 = n/d, fi = i*f1, lambda i = fi*(2*pi/n) = i*f1*(2*pi/n)
m2 = 374
costerm1 = cos(m1*2*pi/n*t)
sinterm1 = sin(m1*2*pi/n*t)
costerm2 = cos(m2*2*pi/n*t)
sinterm2 = sin(m2*2*pi/n*t)
harmonic.model = lm(resi ~ 1 + costerm1 + sinterm1)
summary(harmonic.model)

harmonic.model2 = lm(resi ~ 1 + costerm1 + sinterm1 + costerm2 + sinterm2)
summary(harmonic.model2)

plot.ts(resi)
lines(x, harmonic.model$fitted, col="blue")
lines(x, harmonic.model2$fitted, col="red")
# 얘는 소용이 없다는 걸 알았음


#exponential moving average
libray(QuantTools)
ema.model <-ema(resi,10)
ema.model[1:10] <- rep(0,10)
par(mfrow=c(1,1))
plot.ts(resi)
lines(ema.model, col='red')

library(itsmr)
ema2 = smooth.exp(resi, .3)
plot.ts(resi)
lines(x,ema2, col="red")
plot.ts(resi-ema2)
acf(resi-ema2)
resi2 <- resi-ema2
straight <- lm(resi2~ema2)
test(straight$residuals)
library(robustHD)
hist(straight$residuals)
hist(standardize(straight$residuals))







####################################################
data <- read.csv('c:/users/rogan/desktop/2020exam1.csv')
ts.data <- ts(data, start = 1932, end = 2010, freq = 12)
source('c:/users/rogan/desktop/TS-library.R')
data <- read.csv('c:/users/rogan/desktop/2020exam1.csv')
ts.data <- ts(data, start = 1932, end = 2010, freq = 12)
source('c:/users/rogan/desktop/TS-library.R')
par(mfrow=c(2,2))
plot.ts(ts.data)
# 대충 알아보는 단계
data.vec <- as.vector(data[,1])
x <- 1:935
x2 <- x^2
x3 <- x^3
lmod <- lm(data.vec~1+x+x2)
summary(lmod)
lines(lmod$fitted.values, col = 'blue')
#Detreding만 하는 단계
h.ma = optimize(f=ma.cv, interval=c(5, length(data.vec)/2), Y=data.vec, l=1, tol = .Machine$double.eps^0.25)
out.ma = smooth.ma(data.vec, 86)
resi.detrend.1 <- data.vec - out.ma
acf(resi.detrend.1)
plot.ts(resi.detrend.1)

#seasonality만 확인하는 단계
diff12 = diff(data.vec, lag=12)
par(mfrow=c(1,2))
plot.ts(data.vec)
plot(x[13:935], diff12, type="l", col="red")

for.test <- lm(diff12~1+x[13:935])
test(for.test$residuals)

#detrending & seasonality 둘다 확인하는 단계
diff12 <- diff(resi.detrend.1, lag = 12)
plot.ts(diff12)

for.test <- lm(diff12~1+x[13:935])
test(for.test$residuals)

plot(for.test$residuals[1:(923-1)], resi[2:923], xlab="Y_{t-1}", ylab="Y_t")
