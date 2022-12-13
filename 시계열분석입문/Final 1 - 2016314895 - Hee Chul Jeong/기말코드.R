library(itsmr)
library(MASS)
library(forecast)

# 1-a
data <- read.csv('G:/내 드라이브/skku class files/2021년도 1학기/시계열분석입문/Final 1 - 2016314895 - Hee Chul Jeong/practice2-2021sp.csv')

data <- list(Dates = data[,1], Y = data[,2])
par(mfrow=c(3,1)); plot.ts(data$Y); acf2(data$Y); pacf(data$Y); par(mfrow=c(1,1))

# 1-b
vecdata <- as.vector(data$Y)
x <- 1:length(vecdata)
const <- rep(1,length(vecdata))
lmod <- lm(vecdata~x)

resi <- vecdata - lmod$fitted.values
test(resi)


# 1-c

x <- 1:length(vecdata)
const <- rep(1,length(vecdata))

# using OLS
lmod <- lm(vecdata~x)
summary(lmod)

# using ARIMA
X1 <- cbind(const, x)
fit.reg1 <- arima(vecdata, order=c(2,0,0), xreg = X1, include.mean = FALSE)
fit.reg1

# 1-d
library(MASS)
fit = boxcox(Mod(vecdata)~1, plotit=TRUE)
lambda = fit$x[which.max(fit$y)]
lambda
tran.data <- (vecdata^lambda)


lmod <- lm(tran.data~x)
resi <- tran.data - lmod$fitted.values
test(resi)

fit.1 = arima(tran.data, order = c(2,0,0), seasonal=list(order=c(1,1,0), period = 7))
fit.1

fit.2 = arima(tran.data, order = c(4,0,1), seasonal=list(order=c(2,1,0), period = 7))
fit.2

# 1-e
detach("package:itsmr")
library(forecast)
forecast(fit.2,4)
plot(forecast(fit.2,4))

newx <- 1993:1996
point.pred <- (newx * (-0.0417)) + 117.0267

lower.beta <- -0.0417 - (1.96)*0.0016
high.beta <- -0.0417 + (1.96)*0.0016

lower.pred <- (newx * (lower.beta)) + 117.0267
high.pred <- (newx * (high.beta)) + 117.0267
