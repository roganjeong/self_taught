# 1
# 1-a
data <- read.csv('G:/내 드라이브/skku class files/2021년도 1학기/시계열분석입문/pastexam/2020exam2.csv')
vecdata <- as.vector(data[,1])

par(mfrow=c(1,2))
plot.ts(data)
acf2(data, lag=30)

# The timeplot strongly suggests that the data has either a linear or quadratic trend and that it has seasonality, which can be inferred by the fact that the data is quarterly time series. 
# The ACF diagram also shows that there is a strong correlations for every h = 4. The interesting part is that the pattern of correlations is very consistent for different h, and they are all decreasing at the equivalent rate as the h increases. 

# 1-b
library(itsmr)
test(vecdata)
qqnorm(vecdata); qqline(vecdata)
pacf(vecdata)
# No, it is not stationary because first, there is either a linear or quadratic trend, which suggests that there is a non-constant mean, and this violates the first condition of the stationary time series. 
# Second, the data does not follow normality, which may indicate there is a non-constant variance. 

# 1-c
x <- 1:999
x2 <- x^2
trend1 <- lm(as.vector(data[,1]) ~ 1 + x); summary(trend1)
trend2 <- lm(as.vector(data[,1]) ~ 1 + x + x2); summary(trend2)
resi1 <- trend1$residuals
resi2 <- trend2$residuals
const <- rep(1,999)
X1 <- cbind(const, x)
X2 <- cbind(const, x, x2)
fit.reg1 <- arima(vecdata, order=c(2,0,0), xreg = X1, include.mean = FALSE)
fit.reg1

fit.reg2 <- arima(vecdata, order=c(3,0,0), xreg = X2, include.mean = FALSE)
fit.reg2
# The estimated model : Y = - 146.9455 - 15.0489 * x + 0.0208 * x^2
# I previously assumed that there is either a linear or quadratic trend in the data so I first examined the validity of the two assumptions using OLS method, one for linear and the other for quadratic. 
# Both forms show significance but the quadratic model showed higher statistics in every aspect. Therefore, I chose to optimize my model as a quadratic model. 
# Then, I used AR(2) function to estimate the coefficients and the standard errors of the coefficients because OLS estimators may provide plausible estimates but the standard errors are to high to be used. 

# 1-d
neg.index <- which(vecdata < 0)
tran.data <- vecdata

library(MASS)
library(itsmr)
x = 1:length(vecdata)
fit = boxcox(vecdata~1, plotit=TRUE)
lambda = fit$x[which.max(fit$y)]
lambda

tran.data <- (vecdata^lambda)
tran.data[neg.index] <- (Mod(vecdata)^lambda)[neg.index] * (-1)

plot(tran.data, type="l")
qqnorm(tran.data)
qqline(tran.data)
acf(tran.data, lag=50)
pacf(tran.data, lag=50)

fit.1 = arima(dat.bx, order = c(1,4,0), seasonal=list(order=c(1,0,0), period = 4))
fit.1

fit.2 = arima(dat.bx, order = c(1,1,0), seasonal=list(order=c(1,0,0), period = 4))
fit.2

plot(forecast(fit.1, h=4))
#### 다시
plot.ts(data)

fit.1 = arima(vecdata, order = c(1,1,0), seasonal=list(order=c(1,0,0), period = 4))
fit.1

fit.2 = arima(vecdata, order = c(1,2,0), seasonal=list(order=c(1,0,0), period = 4))
fit.2

fit.3 = arima(vecdata, order = c(1,3,0), seasonal=list(order=c(1,0,0), period = 4))
fit.3

#다시 
par(mfrow=c(2,2)); plot.ts(data[1:250,1]); plot.ts(data[251:500,1]); plot.ts(data[501:750,1]); plot.ts(data[751:999,1]); par(mfrow=c(1,1))
qqnorm(vecdata-trend2$fitted.values); qqline(vecdata-trend2$fitted.values)
# By the 4 time series plots, there seems to be no obvious sign of heterogeneity, and the QQ-plot suggests that the residuals are almost perfectly aligned. Thus, no transformation is needed. 

par(mfrow=c(1,2)); acf(tran.data, lag=50); pacf(tran.data, lag=50); par(mfrow=c(1,1))
# The PACF at lag = 4 and the slowly decaying correlation coefficients on ACF at every lag = 4 suggest the seasonal ARIMA(1,1) at s = 4.
# In addition, there are 2 significant point at lag < 4 on PACF diagram, and those lags on ACF also slowly decaying, which may indicate temporal ARIMA(2,1). 
# Considering the above two inference, SARIMA(2,1,0)x(1,1,0) seems plausible.

fit = arima(vecdata, order = c(2,1,0), seasonal=list(order=c(1,1,0), period = 4))
fit

dat.ff <- ts(vecdata, 4)
auto.arima(dat.ff)
auto.arima(vecdata)

plot(forecast(fit,12))

library(itsmr)
test(residuals(fit.1))
test(residuals(fit.2))
test(residuals(fit.3))


# 1-e
library(forecast)
forecast(fit.1,12)
plot(forecast(fit.1,12))

h=12
newx = cbind(c(1000:1011),c(1000:1011)^2)
plot(forecast(fit.reg2, h=12, xreg = cbind(rep(1, h), newx)))
lines(out.lm$fitted.values, col="red")



#------------------------------------------------------------------------------
# hw-5

#3
mys <- read.csv('G:/내 드라이브/skku class files/2021년도 1학기/시계열분석입문/HWs/mysterious.txt')
par(mfrow = c(3,1)); plot.ts(mys); acf(mys); pacf(mys); par(mfrow=c(1,1))

# The time plot does not have any trend and seems that the variance is not heterogeneous. The PACF cuts off at lag 1, which indicates the plausibility of AR(p) model. 
# The ACF fades rapidly. ARMA(1,0) seems plausible.






