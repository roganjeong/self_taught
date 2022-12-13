data <- read.csv('G:/내 드라이브/skku class files/2021년도 1학기/시계열분석입문/pastexam/2020exam2.csv')

# 1
# 1-a
plot.ts(data)
acf2(data, lag=100)
acf2(data, lag=20)

# 1-b
# No! It is not stationary

# 1-c
x <- 1:999
trend <- lm(as.vector(data[,1]) ~ 1 + x)
resi <- trend$residuals
const <- rep(1,999)
X <- cbind(const, x)
fit.reg <- arima(as.vector(data[,1]), order=c(2,0,0), xreg = X, include.mean = FALSE)
fit.reg
summary(trend)

# 1-d 
data.vec <- as.vector(data[,1])
qqnorm(data.vec);qqline(data.vec)



tran.data <- log(Mod(data.vec))
neg.ind <- which(data.vec < 0)
tran.data <- tran.data[neg.ind] *(-1)
qqnorm(tran.data);qqline(tran.data)

library(MASS)
library(itsmr)
x = 1:999
data.vec2 <- Mod(data.vec)
fit = boxcox(data.vec2~1, plotit=TRUE)
lambda = fit$x[which.max(fit$y)]
lambda

dat.bx = (data.vec2^lambda)[neg.ind]*(-1)
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(dat.bx, type="l")

qqnorm(dat.bx)
qqline(dat.bx)
acf(dat.bx, lag=50)
pacf(dat.bx, lag=50)

