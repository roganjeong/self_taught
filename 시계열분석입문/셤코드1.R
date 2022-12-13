# Regression
source('C:/Users/rogan/Desktop/TS-library.R')
data <- scan('c:/users/rogan/desktop/huron.txt')
par(mfrow=c(1,1))

plot.ts(data)
(n <- length(data))
x <- seq(1, n, by = 1)
out.lm <- lm(data~1 + x)
out.lm$fitted.values
lines(out.lm$fitted.values, col='red')

x2 <- x^2
out.lm2 <- lm(data~1 + x + x2)
lines(out.lm2$fitted.values, col = 'blue')

par(mfrow=c(2,2))
plot(out.lm)

acf(out.lm$residuals,lag = 35, type='correlation')
par(mfrow=c(2,2))
acf2(out.lm$residuals)
title("sample ACF of residuals");
plot(out.lm$residuals[1:(n-1)], out.lm$residuals[2:n], xlab="Y_{t-1}", ylab="Y_t")
title("Plot of residuals - Lag1")
plot(out.lm$residuals[1:(n-2)], out.lm$residuals[3:n], xlab="Y_{t-2}", ylab="Y_t")
title("Plot of residuals - Lag2")
plot(out.lm$residuals[1:(n-3)], out.lm$residuals[4:n], xlab="Y_{t-3}", ylab="Y_t")
title("Plot of residuals - Lag3")
par(mfrow=c(1,1))

#smoothing
ma.cv  # h = bandwidth, Y = data, l = number of test set(usually 1)
h.ma = optimize(f=ma.cv, interval=c(5, length(data)/2), Y=data, l=1, tol = .Machine$double.eps^0.25)
out.ma = smooth.ma(data, h.ma$minimum)

out.ma = smooth.ma(data, 32) # h.ma를 참고하는 것 같음
par(mfrow=c(1,3))
plot.ts(data);
lines(out.ma, col="red")
title("Detrend- MA")
plot.ts(data-out.ma);
title("Residuals - MA")
acf2(data-out.ma)
title("SACF of Residuals - MA")

plot(out.lm$fitted.values)
plot(out.ma)
plot(out.lm2$fitted.values)
#save.image("huron.Rdata")


#removing seasonality
rm(list=ls(all=TRUE))
source("c:/users/rogan/desktop/TS-library.R")
data = scan("c:/users/rogan/desktop/deaths.txt");
data = ts(data, start=1973, end=1979, freq=12)
n = length(data)
par(mfrow=c(1,2))
plot.ts(data);
title("US accidental deaths")
acf2(data, 35);
title("SACF")

#harmonic regression
t = 1:n;
m1 = 6;   # f1 = n/d, fi = i*f1, mi = fi*(2*pi/n) = i*f1*(2*pi/n)
m2 = 12;
costerm1 = cos(m1*2*pi/n*t);
sinterm1 = sin(m1*2*pi/n*t);
costerm2 = cos(m2*2*pi/n*t);
sinterm2 = sin(m2*2*pi/n*t);
out.lm1 = lm(data ~ 1 + costerm1 + sinterm1)
summary(out.lm1)

out.lm2 = lm(data ~ 1 + costerm1 + sinterm1 + costerm2 + sinterm2)
summary(out.lm2)

par(mfrow=c(1,1))
x = as.vector(time(data))
plot.ts(data);
title("US accidental deaths")
lines(x, out.lm1$fitted, col="blue")
lines(x,out.lm2$fitted, col="red")
legend(1975, 10800, lty=c(1,1), col=c("blue","red"), c("k=1", "k=2"))

par(mfrow=c(2,2))
#plot(out.lm2)
plot.ts(data);
title("US accidental deaths")
lines(x,out.lm2$fitted, col="red")
plot(out.lm2$fitted, out.lm2$residuals);
title("Residuals vs Fitted")
acf2(out.lm2$residuals)
title("SACF-residuals")
qqnorm(out.lm2$residuals);
qqline(out.lm2$residuals)


#seasonal averaging
library(itsmr)
season.avg = season(data, d=12)
plot.ts(data);
title("US accidental deaths")
lines(x, season.avg + mean(data), col="red")
resi <- data - season.avg - mean(data)
plot.ts(resi)

#seasonal differencing
diff12 = diff(data, lag=12);
par(mfrow=c(1,2))
plot.ts(data);
title("US accidental deaths")
plot(x[13:73], diff12, type="l", col="red")
title("Seasonal differencing")

#classical
out = classical(data, d=12, order=1);
par(mfrow=c(2,2))
plot.ts(data);
title("step1")
lines(x, out$m1, col="red");
plot.ts(data-out$m1);
title("step2")
lines(x, out$st, col="red");
plot.ts(data-out$st);
title("step3")
lines(x, out$m, col="red");
plot.ts(data);
lines(x, out$fit, col="red")
title("Final")


#exponential moving average
ExMA <- function (data,n){
  ema <- c()
  ema[1:(n-1)] <- NA
  ema[n]<- mean(data[1:n])
  a <- 2/(n+1)
  for (i in (n+1):length(data)){
    ema[i]<-a * data[i] + 
      (1-a) * ema[i-1]
  }
  
  return(ema)
}
ema <-ExMA(data,n=20)
par(mfrow=c(1,1))
plot(data)
lines(ema, col='red')


#spencer's moving average filter (얜 모르겠다)
mys <- read.csv('c:/users/rogan/desktop/2020exam1.csv')
class(mys)
plot.ts(mys)

A <- c(-3,-6,-5,3,21,46,67,74,67,46,21,3,-5,-6,-3)/320
n <- length(as.vector(mys[,1]))
mys <- as.vector(mys[,1])
each15s <- c()
data15 <- c()
each15s <- c()
for (i in 1:935){
  data15 <- c(mys[i],mys[i+1],mys[i+2],mys[i+3],mys[i+4],mys[i+5],mys[i+6],mys[i+7],mys[i+8],mys[i+9],mys[i+10],mys[i+11],mys[i+12],mys[i+13],mys[i+14])
  each15s[i] <- sum(data15*A)
}
each15s



for (i in 8:n){
  data15 <- c(mys[i-7],mys[i-6],mys[i-5],mys[i-4],mys[i-3],mys[i-2],mys[i-1],mys[i],mys[i+1],mys[i+2],mys[i+3],mys[i+4],mys[i+5],mys[i+6],mys[i+7])
  each15s[i-7] <- sum(data15*A)
}
each15s
plot.ts(mys)
lines(each15s, col = 'red')


mys[1:15]*A


