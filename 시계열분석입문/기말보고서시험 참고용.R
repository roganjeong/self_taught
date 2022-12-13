# ch 3

# checking stationary conditions
ch = polyroot(c(1, -2.7607, 3.8106, -2.6535, .9238))
Mod(ch)

# Inverting ARMA(p,q) to MA(infinity)
ARMAtoMA(ar=c(1.0, -0.25),ma=c(1), lag.max=10) # constant terms can be negligible since they are always 1. As for the coefficients of AR process, the signs should be reversed. 

#Calculating theoretical ACF/PACF for ARMA(p,q)
ARMAacf(ar=c(1.0, -0.25), ma=c(1.0), lag.max = 10)
ARMAacf(ar=c(1.0, -0.25), ma=c(1.0), lag.max = 10, pacf = TRUE)

ACF = ARMAacf(ar=c(.8), lag.max=30)
PACF = ARMAacf(ar=c(.8), lag.max=30, pacf=TRUE)
par(mfrow=c(1,2))
plot(0:30, ACF, type="h", xlab="lag", ylab="ACF"); title("AR(1): ACF")
plot(PACF, type="h", xlab="lag", ylab="PACF"); title("AR(1): PACF")

#------------------------------------------------------------------
# ch 5
lake = c(10.38,11.86,10.97,10.8,9.79,
         10.39,10.42,10.82,11.4,11.32,11.44,11.68,11.17,
         10.53,10.01,9.91,9.14,9.16,9.55,9.67,8.44,
         8.24,9.1,9.09,9.35,8.82,9.32,9.01,9,9.8,9.83,9.72,9.89,
         10.01,9.37,8.69,8.19,8.67,9.55,8.92,8.09,
         9.37,10.13,10.14,9.51,9.24,8.66,8.86,8.05,
         7.79,6.75,6.75,7.82,8.64,10.58,9.48,7.38,
         6.9,6.94,6.24,6.84,6.85,6.9,7.79,8.18,
         7.51,7.23,8.42,9.61,9.05,9.26,9.22,9.38,
         9.1,7.95,8.12,9.75,10.85,10.41,9.96,9.61,
         8.76,8.18,7.21,7.13,9.1,8.25,7.91,6.89,
         5.96,6.8,7.68,8.38,8.52,9.74,9.31,9.89,9.96)

# Guessing the structure of the data using ACF and PACF graphics
par(mfrow=c(1,2))
acf2(lake, lag=30)
pacf(lake)

# Estimating AR(2) model using Yule-Walker Equation
ar.yw(lake, aic=FALSE, order.max=2, demean=FALSE)

# Estimating AR(2) model using MLE method
ar2.out = arima(lake, order=c(2,0,0), include.mean=TRUE)

ar11.out = arima(lake, order=c(1,0,1), method = c("CSS-ML"), include.mean = TRUE)
ar1.out = arima(lake, order=c(1,0,0))
ar2.out = arima(lake, order=c(2,0,0))
ma1.out = arima(lake, order=c(0,0,1))

# Automatically estimating the order and the coefficients of ARIMA(p,d,q)
library(forecast)
fit=auto.arima(lake, d=0)
2*(1-pnorm(fit$coef/(sqrt(diag(fit$var.coef)))))

# Formal testing for residuals
library(itsmr)
test(residuals(fit)) # test of randomness

# Forecasting
detach("package:itsmr")
library(forecast)
forecast(fit, 30)

# Checking the goodness of fit 
tsdiag(ar11.out)

# Finding the best model using the information criteria
AICC = BIC = AIC = P = Q = NULL
pmax=3
qmax=3
n = length(lake)
for(p in 0:qmax){
  for(q in 0:qmax){
    fit = arima(lake, order=c(p, 0, q), include.mean=TRUE)
    m = p+q+2
    AIC = c(AIC, -2*fit$loglik + 2*m)
    AICC = c(AICC, -2*fit$loglik + 2*m*n/(n-m-1))
    BIC = c(BIC, -2*fit$loglik + m*log(n))
    P = c(P, p)
    Q = c(Q, q)
  }}                   # calculating different scores for different orders

id1 = which.min(AICC)
id2 = which.min(BIC)
c(P[id1], Q[id1]) # yielding the best model orders
c(P[id2], Q[id2])
plot(AICC)
plot(BIC)

#------------------------------------------------------------

# ch 6

# square-root transformation
plot.ts(data^(1/2))
title("x^(1/2) transformation")

qqnorm(data^(1/2))
qqline(data^(1/2))

# log-transformation
plot.ts(log(data))
title("Log-transformation")

qqnorm(log(data))
qqline(log(data))

# Box-cox transformation for choosing the best lambda for transformation
library(MASS)
library(itsmr)
x = 1:length(data)
fit = boxcox(data~1, plotit=TRUE)
lambda = fit$x[which.max(fit$y)]
lambda

dat.bx = data^lambda
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(dat.bx, type="l")
title("airpass^.14")
qqnorm(dat.bx)
qqline(dat.bx)
acf(dat.bx, lag=50)
pacf(dat.bx, lag=50)




# SARIMA models
# Fitting SARIMA(p,d,q)x(P,D,Q) model
fit.1 = arima(dat, order = c(1,1,0), seasonal=list(order=c(1,0,0), period=12))
fit.1

plot(forecast(fit.1, h=12))

# Model selection by AICC/BIC finds
dat.ff = ts(dat, frequency=12)
auto.arima(dat.ff)


# Adaptive LASSO
rm(list=ls(all=TRUE))
library(parcor)

ar.adaplasso = function(y, p, nf){
  if(missing(nf)){ nf = 10 };
  if(missing(p)){ phat = ar(y, aic = TRUE, order.max=10)$order }
  # Check y is a vector
  y = as.vector(y);
  n = length(y);
  mu.s = mean(y);
  id = 1:n;
  X = NULL;
  for(j in 1:p){
    id1 = id-j;
    id2 = id1[id1 <= 0];
    id3 = id1[id1 > 0];
    X = cbind(X, c(rep(mu.s, length(id2)), y[id3]));
  }
  pp = adalasso(X, y, k=nf, intercept=TRUE);
  return(pp)
}
# creating parameters and orders
n=250
#phi = c(.5, .3, .1);
phi = c(.7, .3, 0, -.2)
nrep=50
order=5
A = B = matrix(0, nrep, order)

for(r in 1:nrep){
  data = arima.sim(n = n, list(ar = phi), sd = 1)
  y = data/sd(data)
  fit = ar.adaplasso(y, p=order)
  fit = ar.adaplasso(y, p=order)
  A[r,] = fit$coefficients.adalasso
  B[r,] = fit$coefficients.lasso
  print(r)
}

par(mfrow=c(1,2))
boxplot(A, main="aLasso")
boxplot(B, main="Lasso")

