setwd("~/Dropbox/Public/TSA2020sp/practice")
rm(list=ls(all=TRUE))
#install.packages("itsmr")
library(itsmr)
source("TS-library.R")
data = scan("huron.txt")

plot.ts(data);
title("Lake Huron Water level")

n = length(data);
x = seq(from=1, to = n, by=1);
x2 = x^2;
out.lm = lm(data ~ 1 + x);
out.lm2 = lm(data ~ 1 + x + x2);
test(out.lm2$residuals);


plot.ts(data);
title("Lake Huron Water level");
lines(out.lm2$fitted.values, col="red")


par(mfrow=c(2,2));
plot(out.lm2)

acf2(out.lm2$residuals);
title("sample ACF of residuals");
plot(out.lm2$residuals[1:(n-1)], out.lm$residuals[2:n], xlab="Y_{t-1}", ylab="Y_t");
title("Plot of residuals - Lag1")
plot(out.lm2$residuals[1:(n-2)], out.lm$residuals[3:n], xlab="Y_{t-2}", ylab="Y_t");
title("Plot of residuals - Lag2")
plot(out.lm2$residuals[1:(n-3)], out.lm$residuals[4:n], xlab="Y_{t-3}", ylab="Y_t");
title("Plot of residuals - Lag3")


h.ma = optimize(f=ma.cv, interval=c(5, length(data)/2), Y=data, l=1, tol = .Machine$double.eps^.25);
out.ma = smooth.ma(data, h.ma$minimum)
test(data - out.ma);


out.ma = smooth.ma(data, 32);
par(mfrow=c(1,3));
plot.ts(data);
lines(out.ma, col="red"); title("Detrend- MA");
plot.ts(data-out.ma); title("Residuals - MA");
acf2(data-out.ma);
title("SACF of Residuals - MA")
save.image("huron.Rdata")


t = 1:n;
m1 = 6;
m2 = 12;
costerm1 = cos(m1*2*pi/n*t);
sinterm1 = sin(m1*2*pi/n*t);
costerm2 = cos(m2*2*pi/n*t);
sinterm2 = sin(m2*2*pi/n*t);
out.lm1 = lm(data ~ 1 + costerm1 + sinterm1);
summary(out.lm1)

out.lm2 = lm(data ~ 1 + costerm1 + sinterm1 + costerm2 + sinterm2);
summary(out.lm2)


par(mfrow=c(2,2));
plot.ts(data);
title("US accidental deaths");
lines(x,out.lm1$fitted, col="red");
plot(out.lm1$fitted, out.lm1$residuals);
title("Residuals vs Fitted");
acf2(out.lm1$residuals);
title("SACF-residuals");
qqnorm(out.lm1$residuals);
qqline(out.lm1$residuals)


library(itsmr)
season.avg = season(data, d=12); plot.ts(data);
title("US accidental deaths");
lines(x, season.avg + mean(data), col="red");
resi = data - season.avg - mean(data)


diff12 = diff(data, lag=12); par(mfrow=c(1,2))
plot.ts(data);
title("US accidental deaths");
plot(x[13:73], diff12, type="l", col="red");
title("Seasonal differencing");


out = classical(data, d=12, order=1);
par(mfrow=c(2,2));
plot.ts(data); title("step1");
lines(x, out$m1, col="red");
plot.ts(data-out$m1); title("step2");
lines(x, out$st, col="red");
plot.ts(data-out$st); title("step3");
lines(x, out$m, col="red");
plot.ts(data);
lines(x, out$fit, col="red");
title("Final");


library(itsmr); load("huron.Rdata");
test(out.lm$residuals)







