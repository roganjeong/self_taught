# 1 - b

r = rep(0, 10000)
y = rep(0, 10000)

for (i in 10000){
  r[i] = rgamma(1 , shape = 4/2, scale = 2/4)
  y[i] = rnorm(1, mean = 0, sd = (sqrt(r[i])^(-1)))
}
mean(y) # the sample mean is approximately equal to the population mean

# 1 - c 
#


# 3 - a
accuracy = rep(NA, 100000)
for (n in 1:100000){
  z = rnorm(n, mean = 0, sd = 1)
  indicator = rep(0, n)
  for (i in 1:n){
    if (z[i] > 2.5){
      indicator[i] = 1
    }
  }
  mean = sum(indicator[indicator == 1])/length(indicator)
  variance = mean*(1-mean)/(n)
  accuracy[i] = 1.96*sqrt(variance) - (-1.96)*sqrt(variance)
}
accuracy = accuracy[1000:100000]
accuracy[accuracy < 0.001][1]

# 3 - b

accuracy = rep(NA, 100000)
for (n in 1:100000){
  z = dgamma(n, shape = 1, rate = 1)
  indicator = rep(0, n)
  for (i in 1:n){
    if (z[i] > 5.3){
      indicator[i] = 1
    }
  }
  mean = sum(indicator[indicator == 1])/length(indicator)
  variance = mean*(1-mean)/(n)
  accuracy[i] = qgamma(0.9975, shape = 1, scale = 1)*sqrt(variance) - (qgamma(0.0025, shape = 1, scale = 1))*sqrt(variance)
}
accuracy = accuracy[1000:100000]
accuracy[accuracy < 0.001][1]