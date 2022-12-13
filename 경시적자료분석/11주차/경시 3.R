data = read.csv('fish copy.csv')$count
sum((data - mean(data))^2/(mean(data)))
qchisq(0.975, 69)
