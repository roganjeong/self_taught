
rm(list=ls(all=TRUE))
source(file="sVAR-library.R")

library(glmnet)
library(vars)

A = matrix(c(
  0.8,0,0,0,0,0,
  0,0,0,.3,0,0,
  0,0,0,0,-.3,0,
  .6,0,0,0,0,0,
  0,0,.6,0,0,0,
  0,0,0,0,0,.8), ncol=6, nrow=6, byrow=TRUE);

#Sigma = diag(6)
d=1
Sigma=matrix( c(
  d^2,d/4,d/6,d/8,d/10,d/12,
  d/4,1,0,0,0,0,
  d/6,0,1,0,0,0,
  d/8,0,0,1,0,0,
  d/10,0,0,0,1,0,
  d/12,0,0,0,0,1
) , ncol=6, nrow=6, byrow=TRUE )

T=500; k = dim(A)[1];

## Simulate data; generated one is dim*length
data =VAR.sim(T, A, Sigma)

out = VAR(t(data), p=1, type="none")
out1 = sVAR.lasso(data, p=1);
out2 = sVAR.adaplasso(data, p=1);

Bcoef(out)
out1$hatA
out2$hatA
A

