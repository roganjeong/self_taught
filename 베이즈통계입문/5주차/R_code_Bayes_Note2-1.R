#----------------------------------------------------------------------------------------------#

# Description of Dirichlet distribution(alpha1,alpha2,alpha3)

library(compositions)
par(mfrow=c(2,2))
dd1 <- rDirichlet.acomp(100,alpha=c(A=1,B=1,C=1))
plot(dd1); title("Dirichlet Distribution: c(1,1,1)")

dd2 <- rDirichlet.acomp(100,alpha=c(A=7,B=7,C=7))
plot(dd2); title("Dirichlet Distribution: c(7,7,7)")

dd3 <- rDirichlet.acomp(100,alpha=c(A=9,B=1,C=1))
plot(dd3); title("Dirichlet Distribution: c(9,1,1)")

dd4 <- rDirichlet.acomp(100,alpha=c(A=1,B=5,C=9))
plot(dd4); title("Dirichlet Distribution: c(1,5,9)")
dev.off()

#----------------------------------------------------------------------------------------------#

# Example (Pre-election Polling): Poll.R
# Note 2-1

library(MCMCpack)
NN <- 1000
yy <- c(727, 583, 137)
alpha <- c(1,1,1)
theta <- rdirichlet(NN, alpha+yy)  # random number generation from Dirichlet dist
count <- (theta[,1]>theta[,2])
sum(count)
hist(theta[,1]-theta[,2], xlab="theta1-theta2", ylab="", yaxt="n",
     nclass=50, density=-1, main="")
sum(count)/NN

# Using MCmultinomdirichlet
library(MCMCpack)
NN <-1000
posterior <- MCmultinomdirichlet(c(727,583,137),c(1,1,1),mc=NN)
summary(posterior)
bush.dukakis.diff <- posterior[,1]-posterior[,2]
count <-(bush.dukakis.diff>0)
hist(bush.dukakis.diff, xlab="theta1-theta2", ylab="", yaxt="n",
     nclass=50, density=-1, main="")
sum(count)/NN



#----------------------------------------------------------------------------------------------#

# Example: logistic regression moel

# improper uniform prior: p(alpha,beta)=1

library(MCMCpack)
x <- rep(c(-0.863,-0.296,0.053,0.727),each=5)
y <- c(rep(0,5), 1, rep(0,4), rep(1,3), rep(0,2), rep(1,5))

cbind(x,y)
dd <-data.frame(cbind(x,y))
po <-MCMClogit(y~x,data=dd,burnin=1000, mcmc=10000,thin=1)
summary(po)

# user-defined independent Cauchy prior
logpriorfun <- function(beta){
  sum(dcauchy(beta,log=TRUE))
}
po.cauchy <-MCMClogit(y~x,data=dd,burnin=1000, mcmc=10000,thin=1,user.prior.density=logpriorfun,logfun=TRUE)
summary(po.cauchy)
