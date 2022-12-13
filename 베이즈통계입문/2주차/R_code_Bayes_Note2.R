# Note 2

# Binomial-Beta 
xa=c(0,1,2,3,4,5,6,7,8,9,10,11) # number of trials
ya=c(0,0,0,1,1,1,2,2,2,3,3,3)  # number of successes
xx=seq(0,1,0.01)
par(mfrow=c(4,3))
for(i in 1:12){
  yy=dbeta(xx,1+ya[i],1+xa[i]-ya[i])
  plot(xx,yy,type="h",ylim=c(0,3),
  main=c(paste(xa[i]," trials: ",ya[i]," heads")))
}


#-----------------------------------------------------------------------------------------#

# Example (IQ): N(\theta,\sigma^2) where \theta is unknown and \sigma^2 is knwon
# Y ~ N(\theta,10^2) : sampling distribution 
# \theta ~ N(100,225) : prior distribution
# \theta | y ~ N((400+9y)/13, 69.23) : posterior distribution

theta <- seq(30, 170, .5)
mu0 <- 100
tau0 <- 15
sigma <- 10
y <- 115
plot(theta, dnorm(115,theta,sigma), type="l", xlab="theta", ylab="", ylim=c(0, .05))
lines(theta, dnorm(theta, mu0, tau0), lty=3)
lines(theta, dnorm(theta, (400+9*y)/13, sqrt(69.23)), lty=2)
text(80, .02, "prior")
text(95, .04, "posterior")
text(135, .03, "Likelihood")

library(LearnBayes)
probs <-c(1.0)
normal.par1 <-c(mu0,tau0^2)  # Hyperparameters
normalpar <-rbind(normal.par1) 
data <-c(y,sigma^2)          # Sampling distribution
normal.normal.mix(probs,normalpar,data)


#-------------------------------------------------------------------------------------------#

# Example (strength of strings): N(\theta,\sigma^2) where \theta is known and \sigma^2 is unknwon
# Note 2
# Y_1,...,Y_20 iid N(50,\sigma^2) : sampling distribution
# \sigma^2 ~ IGamma(a/2.b/2) : prior distribution
# \sigma^2 | y_1,...,y_20 ~ IG((n+a)/2, (\sum_{i=1}^n (y_i-50)^2+b)/2) 
 
library(MCMCpack)
a<-5
b<-100
y<-c(46,58,40,47,47,53,43,48,50,55,49,50,52,56,49,54,51,50,52,50)
n<-20
sigma2<-seq(0,100,.1)
theta<-50
a1<-(n+a)/2
b1<-((n-1)*var(y)+b)/2
plot(sigma2,dinvgamma(sigma2,(n-2)/2,b1-b/2),type="l",xlab="sigma^2",ylab="",ylim=c(0,.1))
lines(sigma2,dinvgamma(sigma2,a/2,b/2),lty=3)
lines(sigma2,dinvgamma(sigma2,a1,b1),lty=2)
text(4,.02,"prior")
text(6,.07,"posterior")
text(46,.02,"likelihood")


#-----------------------------------------------------------------------------------------------#

# Example (heart plant failure)
# Note 2
# Y_1,...,Y_n ~ Poisson(\lambda) : sampling distributon
# \lambda ~ Gamma(16,15174) : prior distribution
# \lambda | y_1,...,y_66 ~ Gamma(\sum_{i=1}^n y_i+a,n+b) where n=66, \sum_{i=1}^n y_i=1

alpha<-16
beta<-15174
yobs<-1   # \sum_{i=1}^n y_i=1
n<-66
lambda<-seq(0,0.0025,length=1000)
plot(lambda,dgamma(lambda,shape=alpha,rate=beta),type="l",xlab="lambda",ylab="")
lines(lambda,dgamma(lambda,shape=alpha+yobs,rate=beta+n),lty=2)
text(0.00045,1000,"prior")
text(0.0018,500,"posterior")

