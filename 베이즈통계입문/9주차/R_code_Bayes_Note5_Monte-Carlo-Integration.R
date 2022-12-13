#-------------------------------------------------------#
#                 Monte Carlo Integration                 #
#-------------------------------------------------------#
nsim <-10000; u <-runif(nsim);
# function to be integrated
mci.ex <- function(x){
(cos(50*x)+sin(20*x))^2
}
plot(function(x) mci.ex(x), xlim=c(0,1),ylim=c(0,4))
# Monte Carlo sum
sum(mci.ex(u))/nsim

#-------------------------------------------------------#
#        Importance Sampling (Example 5.1)              #
#-------------------------------------------------------#

m=10000; U=runif(m); 
Y=exp(U^2);
mean(Y) # importance using uniform(0,1)
e=exp(1); X=log(1+(e-1)*U);
T=(e-1)*exp(X^2-X);
mean(T) # importance using e^x/(e-1)


#-------------------------------------------------------#
#        Importance Sampling (Example 5.2)              #
#-------------------------------------------------------#

m = 10000; U = matrix(runif(2*m),ncol=2); 
T = exp((U[,1]+U[,2])^2);
mean(T) # importance using uniform(0,1)*uniform(0,1)
e = exp(1); X = log(1+(e-1)*U);
T = (e-1)^2*exp((X[,1]+X[,2])^2-(X[,1]+X[,2]));
mean(T) # importance e^{x_1+x_2}/(e-1)^2


#-------------------------------------------------------#
#        Cauchy sampling distribution & P(theta)=1      #
#-------------------------------------------------------#

# Plotting of p(theta|y) and h(theta)
theta <- seq(0, 10, length=1000)
fn <- function(theta) {
      1/( (1+(theta-4.0)^2)*(1+(theta-5.5)^2)*(1+(theta-7.5)^2)*
          (1+(theta-4.5)^2)*(1+(theta-3.0)^2) )
}

denom <- integrate(fn, 0, Inf)
posterior <- function(theta) fn(theta)/as.numeric(denom[1])
plot(theta, posterior(theta), type="l", ylim=c(0,0.8), ylab="")
lines(theta, dnorm(theta, 4.45, sqrt(0.38)), lty=3)
text(4.45,0.67,"h(theta)")
text(6.5,0.2,"p(theta | y)")

# Plotting of w(theta)
theta <- seq(0, 10, length=1000)
w <- function(theta) {
  (sqrt(0.76*pi)*exp((theta-4.45)^2/0.76))/(pi^5*(1+(theta-4)^2)*(1+(theta-5.5)^2)*(1+(theta-7.5)^2)*(1+(theta-4.5)^2)*(1+(theta-3)^2))
}
plot(theta, w(theta))



#-------------------------------------------------------#
#               Gibbs sampling                          #
#          Robert and Casella, 10.17                    #
#          y_i=number of failures                       #
#          t_i=time                                     #
#-------------------------------------------------------#

y <- c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22)
t <- c(94, 16, 63, 126, 5, 31, 1, 1, 2, 10)
rbind(y, t)

# Function for Gibbs sampling
gibbs <- function(n.sims, beta.start, alpha, gamma, delta, y, t, burnin =0, thin = 1) {

 beta.draws <- c()
 lambda.draws <- matrix(NA, nrow = n.sims-burnin, ncol = length(y))
 beta.cur <- beta.start

 lambda.update <- function(alpha, beta, y, t) {
  rgamma(length(y), y + alpha, t + beta)
 }

 beta.update <- function(alpha, gamma, delta, lambda, y) {
  rgamma(1, length(y) * alpha + gamma, delta + sum(lambda))
 }

 for (i in 1:n.sims) {
  lambda.cur <- lambda.update(alpha = alpha, beta = beta.cur, y = y, t = t)
  beta.cur <- beta.update(alpha = alpha, gamma = gamma, delta = delta, 
                          lambda = lambda.cur, y = y)
  if (i > burnin & (i - burnin)%%thin == 0) {
   lambda.draws[(i - burnin)/thin, ] <- lambda.cur
   beta.draws[(i - burnin)/thin] <- beta.cur
  }
 }

 return(list(lambda.draws = lambda.draws, beta.draws = beta.draws))
}

# Starting value,1]s for beta
beta.cur <- 1

# Function of full conditional for lambda
lambda.update <- function(alpha, beta, y, t) {
 rgamma(length(y), y + alpha, t + beta)
}

# Function of full conditional for beta
beta.update <- function(alpha, gamma, delta, lambda, y) {
 rgamma(1, length(y) * alpha + gamma, delta + sum(lambda))
}

# Draw lambda from its full conditional
# Draw beta from its full conditional
posterior <- gibbs(n.sims = 10000, beta.start = 200, alpha = 1.8,
 gamma = 0.01, delta = 1, y = y, t = t)

# Posterior means and standard deviations for lambda
colMeans(posterior$lambda.draws)

apply(posterior$lambda.draws, 2, mean)
apply(posterior$lambda.draws, 2, sd)

# Posterior means and standard deviations for beta
mean(posterior$beta.draws)
sd(posterior$beta.draws)


####################################################
#####             Flu shot data
####################################################
library(TeachingDemos)

flu.data <- c(1,1,1,1,1,0,1,1,1,1,0,1,1,0,1,1,0,0,1)
# data for the observed 19 individuals

my.y <- sum(flu.data)  # count of all "successes"

tot.draws <- 10000 # number of sampled values from each full conditional

# Initial values for quantities of interest:
X.20.init <- 1
theta.init <- 0.5

# Dummy Vectors that will hold values for samples of interest:
X.20.vec <- c(X.20.init, rep(NULL, times = tot.draws) )
theta.vec <- c(theta.init, rep(NULL, times = tot.draws) )

for (j in 2:(tot.draws+1) ) {
 theta.vec[j] <- rbeta(n=1, my.y + X.20.vec[j-1] + 1, 20 - my.y - X.20.vec[j-1] + 1)
 X.20.vec[j] <- rbinom(n=1, size=1, prob=theta.vec[j])
}

# Remove initial values:
theta.vec <- theta.vec[-1]
X.20.vec <- X.20.vec[-1]

# remove first 2000 sampled values as "burn-in":

theta.post <- theta.vec[-(1:2000)]
X.20.post <- X.20.vec[-(1:2000)]

## Posterior summary for theta:

plot(density(theta.post))  # plot of estimated posterior for theta

mean(theta.post)  # Posterior mean of the other 8000 sampled theta values

median(theta.post)  # Posterior median of the other 8000 sampled theta values

quantile(theta.post, probs=c(0.025,0.975) )  # approximate 95% quantile-based interval for theta

emp.hpd(theta.post, conf=0.95)  # approximate 95% HPD interval for theta

## NOTE:  The observed-data MLE for theta here is:
mean(flu.data)
# which is biased in this case because it lacks the missing value.

## Posterior summary for the missing data value X.20:

mean(X.20.post)  # Posterior mean for X.20

var(X.20.post) # Posterior variance for X.20



#------------------------------------------------------------#
#              Bivariate normal distribution                 #
#------------------------------------------------------------#

# initialize constants and parameters
N <- 5000     # length of chain
burn <- 1000            # burn-in length
X <- matrix(0,N,2)      # the chain, a bivariate sample
rho <- -.75             # correlation
mu1 <- 0
mu2 <- 2
sigma1 <- 1
sigma2 <- .5
s1 <- sqrt(1-rho^2)*sigma1
s2 <- sqrt(1-rho^2)*sigma2

#generate the chain
X[1,] <- c(mu1,mu2)            # initialize
for (i in 2:N) {
  x2 <- X[i-1,2]
  m1 <- mu1+rho*(x2-mu2)*sigma1/sigma2
  X[i,1] <- rnorm(1,m1,s1)
  x1 <- X[i,1]
  m2 <- mu2+rho*(x1-mu1)*sigma2/sigma1
  X[i,2] <- rnorm(1,m2,s2)
}

b <- burn+1
x <- X[b:N, ]


# compare sample statistics to parameters
colMeans(x)
cov(x)
cor(x)
plot(x, main="", cex=.5, xlab=bquote(X[1]),
     ylab=bquote(X[2]), ylim=range(x[,2]))

 
#-------------------------------------------------------#
#             X1,...,X_10 ~ N(theta,sigma^2)             #
#  theta ~ N(mu0,tau0^2);  sigma^2 ~ IGamma(a,b)  #
#-------------------------------------------------------#

x=c(6, 7, 9,10,12,15,18,19,20,21)
M=5000;  mu0=tau0=15;  a=b=3
n=length(x);  xbar=mean(x);  xvar=sig2=var(x)
theta=matrix(nr=M,nc=2);   
post.a=a+n/2
for(i in 1:M){
   post.mu=(sig2/n*mu0+tau0*xbar)/(sig2/n+tau0)
   post.var=1/(1/tau0+n/sig2)
   mu=rnorm(1,post.mu,sqrt(post.var))
   post.b=b+1/2*((n-1)*xvar+n*(xbar-mu)^2)
   sig2=1/rgamma(1,post.a,post.b)
   theta[i,]=c(mu,sig2)
}
hist(theta[,1],breaks=20,freq=F,col="gray90")
lines(density(theta[,1]),type='l',col='red',lwd=2)
hist(theta[,2],breaks=20,freq=F,col="gray90")
lines(density(theta[,2]),type='l',col='red',lwd=2)

#-------------------------------------------------------#
#   Metropolis algorithm to generate a Gamma(1.7,4.4)   #
#    distribution with a normal jumping distribution    #
#    with standard deviation of 2                       #
#-------------------------------------------------------#

library(TeachingDemos)
mh.gamma <- function(n.sims, start, burnin, cand.sd, shape, rate) {
 theta.cur <- start
 draws <- c()
 theta.update <- function(theta.cur, shape, rate) {
 theta.can <- rnorm(1, mean = theta.cur, sd = cand.sd)
 accept.prob <- dgamma(theta.can, shape = shape, rate = rate)/
                dgamma(theta.cur, shape = shape, rate = rate)
 if (runif(1) <= accept.prob)
 theta.can
 else theta.cur
 }
 for (i in 1:n.sims) {
 draws[i] <- theta.cur <- theta.update(theta.cur, shape = shape,
 rate = rate)
 }
 return(draws[(burnin + 1):n.sims])
}

# random numbers
mh.draws <- mh.gamma(100000, start = 1, 
   burnin = 1000, cand.sd = 2, shape = 1.7, rate = 4.4)



plot(density(mh.draws))  # plot of estimated posterior for theta
mean(mh.draws)  # Posterior mean 
median(mh.draws)  # Posterior median 
quantile(mh.draws, probs=c(0.025,0.975) )  # approximate 95% quantile-based interval for theta
emp.hpd(mh.draws, conf=0.95)  # approximate 95% HPD interval for theta



#-------------------------------------------------------#
#       Metropolis algorithm to generate a t_\nu        #
#    distribution with a normal jumping distribution    #
#    with various standard deviation                    #
#-------------------------------------------------------#

rw.Metropolis <- function(n,sigma,x0,N){
 x <- numeric(N)
 x[1] <- x0
 u <- runif(N)
 k <- 0
 for (i in 2:N) {
  y <- rnorm(1, x[i-1], sigma)
  if(u[i] <= (dt(y,n)/dt(x[i-1],n))){
    x[i] <- y
  }else{
    x[i] <- x[i-1]
    k <- k+1
  }
 }
 return(list(x=x, k=k))
}

n <- 4    # degrees of freedom for target Student t dist.
N <- 2000
sigma <- c(.05,.5,2,16)

x0 <- 25
rw1 <- rw.Metropolis(n, sigma[1], x0, N)
rw2 <- rw.Metropolis(n, sigma[2], x0, N)
rw3 <- rw.Metropolis(n, sigma[3], x0, N)
rw4 <- rw.Metropolis(n, sigma[4], x0, N)

# number of candidate points rejected
print(c(rw1$k, rw2$k, rw3$k, rw4$k)/N)

par(mfrow=c(2,2))
plot(rw1[[1]])
plot(rw2[[1]])
plot(rw3[[1]])
plot(rw4[[1]])


#-------------------------------------------------------#
#   t_\nu-distribution with scale mixture of normals    #
#-------------------------------------------------------#

library(MCMCpack)
m=10000
nu <-3

w <-rinvgamma(m,shape=nu/2,scale=nu/2)
x1 <-rnorm(m,mean=0,sd=sqrt(w))
mean(x1)
var(x1)
nu/(nu-2) # variance(X), X~t_nu 

x2 <-rt(m,df=nu)
mean(x2)
var(x2)




 