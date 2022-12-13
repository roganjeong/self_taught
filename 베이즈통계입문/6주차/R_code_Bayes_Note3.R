#-----------------------------------#
#   Example 2  in Point Estimation  #
#-----------------------------------#

y <-5

# Joint distribution of theta and y
fn <-function(a){exp(-(y-a))/(pi*(1+a^2))}

# Marginal distribution of y
py <-integrate(fn,-Inf,y)
as.numeric(py[1])

# Posterior distribution of theta given y
theta <- seq(-10,5,length=110)
posterior <-exp(-(y-theta))/(pi*(1+theta^2)*as.numeric(py[1]))

# Posterior mean for theta
fn1 <- function(a){a*exp(a)/(1+a^2)}
fn2 <- function(a){exp(a)/(1+a^2)}
val1 <- integrate(fn1,-Inf,y)
val2 <- integrate(fn2,-Inf,y)
pmean <- as.numeric(val1[1])/as.numeric(val2[1])
pmean


#-----------------------------------#
#   Example 2  in Credible Sets     #
#-----------------------------------#

p <-0.95
fn1 <-function(a){
 y <-c(4.0,5.5,7.5,4.5,3.0)
 f <-1
 for(i in 1:5){
  f<-f*1/(1+(y[i]-a)^2)
  print(c(i,f))
 } 
 return(f)
}

# Calculate posterior distribution
val <- integrate(fn1,0,Inf)  # normalizing constant
posta <- function(a){fn1(a)/as.numeric(val[1])} # posterior distribution

theta <- seq(0,10,length=300)
posterior <- posta(theta)
plot(theta,posterior,type="l")

# maximized value of the posterior dist.
md <- posterior[posterior==max(posterior)]
md

# find HPD credible interval using discretization
x <- theta
dx <- posterior
px <- dx/sum(dx)  # discretize the posterior density
pxs <- -sort(-px) # max to min
ct <- min(pxs[cumsum(pxs)<p]) # cumsum: cumulative sum
ct

x <- matrix(x)
px <- matrix(px)
xpx <- cbind(x,px)
list(hpdr=range(xpx[,1][xpx[,2]>=ct]),mode=md)


# find HPD credible interval using bisection method
fn1 <- function(a){
 y <- c(4.0,5.5,7.5,4.5,3.0)
 f <-1
 for (i in 1:5){
  f <- f*1/(1+(y[i]-a)^2)
 }
 return(f)
}

val <- integrate(fn1, 0, Inf)
p_density <- function(a){
   fn1(a)/as.numeric(val[1])
}

theta <- seq(0,10,0.1)
posterior <- p_density(theta)
plot(theta,posterior,type="l")

range(posterior)

k <- 0.06335
f <- function(a) {
   p_density(a) - k
}

bisection <- function(x0,x1,epsilon) {
   fx0 <- f(x0)
   fx1 <- f(x1)
   error <- abs(x1-x0)
   while(error > epsilon) {
      error <- error/2
      x2 <- (x0+x1)/2
      fx2 <- f(x2)
      if(fx0*fx2 < 0) {
         x1 <- x2
         fx1 <- fx2
      }
      else {
         x0 <- x2
         fx0 <- fx2
      }
   }
   return(x2)
}

epsil <- 10**-100
theta_1 <- bisection(0,4.5,epsil)
theta_2 <- bisection(4.5,10,epsil)
theta_1
theta_2
result <- integrate(p_density,theta_1,theta_2)
as.numeric(result[1])
 

#-----------------------------------#
#           Bayes Factor            #
#-----------------------------------#

library(LearnBayes)
prior.par=c(100,15)  # theta ~ N(100,15^2)
data=c(115,1,10)  # y=115 |theta ~ N(theta,10^2)

# Compute a Bayesian test of the hypothesis that a normal mean is less than or equal to a specified value
mnormt.onesided(100,prior.par,data)  




###############################################
# Comparing Two Means:  Bayes Factor Approach #
###############################################

# Data for blood pressure reduction on 21 subjects
# (10 took calcium supplements, 11 took placebo):
calcium <- c(7,-4,18,17,-3,-5,1,10,11,-2)
placebo <- c(-1,12,-1,-3,3,-5,5,2,-11,-1,-3)

x1 <- calcium
x2 <- placebo

xbar.1 <- mean(x1); xbar.2 <- mean(x2);
s2.1 <- var(x1); s2.2 <- var(x2);
n1 <- length(x1);  n2 <- length(x2);

s2.p <- ((n1-1)*s2.1 + (n2-1)*s2.2)/(n1+n2-2)
n.alt <-   1 / (1/n1 + 1/n2)

# Usual two-sample t-statistic:
t.star <- (xbar.1 - xbar.2) / sqrt(s2.p/n.alt)

# prior mean and variance for Delta = |mu1 - mu2|/sigma:

mean.Delta <- 0  
var.Delta <- 1/9 ## "objective" prior information

# mean.Delta <- 0.5  
# var.Delta <- 1/9 ## stronger prior belief that the two population means differ

my.pv <- sqrt(1 + n.alt * var.Delta) # scale parameter for noncentral-t
my.ncp <- mean.Delta * sqrt(n.alt) / my.pv # noncentrality parameter for noncentral-t

# Calculating Bayes Factor:

B <- dt(t.star, df = n1+n2-2, ncp = 0) / ( dt(t.star/my.pv, df = n1+n2-2, ncp = my.ncp)/my.pv )

print(B)

# Probability that H_0 is true, given equal prior probabilities for H_0 and H_a:

1 / (1 + (1/B))


