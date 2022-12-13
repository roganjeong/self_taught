#################
# Bayesian H.W1 #
#################

#### 1-(a) ####
 
y <- seq(-7,10,0.02)
dens <- 0.5*dnorm(y,1,2)+0.5*dnorm(y,2,2)
plot(y, dens, ylim=c(0,1.1*max(dens)), 
type="l",xlab="y", ylab="",xaxs="i",yaxs="i",yaxt="n",bty="n",cex=2)



#### 4-(a) ####
 
theta <- seq(0,1,0.001)
density <- dbeta(theta,1,0.67)
plot(theta,density,xlim=c(0,1),ylim=c(0,3),type="l",main="Posterior Density for θ",ylab="",yaxt="n",bty="n")

#### 4-(b) ####

theta <- seq(0,1,0.001)
density <- dbeta(theta,651,350.67)
cond <- density/max(density)>0.001
plot(theta[cond],density[cond],type="l",main="Posterior Density for θ",xlab="theta",ylab="",yaxt="n",bty="n")

