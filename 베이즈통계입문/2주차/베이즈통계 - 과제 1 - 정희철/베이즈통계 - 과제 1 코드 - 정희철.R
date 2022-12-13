sd = 2 #baseline

sd.vec <- c(1/2, 2, 4) #different standard deviations for comparisons

#setting the values of thetas
theta_1=1
theta_2=2

x <- seq(-10,10,length.out = 10000)*sd + 1.5
y <- 0.5*dnorm(x,theta_1,sd.vec[1]) + 0.5*dnorm(x,theta_2,sd.vec[1])
com_y <- (dnorm(x,theta_1,sd.vec[1])*0.5)/y
plot(x,com_y,ylab = "density",type = "l",lty=1.5,lwd=2)
for (i in 2:3){
  y <- 0.5*dnorm(x,theta_1,sd.vec[i]) + 0.5*dnorm(x,theta_2,sd.vec[i])
  com_y <- (dnorm(x,theta_1,sd.vec[i])*0.5)/y
  lines(x,com_y,col=colors[i])
}
legend("bottomleft",col=c("black","red","blue"),lty=1,c("sd=0.5","sd=2","sd=4"),title = "SD's")
