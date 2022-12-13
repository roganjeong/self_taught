# 1-a
machineLife <- c(21,33,34,56,43,41,61,40,31,43,34,47,45,37,50,41,25,29,50,46,38,36,54,47)
#A <- as.factor(c(-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1))
#B <- as.factor(c(-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1))
#C <- as.factor(c(1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1))
#data <- data.frame(machineLife,A,B,C)
one <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
A <- c(-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
B <- c(-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1)
C <- c(1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1)
AB <- A*B
BC <- B*C
AC <- A*C
ABC <- A*B*C
(X <- cbind(one, A, B, C, AB, BC, AC, ABC))
betas <- (solve(t(X)%*%X)%*%t(X))%*%machineLife
factorEffects <- 2*betas

# 1-b
A <- as.factor(A)
B <- as.factor(B)
C <- as.factor(C)

data <- data.frame(machineLife, A, B, C)
model <- aov(data = data, machineLife ~ A + B + C + A:B + B:C + A:C + A:B:C)
summary(model)


# 1-d
lmod <- summary(lm(data = data, machineLife ~ B + C + A:C))
lmod$residuals
qqnorm(lmod$residuals)
qqline(lmod$residuals)
lmod$coefficients

cbind(machineLife, B, C, AC)
model.beta <- cbind(B, C, AC)
pred <- 40.917 + 5.75*model.beta[,2] -3.5*model.beta[,3] + 4.5*(model.beta[,1]*model.beta[,3])
res <- machineLife - pred
qqnorm(res)
qqline(res)
plot(res)

# 1-e
par(mfrow = c(1,3))
interaction.plot(A,B,machineLife)
interaction.plot(B,C,machineLife)
interaction.plot(A,C,machineLife)

# 2
rep1.new <- c(28.25,33,34,63.25,43,48.25,68.25,40)
rep1.new <- c(21,33,34,56,43,41,61,40)
one.new <- c(1,1,1,1,1,1,1,1)
A.new <- as.factor(c(-1,1,-1,1,-1,1,-1,1))
B.new <- as,factor(c(-1,-1,1,1,-1,-1,1,1))
C.new <- as.factor(c(1,1,1,1,-1,-1,-1,-1))
block <- as.factor(c(1,-1,-1,1,-1,1,1,-1))
AB.new <- as.factor(A.new*B.new)
BC.new <- as.factor(B.new*C.new)
AC.new <- as.factor(A.new*C.new)
ABC.new <- A.new*B.new*C.new
summary(aov(rep1.new ~ A.new:B.new:C.new + B.new + C.new + A.new:C.new ))

summary(aov(rep1.new ~ A.new + B.new + C.new + A.new:B.new + B.new:C.new + A.new:C.new))

# 3
blocks <- as.factor(c(1,-1,-1,1,-1,1,1,-1,1,-1,-1,1,-1,1,1,-1,1,-1,-1,1,-1,1,1,-1))
summary(aov(machineLife ~ A:B:C + B + C + A:C))
