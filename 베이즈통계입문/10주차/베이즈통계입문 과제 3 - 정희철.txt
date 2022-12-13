# 1
library(tidyverse)
library(MCMCpack)
data <- read.table("coal_data.txt", header = FALSE)

# given that k = 40
data_theta <- data[1:40,2]
y_theta <- as.vector(data_theta)
# Gibbs Sampling - theta #
gibbs_theta <- function(n, beta_init, a1, c1, d1, y1, k, burnin =0, thin = 1) {
  
  beta_samples <- c()
  theta_samples <- matrix(NA, nrow = n-burnin, ncol = length(y1))
  beta_cur <- beta_init
  
  theta_update <- function(a1, beta, y1, k) {
    rgamma(length(y1), sum(y1) + a1, 112 - k + 1/beta)
  }
  
  beta_update <- function(a1, c1, theta, d1) {
    rgamma(1, a1 + c1, theta + 1/d1)
  }
  
  for (i in 1:n) {
    theta_cur <- theta_update(a1 = a1, beta = beta_cur, y1 = y1, k = k)
    beta_cur <- beta_update(a1 = a1, c1 = c1, 
                            theta = theta_cur, d1 = d1)
    if (i > burnin & (i - burnin)%%thin == 0) {
      theta_samples[(i - burnin)/thin, ] <- theta_cur
      beta_samples[(i - burnin)/thin] <- beta_cur
    }
  }
  
  return(list(theta_samples = theta_samples, beta_samples = beta_samples))
}
## 

# Calculation from the samples
post_dist <- gibbs_theta(n = 10000, beta_init = 1, a1 = 0.5, c1 = 1, d1 = 1, y1 = y_theta, k = 40, burnin = 1000, thin = 1)

post_theta <- c(mean(post_dist$beta_samples), var(post_dist$beta_samples))



# given that k = 40
data_lambda <- data[41:112,2]
y_lambda <- as.vector(data_lambda)

# Gibbs Sampling - Lambda

gibbs_lambda <- function(n, beta_init, a2, c2, d2, y2, k, burnin =0, thin = 1) {
  
  beta_samples <- c()
  lambda_samples <- matrix(NA, nrow = n-burnin, ncol = length(y2))
  beta_cur <- beta_init
  
  lambda_update <- function(a2, beta, y2, k) {
    rgamma(length(y2), sum(y2) + a2, 112 - k + 1/beta)
  }
  
  beta_update <- function(a2, c2, lambda, d2) {
    rgamma(1, a2 + c2, lambda + 1/d2)
  }
  
  for (i in 1:n) {
    lambda_cur <- lambda_update(a2 = a2, beta = beta_cur, y2 = y2, k = k)
    beta_cur <- beta_update(a2 = a2, c2 = c2, 
                            lambda = lambda_cur, d2 = d2)
    if (i > burnin & (i - burnin)%%thin == 0) {
      lambda_samples[(i - burnin)/thin, ] <- lambda_cur
      beta_samples[(i - burnin)/thin] <- beta_cur
    }
  }
  
  return(list(lambda_samples = lambda_samples, beta_samples = beta_samples))
}
## 

# Calculation from the samples
post_dist <- gibbs_lambda(n = 10000, beta_init = 1, a2 = 0.5, c2 = 1, d2 = 1, y2 = y_lambda, k = 40, burnin = 1000, thin = 1)

post_lambda <- c(mean(post_dist$beta_samples), var(post_dist$beta_samples))



# Final Output
(post_theta)
(post_lambda)

(R = post_theta[1]/post_lambda[1])

####################################################################################################

# 2 

# 2 - a
y = rgamma(100, shape = 5, rate = 5)
y_sample_mean = mean(y) # 1.015347
y_sample_var = var(y) # 0.2325561
# the expected value of the gamma distribution is shape/rate, which is in this case 1, and the expected variance is shape/(rate)^2, which is in this case 0.25.
# Both the sample mean and variance are very close to the expected mean and variance of the gamma distribution.

# 2 - b
gibbs <- function(n, alpha_init, beta_init, y, burnin =0, thin = 1) {
  
  alpha_samples <- c()
  beta_samples <- c()
  grid_samples <- matrix(NA, nrow = n-burnin, ncol = length(y))
  alpha_cur <- alpha_init
  beta_cur <- beta_init
  
  alpha_update <- function(y) {
    rgamma(length(y), sum(y) + 1, 100 + 1/(0.01))
  }
  
  beta_update <- function(y) {
    rgamma(length(y), sum(y) + 1, 100 + 1/(0.01))
  }
  
  for (i in 1:n) {
    alpha_cur <- alpha_update(y=y)
    beta_cur <- beta_update(y=y)
    if (i > burnin & (i - burnin)%%thin == 0) {
      alpha_samples[(i - burnin)/thin] <- alpha_cur
      beta_samples[(i - burnin)/thin] <- beta_cur
    }
  }
  
  return(list(alpha_samples = alpha_samples, beta_samples = beta_samples))
}
post_dist <- gibbs(n = 10000, alpha_init = 5, beta_init = 5, y = y, burnin =0, thin = 1)

sum(post_dist$alpha_samples < 5) # 10000
sum(post_dist$beta_samples < 5) # 10000

# 2 - c

# 2 - d
gibbs <- function(n, alpha_init, beta_init, y, burnin =0, thin = 1) {
  
  alpha_samples <- c()
  beta_samples <- c()
  grid_samples <- matrix(NA, nrow = n-burnin, ncol = length(y))
  alpha_cur <- alpha_init
  beta_cur <- beta_init
  
  alpha_update <- function(y) {
    rep(5, 100)
  }
  
  beta_update <- function(y) {
    rgamma(length(y), sum(y) + 1, 100 + 1/(0.01))
  }
  
  for (i in 1:n) {
    alpha_cur <- alpha_update(y=y)
    beta_cur <- beta_update(y=y)
    if (i > burnin & (i - burnin)%%thin == 0) {
      alpha_samples[(i - burnin)/thin] <- alpha_cur
      beta_samples[(i - burnin)/thin] <- beta_cur
    }
  }
  
  return(list(alpha_samples = alpha_samples, beta_samples = beta_samples))
}
post_dist <- gibbs(n = 1000, alpha_init = 5, beta_init = 5, y = y, burnin =0, thin = 1)

post_beta = post_dist$beta_samples[post_dist$beta_samples < 5] # 10000
mean(post_beta)
var(post_beta)

# 3 - e 
# 3 - f









