
#############################################
# sparse VAR with regular Lasso
##############################################
library(glmnet)
library(vars)

sVAR.lasso = function(data, p, nf){
  
  if(missing(nf)){ nf = 10 };
  if(missing(p)){ p = 1 };
  
  y = data - rowMeans(data);
  T1 = dim(y)[2]-p;
  k = dim(y)[1];
  # create vector X1 and Y1
  X1 = matrix(0, k*p, T1); 
  Y1 = matrix(0, k, T1);
  for(j in 1:T1){
    # ar term
    id = seq(from= j+p-1, to = j, by=-1);
    x = as.vector(y[,id]);
    X1[,j] = x;
    Y1[,j] = y[,(j+p)];
  }
  ty = t(y); ty = data.frame(ty);
  out = VAR(ty, type="none", p=p);
  Sig= summary(out)$covres;
  sv=svd(Sig)
  hal = sv$u%*%diag(1/sqrt(sv$d))%*%t(sv$v)
  y1 = as.vector(Y1);
  x1 = kronecker(t(X1), diag(1, k));
  
  diff =100; iter=1;
  while( diff >= .01 & iter < 5){
    adjSig = kronecker(diag(1, T1), hal);
    Y2 = adjSig%*%y1;
    X2 = adjSig%*%x1;
    cvfit = cv.glmnet(X2, Y2, alpha = 1, intercept=TRUE, standardize=FALSE, type.measure = "mse", nfolds=nf);
    cf.cv = coef(cvfit, s = "lambda.min")
    cf.cv = cf.cv[-1];
    hA1 = matrix(cf.cv, nrow=k, byrow=FALSE);
    Signew= VAR.sigma(y, hA1)$Sigma_z;
    diff = sum((Sig - Signew)^2);
    Sig = Signew;
    iter = iter +1;
    sv=svd(Sig)
    hal = sv$u%*%diag(1/sqrt(sv$d))%*%t(sv$v)
  }
  
  out=list();
  out$cv= cvfit;
  out$hatA = hA1;
  out$lam = cvfit$lambda.min;
  out$Sigma_z = Sig;
  
  return(out);

}

###########################################
# sparse VAR with adaptivelasso
###########################################

sVAR.adaplasso = function(y, p, nf){ 
  if(missing(nf)){ nf=10};
  y = y - rowMeans(y);
  k = dim(y)[1];
  T = dim(y)[2];
  T1 = T-p;
  
  # create vector X1 and Y1
  X1 = matrix(0, k*p, T1); 
  Y1 = matrix(0, k, T1);
  for(j in 1:T1){
    # ar term
    id = seq(from= j+p-1, to = j, by=-1);
    x = as.vector(y[,id]);
    X1[,j] = x;
    Y1[,j] = y[,(j+p)];
  }
  
  y1 = as.vector(Y1);
  x1 = kronecker(t(X1), diag(1, k));
  
  hA0 = kronecker(solve(X1%*%t(X1))%*%X1, diag(k))%*%y1;
  hA0 = matrix(hA0, nrow=k);
  Sig = VAR.sigma(y, hA0)$Sigma_z;
  sv = svd(Sig)
  hal = sv$u%*%diag(1/sqrt(sv$d))%*%t(sv$v)
  
  hhA0 = hA0;
  diff =100; iter=1;
  while( diff >= .01 & iter < 5){
    
    adjSig = kronecker(diag(1, T1), hal);
    Y2 = adjSig%*%y1;
    X2 = adjSig%*%x1;
    
    pp = adalasso(X2, Y2, k=nf, intercept=FALSE);
    hA2 = matrix(pp$coefficients.lasso, nrow=k);
    hA1 = matrix(pp$coefficients.adalasso, nrow=k);
    diff = sum((hA1 - hA0)^2);
    iter = iter +1;
    hA0 = hA1;
    Sig= VAR.sigma(y, hA0)$Sigma_z;
    sv=svd(Sig)
    hal = sv$u%*%diag(1/sqrt(sv$d))%*%t(sv$v)
  }
  
  out=list();
  out$cv= pp$cv.adalasso;
  out$hatA = hA1;
  out$lam = pp$lambda.adalasso;
  out$Sigma_z = Sig;
  ## standard LASSO result
  out$lasA = hA2;

  return(out);
}

adalasso = function (X, y, k = 10, use.Gram = TRUE, both = TRUE, intercept = TRUE) {
  colnames(X) = 1:ncol(X)
  n <- length(y)
  cv.adalasso <- NULL
  globalfit <- mylars(X, y, k = k, use.Gram = use.Gram, normalize = TRUE, intercept = intercept) 
  coefficients.lasso = globalfit$coefficients
  intercept.lasso = globalfit$intercept 
  cv.lasso <- globalfit$cv.lasso
  lambda <- globalfit$lambda
  lambda.lasso <- globalfit$lambda.opt 
  coefficients.adalasso = NULL
  lambda.adalasso <- intercept.adalasso <- NULL 
  if (use.Gram == TRUE) {
    type = "covariance"
  }
  if (use.Gram == FALSE) { type = "naive"
  }
  if (both == TRUE) {
    ## Block CV
    all.folds <- split(1:n, rep(1:k, each = floor(n/k)+1)[1:n]) 
    residmat <- matrix(0, length(lambda), k) 
    for (i in seq(k)) {
      omit <- all.folds[[i]]
      Xtrain <- X[-omit, , drop = FALSE]
      ytrain <- y[-omit]
      Xtest <- X[omit, , drop = FALSE]
      ytest <- y[omit]
      my.lars <- mylars(Xtrain, ytrain, k = k, normalize = TRUE,
                        use.Gram = use.Gram, intercept = intercept) 
      coef.lasso <- my.lars$coefficients
      weights <- 1/abs(coef.lasso[abs(coef.lasso) > 0]) 
      if (length(weights) == 0) {
        residmat[, i] <- mean((mean(ytrain) - ytest)^2) 
      }
      if (length(weights) == 1) {
        residmat[, i] = mean((ytest - my.lars$intercept - Xtest %*% coef.lasso)^2)
      }
      if (length(weights) > 1) {
        XXtrain <- Xtrain[, names(weights), drop = FALSE]
        XXtest <- Xtest[, names(weights), drop = FALSE]
        XXtrain <- scale(XXtrain, center = FALSE, scale = weights) 
        XXtest <- scale(XXtest, center = FALSE, scale = weights) 
        fit <- glmnet(XXtrain, ytrain, type.gaussian = type, standardize = FALSE, intercept = intercept) 
        pred <- predict(fit, newx = XXtest, type = "response", s = lambda) 
        if (length(omit) == 1) {
          pred <- matrix(pred, nrow = 1) }
      residmat[, i] <- apply((ytest - pred)^2, 2, mean) }
    }
    cv <- apply(residmat, 1, mean)
    cv.adalasso <- min(cv)
    weights <- 1/abs(coefficients.lasso[abs(coefficients.lasso) > 0]) 
    coefficients.adalasso <- rep(0, ncol(X)) 
    names(coefficients.adalasso) <- 1:ncol(X)
    
    if (length(weights) > 0) {
      XX <- X[, names(weights), drop = FALSE] 
      if (length(weights) == 1)
        XX <- XX/weights
      else XX <- scale(XX, center = FALSE, scale = weights) 
      if (length(weights) <= 1) {
        intercept.adalasso = intercept.lasso
        coefficients.adalasso <- coefficients.lasso
        lambda.adalasso = 0
      }
      else {
        fit <- glmnet(XX, y, type.gaussian = type, standardize = FALSE, intercept = intercept)
        lambda.adalasso <- lambda[which.min(cv)] 
        coefficients = predict(fit, type = "coefficients", s = lambda.adalasso)
        intercept.adalasso <- coefficients[1]
        coefficients.adalasso[names(weights)] <- coefficients[-1]/weights 
        }
    } 
  }
  return(list(cv.lasso = cv.lasso, lambda.lasso = lambda.lasso,
              cv.adalasso = cv.adalasso, lambda.adalasso = lambda.adalasso, intercept.lasso = intercept.lasso, intercept.adalasso = intercept.adalasso, coefficients.lasso = coefficients.lasso, coefficients.adalasso = coefficients.adalasso))
}

        
        
mylars = function (X, y, k = 10, use.Gram = TRUE, normalize = TRUE, intercept = TRUE) {
  x <- X
  n <- length(y)
  all.folds <- split(sample(1:n), rep(1:k, length = n)) 
  if (use.Gram == TRUE) {
    type = "covariance"
  }
  if (use.Gram == FALSE) { type = "naive"
  }
  globalfit <- glmnet(x, y, family = "gaussian", standardize = normalize,
                      type.gaussian = type, intercept = intercept) 
  lambda <- globalfit$lambda
  residmat <- matrix(0, length(lambda), k) 
  for (i in seq(k)) {
    omit <- all.folds[[i]]
    fit <- glmnet(x[-omit, , drop = FALSE], y[-omit], type.gaussian = type,
                  standardize = normalize, family = "gaussian", intercept = intercept) 
    fit <- predict(fit, newx = x[omit, , drop = FALSE], type = "response",s = lambda) 
    if (length(omit) == 1)
    fit <- matrix(fit, nrow = 1)
    residmat[, i] <- apply((y[omit] - fit)^2, 2, mean)
  }
  cv <- apply(residmat, 1, mean)
  cv.lasso <- min(cv)
  cv.error <- sqrt(apply(residmat, 1, var)/k)
  lambda.opt <- lambda[which.min(cv)]
  coefficients = predict(globalfit, type = "coefficients", s = lambda.opt)
  inter = coefficients[1]
  coefficients = coefficients[-1]
  names(coefficients) = 1:ncol(X)
  object <- list(lambda = lambda, cv = cv, lambda.opt = lambda.opt,
                 cv.lasso = cv.lasso, intercept = inter, coefficients = coefficients) 
  invisible(object)
}


ar.adaplasso = function(y, p, nf){ if(missing(nf)){ nf = 10 };
  if(missing(p)){ phat = ar(y, aic = TRUE, order.max=10)$order }
  # Check y is a vector
  y = as.vector(y); n = length(y); mu.s = mean(y); id = 1:n;
  X = NULL;
  for(j in 1:p){
    id1 = id-j;
    id2 = id1[id1 <= 0];
    id3 = id1[id1 > 0];
    X = cbind(X, c(rep(mu.s, length(id2)), y[id3])); }
  pp = adalasso(X, y, k=nf, intercept=TRUE);
  return(pp) }

#############################################################
# Generating VAR(p) model
##############################################################

VAR.sim = function(T, A, Sigma){
  k = dim(A)[1];
  p = dim(A)[2]/k;
  burn = 500;
  
  inno = mvrnorm(n=T+burn, rep(0, k), Sigma);
  init = mvrnorm(n=p, rep(0, k), Sigma);
  init = matrix(init, nrow=p);
  
  # Find index for previous observations
  j=1;
  # ar term
  id = seq(from= j+p-1, to = j, by=-1);
  
  Y = matrix(0, (T+burn), k);
  for(r in 1:(T+burn)){
    Y[r,] = A%*%as.vector(t(init[id,])) + inno[r,];
    init = rbind(init[-1,], Y[r,]);
  }
  
  return(t(Y[-(1:burn),])) # Final data is k*T matrix
}

###########################################
# OLS estimation of VAR
###########################################

VAR.lse  = function(y, p){
  y = y - rowMeans(y);
  T = dim(y)[2];
  T1 = T-p;
  k = dim(y)[1];
  
  # create vector X1 and Y1
  X1 = matrix(0,k*p, T1); 
  Y1 = matrix(0,k, T1);
  for(j in 1:T1){
    # ar term
    id = seq(from= j+p-1, to = j, by=-1);
    x = as.vector(y[,id]);
    X1[,j] = x;
    Y1[,j] = y[,(j+p)];
  }
  
  hatA = Y1%*%t(X1)%*%solve(X1%*%t(X1));
  
  Resi = matrix(0,k, T1); 
  # residuals
  for(j in 1:T1){
    id = seq(from= j+p-1, to = j, by=-1);
    x = as.vector(y[,id]);
    Resi[,j] =  y[,p+j]  - hatA%*%x;
  }
  Sigma_z = Resi%*%t(Resi)/T1;
  bic =  T1*log(det(Sigma_z)) + log(T1)* sum(hatA != 0);
  return(list(hatA = hatA, Sigma=Sigma_z, bic=bic, p=p))
}

######################################
# Sigma estimation in VAR
######################################
VAR.sigma = function(y, hA1){
  k = dim(y)[1];
  T = dim(y)[2];
  p = dim(hA1)[2]/k;
  T1 = T - p;

  # Calculate residuals
  Resi = matrix(0, k, T1);
  for(j in 1:T1){
    # ar term
    id = seq(from= j+p-1, to = j, by=-1);
    Resi[,j] =  y[,p+j]  - hA1%*%as.vector(y[,id]);
  }
  
  # Estimate Sigma
  Sigma_z = Resi%*%t(Resi)/T;
  out = list();
  out$Resi = Resi;
  out$Sigma_z = Sigma_z;
  return(out);
}

##############################################################
# Forecasting VAR(p) model
##############################################################

VAR.forecast = function(yf, h, A){
  
  T1 = dim(yf)[2];
  k = dim(A)[1];
  p = dim(A)[2]/k;
  
  # Find index for previous observations
  id = seq(from= T1, to = T1-p+1, by=-1);
  
  Y1 = Y = matrix(0, k, h);
  for(r in 1:h){
    Y[,r] = A%*%as.vector(yf[,id]);
    yf = cbind(yf[,-1], Y[,r]);
    Y1[,r] = Y[,r];
  }
  
  return(Y1) # Final data is k*T matrix
}



