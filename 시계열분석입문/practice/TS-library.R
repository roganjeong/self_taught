

library(itsmr)
################################################
# ACF function
################################################


acf2 <- function(data, lag){
if(missing(lag)){ lag = 35;}

    thr=qnorm(1-.05/2, mean=0, sd=1/sqrt(length(data)));

    a1 = acf(data, lag, plot = FALSE);
    x = seq(1, lag, by=1);
    y = a1$acf[-1];
    plot(x, y, type="h", xlab = "Lag", ylab="ACF");
    abline(h=0, col = "black");
    abline(h = -thr, col="blue");
    abline(h = thr, col="blue");
    title("");
}


ma.cv <- function(h, Y, l)
{
  Y <- as.vector(Y)
  n <- length(Y)
  cv <- 0
  ind = 1:n;
  eps <- 1.0e-16

  for ( i in 1:n ){
   del = seq(max(i-l, 1), min(i+l, n), by=1);
   id = ind[-del];
   Z = Y[-del];
   tmp <- (id-i)/h
   s0 <- (abs(tmp)<=1)
   s1 <- Z*s0
   m <- sum(s1)/max(eps, sum(s0))
   cv <- cv+(Y[i]-m)^2;
}
  return(cv/n)
}


classical = function(data, d, order){
	n=length(data);
	# step 1
	q=ifelse(d%%2, (d-1)/2, d/2)
	x=c(rep(data[1], q), data, rep(data[n], q));
	if(d %%2 == 0){
	ff= c(.5, rep(1, 2*q-1), .5)/d;}
	if(d %%2 == 1){
	ff= rep(1, 2*q+1)/d;}
	xx = filter(x, ff, method = c("convolution"))
	mhat = na.omit(xx);
	mhat = as.numeric(mhat);
	# step 2
	z = data - as.numeric(mhat);
	st = season(z, d);
	# step 3 (regression)
	mnew = trend(data-st, order);
	# step 4 (residuals)
	fit = mnew + st;
	resi = data - fit;
return(list(fit=fit, st=st, m=mnew, resi=resi, m1=mhat))
}







