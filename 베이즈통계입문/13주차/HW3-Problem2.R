  par(mfrow=c(1, 1), oma = rep(0,4))

  ## you thought this was random? you thought wrong... this summer...
  set.seed(12348)
  ##set.seed(23423)
  
  message("PART A")
  ## draw the gammas
  Y <- rgamma(100, shape=5, rate=5)
  print(mean(Y))
  print(var(Y))
  cat("\nSHOULD have mean 1, and variance 1/5\n")

  message("PART B")
  ## make a GRIDSIZE x GRIDSIZE approximation
  GRIDSIZE <- 100
  ## most of the posterior falls in the range 1:9 (both alpha and beta)
  alphaGridPoints <- seq(1, 9, len=GRIDSIZE)
  betaGridPoints  <- seq(1, 9, len=GRIDSIZE)

  ## now do the calculations...
  posteriorRawGrid <- matrix(0, nr=GRIDSIZE, nc=GRIDSIZE)
  for(ii in 1:GRIDSIZE)
    for(jj in 1:GRIDSIZE)
      posteriorRawGrid[ii, jj] <- prod(dgamma(Y, shape=alphaGridPoints[ii], rate=betaGridPoints[jj])) *
        dgamma(alphaGridPoints[ii], shape=1, rate=0.01) *
          dgamma(betaGridPoints[jj], shape=1, rate=0.01)

  ## visualize, using image() (this was not required)
  image(posteriorRawGrid, main = "Grid Approximation of Posterior", xlab="alpha", ylab="beta", xaxt="n", yaxt="n")
  axis(1, at = seq(0, 1, len=6), labels = round(alphaGridPoints[floor(seq(1, GRIDSIZE, len=6))], 1))
  axis(2, at = seq(0, 1, len=6), labels = round(betaGridPoints[floor(seq(1, GRIDSIZE, len=6))], 1), las=2)
  if(savePlots) readline("ADJUST PLOT..."); dev.copy2pdf(file = "724-hw3-problem-3b.pdf")

  ## some simple calculations, again not required for anything
  probabilityAlphaLessThanFive <- sum(apply(posteriorRawGrid, 1, sum)[alphaGridPoints < 5]/sum(apply(posteriorRawGrid, 1, sum)))
  probabilityBetaLessThanFive <- sum(apply(posteriorRawGrid, 2, sum)[betaGridPoints < 5]/sum(apply(posteriorRawGrid, 2, sum)))

  ## now we normalize, and we sample
  posteriorNormedGrid <- posteriorRawGrid / sum(posteriorRawGrid)
  tenKSample <- sample(length(posteriorNormedGrid), size=10000, prob = c(posteriorNormedGrid), replace=TRUE)
  ## this function will decode an index of the grid into (alpha, beta) coordinates
  Decoder <- function(idx) c(alphaGridPoints[((idx-1) %% GRIDSIZE + 1)], betaGridPoints[ceiling(idx / GRIDSIZE)])

  ## decode the sample into alpha and beta
  tenKSampleDecoded <- t(sapply(tenKSample, Decoder))

  ## use these samples to calculate the probabilities each parameter does not exceed five
  probabilityAlphaLessThanFive <- mean(tenKSampleDecoded[,1] < 5)
  probabilityBetaLessThanFive  <- mean(tenKSampleDecoded[,2] < 5)

  ## spit it out!
  cat("\nP( ALPHA < 5) = ", probabilityAlphaLessThanFive)
  cat("\nP( BETA < 5) = ", probabilityBetaLessThanFive)
  cat("\n\n")

  message("PART C")
  normalizing <- 0
  ## a really dumb way to do this...
  alphaGridPointsSize <- mean(diff(alphaGridPoints))
  betaGridPointsSize  <- mean(diff(betaGridPoints))
  ## the totally unncessary retarded loop of dumbness
  ## should be: normalizing <- posteriorNormedGrid * alphaGridPointsSize * betaGridPointsSize
  for(ii in 1:100)
    for(jj in 1:100)
      normalizing <- normalizing + posteriorNormedGrid[ii, jj] * alphaGridPointsSize * betaGridPointsSize

  ## confess!
  cat("\nNormalizing constant: ", normalizing, "\n")

  message("PART D")
  ## reference homework 1, and we see that for fixed alpha, we have a
  ## gamma posterior with parameters (prior parameters A, B):
  ## alpha-star: n*alpha + A
  ## beta-star: sum y_i + B

  fixedAlpha <- 5
  alphaStar <- length(Y) * fixedAlpha + 1
  betaStar  <- sum(Y) + 0.01

  fixedAlphaPosteriorSample <- rgamma(1000, alphaStar, betaStar)
  cat("\nPOSTERIOR BETA MEAN = ", mean(fixedAlphaPosteriorSample))
  cat("\nPOSTERIOR BETA VARIANCE = ", var(fixedAlphaPosteriorSample))
  cat("\nP(BETA < 5 | alpha=5, Y) = ", mean(fixedAlphaPosteriorSample < 5), "\n")

  message("PART E")

  ## fix beta
  fixedBeta <- 5

  ## FixedBetaMetropolisSampler: metropolis sampler for alpha, given fixed beta
  ## fixedBeta: the value of beta
  ## nn: length of the chain
  ## Y: some data
  ## propSd: we use a normal proposal distribution, this is its standard deviation
  FixedBetaMetropolisSampler <- function(fixedBeta, nn, Y, propSd = sqrt(1 / 5)) {
    ## start somewhere for god's sake
    currentValue <- rgamma(1, 1, fixedBeta)
    ## here is the (un-normalized) target density
    TargetAlphaPosteriorBetaFixedUnNormed <- function(alpha, Y) {
      if(alpha > 0)
        return(prod(dgamma(Y, shape=alpha, rate=fixedBeta)) *
          dgamma(alpha, shape=1, rate=0.01))
      return(0) ## if alpha is zero, return zero so we can reject that impossible proposal
    }
    ## the chain
    chainStorage <- rep(0, nn)
    for( ii in 1:nn ){
      if( ii == 1000) cat("\n")
      if( ii %% 1000 == 0) cat(ii, " ")
      proposal <- rnorm(1, fixedBeta * mean(Y), propSd) ## here is the proposal distribution
      if(runif(1) < min(1, TargetAlphaPosteriorBetaFixedUnNormed(proposal, Y) / TargetAlphaPosteriorBetaFixedUnNormed(currentValue, Y)))
        currentValue <- proposal
      chainStorage[ii] <- currentValue
    }
    chainStorage
  }

  ## run that chain
  testMetroChain <- FixedBetaMetropolisSampler(fixedBeta, 5500, Y, sd(Y))

  ## plot it to check the health of it
  par(mfrow = c(2, 3), oma=c(0,0,4,0))
  plot(testMetroChain, type="l", main = paste("Trace plot"), xlab="Step", ylab="y")
  acf(testMetroChain, main = paste("Acf plot"))
  hist(testMetroChain, main = paste("Histogram"), xlab="y")

  ## thin and burn
  cat("\nVisually, I see we need to thin to ~ every 5th, and burn about 500 I guess (it does not seem to really matter) \n")
  burn <- 500
  thinning <- 5
  thinnedMetroChain <- testMetroChain[-(1:burn)]
  thinnedMetroChain <- thinnedMetroChain[seq(thinning, floor(length(thinnedMetroChain)/thinning) * thinning, by = thinning)]

  ## plot up the thinned and burned thing
  plot(thinnedMetroChain, type="l", main = paste("Trace plot (Thinned & Burned Chain)"), xlab="Step", ylab="y")
  acf(thinnedMetroChain, main = paste("Acf plot (Thinned & Burned Chain)"))
  hist(thinnedMetroChain, main = paste("Histogram (Thinned & Burned Chain)"), xlab="y")

  title(main = "Metropolis for gamma likelihood, exponential priors, beta fixed", outer=TRUE)
  if(savePlots) readline("ADJUST PLOT..."); dev.copy2pdf(file = "724-hw3-problem-3f.pdf")

  ## spit out some numbers who cares
  cat("\nPOSTERIOR ALPHA MEAN = ", mean(thinnedMetroChain))
  cat("\nPOSTERIOR ALPHA VARIANCE = ", var(thinnedMetroChain))
  cat("\nP(ALPHA < 5 | beta=5, Y) = ", mean(thinnedMetroChain < 5), "\n")

  message("PART F")
  
  ## now a metropolis sampler within a gibbs sampler

  ## MetropolisWithinGibbsGammaTwoParameter: metropolis within gibbs sampler for a two parameter gamma target
  ## nn: length of the chain
  ## Y: some data
  ## propSd: we use a normal proposal distribution (for alpha), this is its standard deviation
  MetropolisWithinGibbsGammaTwoParameter <- function(nn, Y, propSd = sqrt(1/5)) {
    ## the un-normalized target for the alpha step
    TargetAlphaPosteriorBetaFixedUnNormed <- function(alpha, Y, fixedBeta) {
      if(alpha > 0)
        return(prod(dgamma(Y, shape=alpha, rate=fixedBeta)) *
          dgamma(alpha, shape=1, rate=0.01))
      return(0)
    }
    ## staring value, chain storage
    currentValue <- rgamma(2, shape=sqrt(5), rate=1 / sqrt(5))
    chainStorage <- matrix(NA, nr=nn, nc=2)
    for(jj in 1:nn) {
      if( jj == 1000) cat("\n")
      if( jj %% 1000 == 0) cat(jj, " ")
      ## update beta, with a gibbs step, draw from the conjugate gamma posterior
      fixedAlpha <- currentValue[1]
      alphaStar  <- length(Y) * fixedAlpha + 1
      betaStar   <- sum(Y) + 0.01
      currentValue[2] <- rgamma(1, alphaStar, betaStar)
      ## update alpha, with a metropolis step
      proposal <- rnorm(1, currentValue[2] * mean(Y), propSd)
      if(runif(1) < min(1, TargetAlphaPosteriorBetaFixedUnNormed(proposal, Y, currentValue[2]) / TargetAlphaPosteriorBetaFixedUnNormed(currentValue[1], Y, currentValue[2])))
        currentValue[1] <- proposal
      chainStorage[jj, ] <- currentValue
    }
    chainStorage
  }

  ## run that thing, and plot it
  mwggg <- MetropolisWithinGibbsGammaTwoParameter(180500, Y, sqrt(Y))
  par(mfrow = c(2, 1))
  plot(mwggg[,1])
  plot(mwggg[,2])
  acf(mwggg[-(1:500), ])

  ## thin and burn
  cat("\nVisually, I see we need to remove ~ every 180th (suggests a bad proposal distribution), and burn about 500 I guess...\n")
  burn <- 500
  thinning <- 180
  thinnedMetroGibbsChain <- mwggg[-(1:burn), ]
  thinnedMetroGibbsChain <- thinnedMetroGibbsChain[seq(thinning, floor(nrow(thinnedMetroGibbsChain)/thinning) * thinning, by = thinning), ]

  ## now plot the glorious results
  par(mfrow = c(2, 3))
  plot(thinnedMetroGibbsChain[ ,1], type="l", main = paste("ALPHA: Trace plot (Thinned & Burned)"), xlab="Step", ylab="alpha")
  acf(thinnedMetroGibbsChain[, 1], main = paste("ALPHA: Acf plot (Thinned & Burned)"))
  hist(thinnedMetroGibbsChain[, 1], main = paste("ALPHA: Histogram (Thinned & Burned)"), xlab="alpha")
  plot(thinnedMetroGibbsChain[ ,2], type="l", main = paste("BETA: Trace plot (Thinned & Burned)"), xlab="Step", ylab="beta")
  acf(thinnedMetroGibbsChain[, 2], main = paste("BETA: Acf plot (Thinned & Burned)"))
  hist(thinnedMetroGibbsChain[, 2], main = paste("BETA: Histogram (Thinned & Burned)"), xlab="beta")
  title(main = "Gamma metropolis within gibbs results", outer=TRUE)

  if(savePlots) readline("ADJUST PLOT..."); dev.copy2pdf(file = "724-hw3-problem-3g.pdf")

  ## I compell you to print the results
  cat("\nPOSTERIOR ALPHA, BETA MEAN = ", apply(thinnedMetroGibbsChain, 2, mean))
  cat("\nPOSTERIOR ALPHA, BETA VARIANCE = ", apply(thinnedMetroGibbsChain, 2, var))
  cat("\nP(ALPHA < 5 | Y) = ", mean(thinnedMetroGibbsChain[, 1] < 5), "\n")
