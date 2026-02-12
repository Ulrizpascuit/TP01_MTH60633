
f_forecast_var <- function(y, level) {
  ### Compute the VaR forecast of a GARCH(1,1) model with Normal errors at the desired risk level
  #  INPUTS
  #   y     : [vector] (T x 1) of observations (log-returns)
  #   level : [scalar] risk level (e.g. 0.95 for a VaR at the 95# risk level)
  #  OUTPUTS
  #   VaR   : [scalar] VaR forecast 
  #   sig2  : [vector] (T+1 x 1) conditional variances
  #   theta : [vector] GARCH parameters
  #  NOTE
  #   o the estimation is done by maximum likelihood
  
  # Fit a GARCH(1,1) model with Normal errors
  # Starting values and bounds
  theta0 <- c(0.1 * var(y), 0.1, 0.8)
  #il n'y a que theta[1] qui doit etre strictement positif les autres peuvent être 0 
  LB     <- c(1e-5,1e-5,1e-5) 
  # Stationarity condition
  A <- matrix(c(1,0,0,0,1,0,0,0,1,0,-1,-1),nrow=4, byrow=TRUE) #matrice diag & contrainte theta[2] + theta [3] inf à 1-err
  b <- c(LB[1],LB[2],LB[3],-(1-1e-5) 
  # Run the optimization
  fit <- constrOptim(
  theta = theta0,
  f     = f_nll,
  y     = y,
  ui    = A,
  ci    = b
  )
  theta <- fit$par
  # Recompute the conditional variance
  sig2 <- ComputeHtGarch(theta, y)
  
  # Compute the next-day ahead VaR for the Normal model
  VaR <- -qnorm(1 - level) * sqrt(tail(sig2, 1)) 
  
  out <- list(VaR_Forecast = VaR, 
              ConditionalVariances = sig2, 
              GARCH_param = theta)
  
  out
}

f_nll <- function(theta, y) {
  ### Fonction which computes the negative log likelihood value 
  ### of a GARCH model with Normal errors
  #  INPUTS
  #   theta  : [vector] of parameters
  #   y      : [vector] (T x 1) of observations
  #  OUTPUTS
  #   nll    : [scalar] negative log likelihood value
  
  T <- length(y)
  
  # Compute the conditional variance of a GARCH(1,1) model
  sig2 <- ComputeHtGarch(theta, y)
  
  # Consider the T values
  sig2 <- sig2[1:T]
  
  # Compute the loglikelihood
  ll <- sum(dnorm(y, mean = 0, sd = sqrt(sig2), log = TRUE))
  
  # Output the negative value
  nll <- -ll
  
  nll
}

f_ht <- function(theta, y)  {
  ### Function which computes the vector of conditional variance
  #  INPUTS
  #   x0 : [vector] (3 x 1)
  #   y     : [vector] (T x 1) log-returns
  #  OUTPUTS 
  #   sig2  : [vector] (T+1 x 1) conditional variances
  
  # Extract the parameters
  a0 <- theta[1]
  a1 <- theta[2]
  b1 <- theta[3]
  
  T <- length(y)
  
  # Initialize the conditional variances
  sig2 <- rep(NA, T + 1)
  
  # Start with unconditional variances
  sig2[1] <- a0 / (1 - a1 - b1)
  
  # Compute conditional variance at each step
  #choc d'hier*coef de réaction + persistance*coef de mémoire
  for (t in 2:(T + 1)) {
  sig2[t] <- a0 + a1 * y[t-1]^2 + b1 * sig2[t-1]
  }
  sig2
}
