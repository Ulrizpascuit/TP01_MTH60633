library("here")
library("zoo")
library("xts")
library("PerformanceAnalytics")
library("compiler")
library("parallel")
library("pbapply")
library("Rcpp")


source(here("Function", "f_forecast_var.R"))


cpp_code <- '
  #include <Rcpp.h>
  using namespace Rcpp;

  // [[Rcpp::export]]
  NumericVector f_ht_cpp(NumericVector theta, NumericVector y) {
    double a0 = theta[0];
    double a1 = theta[1];
    double b1 = theta[2];

    int T = y.size();
    NumericVector sig2(T + 1);

    double denom = 1.0 - a1 - b1;
    if (denom <= 1e-12) denom = 1e-12;
    sig2[0] = a0 / denom;

    for (int i = 1; i < T + 1; i++) {
      double yi1 = y[i - 1];
      sig2[i] = a0 + a1 * yi1 * yi1 + b1 * sig2[i - 1];
    }
    return sig2;
  }
'
Rcpp::cppFunction(code = cpp_code)

f_ht <- function(theta, y) {
  y <- as.numeric(y)
  y <- y[is.finite(y)]
  f_ht_cpp(as.numeric(theta), y)
}

f_nll <- compiler::cmpfun(f_nll)
f_forecast_var <- compiler::cmpfun(f_forecast_var)
f_ht <- compiler::cmpfun(f_ht)  


load(here("Data", "processed", "prices_processed.rda"))

rets_ <- Return.calculate(prices = prices_processed, method = "log")
rets  <- rets_[-1, ]


window <- 1000
h <- 1000
level <- 0.95

stopifnot(NROW(rets) >= window + h)

n_assets <- ncol(rets)
dates_var <- index(rets)[(window + 1):(window + h)]

# Worker calcule un bloc k_idx pour la colonne j
roll_chunk <- function(rets, j, k_idx, window, level) {
  VaR_part <- numeric(length(k_idx))
  for (ii in seq_along(k_idx)) {
    k <- k_idx[ii]
    y_window <- rets[k:(window + k - 1), j]
    out <- f_forecast_var(y_window, level)
    VaR_part[ii] <- tail(out$VaR_Forecast, 1)
  }
  list(j = j, k_idx = k_idx, VaR = VaR_part)
}


ncores <- max(1, detectCores() - 1)
cl <- makeCluster(ncores)


clusterExport(cl, varlist = c(
  "rets","window","h","level","roll_chunk",
  "f_forecast_var","f_nll","f_ht",
  "cpp_code"
), envir = environment())


clusterEvalQ(cl, {
  library(Rcpp)
  library(compiler)
  library(xts)
  library(zoo)
  library(PerformanceAnalytics)
  

  cppFunction(code = cpp_code)
  

  f_ht <- function(theta, y) {
    y <- as.numeric(y)
    y <- y[is.finite(y)]
    f_ht_cpp(as.numeric(theta), y)
  }
  

  f_nll <- cmpfun(f_nll)
  f_forecast_var <- cmpfun(f_forecast_var)
})

k_chunks <- splitIndices(h, ncores)

tasks <- vector("list", length = n_assets * length(k_chunks))
t <- 1
for (j in 1:n_assets) {
  for (cc in seq_along(k_chunks)) {
    tasks[[t]] <- list(j = j, k_idx = k_chunks[[cc]])
    t <- t + 1
  }
}

VaR_roll <- matrix(NA_real_, nrow = h, ncol = n_assets)
colnames(VaR_roll) <- colnames(rets)

system.time({
  res_list <- pblapply(tasks, function(task) {
    roll_chunk(rets = rets, j = task$j, k_idx = task$k_idx,
               window = window, level = level)
  }, cl = cl)
})

stopCluster(cl)


for (res in res_list) {
  VaR_roll[res$k_idx, res$j] <- res$VaR
}

VaR_roll_xts <- xts(VaR_roll, order.by = dates_var)

plot(VaR_roll_xts,
     main = "Rolling VaR 95% (1-step ahead) - Parallel chunks + Rcpp",
     legend.loc = "bottomleft")
