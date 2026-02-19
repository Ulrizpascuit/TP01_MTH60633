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


n_assets <- ncol(rets)
dates_var <- index(rets)[(window + 1):(window + h)]


roll_chunk <- function(rets, j, k_idx, window, level) {
  VaR_part <- numeric(length(k_idx))
  H_part   <- numeric(length(k_idx))
  for (ii in seq_along(k_idx)) {
    k <- k_idx[ii]
    y_window <- rets[k:(window + k - 1), j]
    out <- f_forecast_var(y_window, level)
    VaR_part[ii] <- tail(out$VaR_Forecast, 1)
    H_part[ii]   <- tail(out$ConditionalVariances, 1)
  }
  list(j = j, k_idx = k_idx, VaR = VaR_part, H = H_part)
}

#***************************** START PARALLELISME
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
  
  f_ht <- cmpfun(f_ht)
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
H_roll   <- matrix(NA_real_, nrow = h, ncol = n_assets)
colnames(VaR_roll) <- colnames(rets)
colnames(H_roll)   <- colnames(rets)

system.time({
  res_list <- pblapply(tasks, function(task) {
    roll_chunk(rets = rets, j = task$j, k_idx = task$k_idx,
               window = window, level = level)
  }, cl = cl)
})

stopCluster(cl)
#******************************* STOP PARALLELISME

for (res in res_list) {
  VaR_roll[res$k_idx, res$j] <- res$VaR
  H_roll[res$k_idx, res$j]   <- res$H
}

VaR_roll_xts <- xts(VaR_roll, order.by = dates_var)
H_roll_xts <- xts(H_roll, order.by = dates_var)
plot(VaR_roll_xts,
     main = "Rolling VaR 95% (1-step ahead) - Parallel chunks + Rcpp",
     legend.loc = "bottomleft")


level <- 0.95
p <- 1 - level
r_real <- rets[,1][index(VaR_roll_xts)]  
VaR_pred <- VaR_roll_xts[,1]
viol <- (r_real < VaR_pred)
phat <- mean(viol, na.rm=TRUE)
phat


dat <- merge(rets[, 1:2], VaR_roll_xts[, 1:2])


png(filename = "rets_vs_VaR_95_two_series.png",
    width = 1600, height = 900, res = 150)

par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

for (j in 1:2) {
  ret_j <- dat[, j]      
  var_j <- dat[, j + 2]  
  
  ylim_ <- range(c(ret_j, var_j), na.rm = TRUE)
  
  plot(index(dat), as.numeric(ret_j), type = "l",
       main = paste0("Rendements réalisés et VaR 95% — ", colnames(rets)[j]),
       xlab = "Date", ylab = "Rendement / VaR",
       ylim = ylim_)
  
  lines(index(dat), as.numeric(var_j), lwd = 2)
  
  legend("topright",
         legend = c("Rendements réalisés", "VaR 95% (quantile gauche)"),
         lwd = c(1, 2), bty = "n")
}

cat("PNG enregistré : rets_vs_VaR_95_two_series.png\n")



dat_all <- merge(rets[, 1:2], VaR_roll_xts[, 1:2], H_roll_xts[, 1:2])

z_xts <- merge(
  dat_all[,1] / sqrt(dat_all[,5]),
  dat_all[,2] / sqrt(dat_all[,6])
)

colnames(z_xts) <- paste0("z_", colnames(rets)[1:2])
par(mfrow=c(2,1))

plot(z_xts[,1], main="Innovations standardisées — Série 1",
     ylab="z_t", col="black")
abline(h=0, lty=2)

plot(z_xts[,2], main="Innovations standardisées — Série 2",
     ylab="z_t", col="black")
abline(h=0, lty=2)


par(mfrow=c(1,2))

hist(z_xts[,1], probability=TRUE, breaks=40,
     main="Histogramme Série 1")

curve(dnorm(x, mean=0, sd=1),
      add=TRUE, lwd=2)

hist(z_xts[,2], probability=TRUE, breaks=40,
     main="Histogramme Série 2")

curve(dnorm(x, mean=0, sd=1),
      add=TRUE, lwd=2)


z_clean <- na.omit(z_xts)
rho <- cor(z_clean[,1], z_clean[,2])
library("mvtnorm")

mu    <- c(0, 0)
Sigma     <- matrix(c(1, rho, rho, 1), 2, 2)

set.seed(1234)
n_sim <- 1000
innov <- mvtnorm::rmvnorm(n = n_sim, mean = mu, sigma = Sigma)
colnames(innov) <- paste0("z_sim_", colnames(rets)[1:2])
rho
cor(innov[,1], innov[,2])
sigma1_next <- as.numeric(tail(sqrt(H_roll_xts[,1]), 1))
sigma2_next <- as.numeric(tail(sqrt(H_roll_xts[,2]), 1))
rets_sim <- cbind(
  r_sim_1 = sigma1_next * innov[,1],
  r_sim_2 = sigma2_next * innov[,2]
)
colnames(rets_sim) <- paste0("r_sim_", colnames(rets)[1:2])
apply(rets_sim, 2, mean)
apply(rets_sim, 2, sd)
cor(rets_sim[,1], rets_sim[,2]) 
par(mfrow=c(1,2))
plot(innov[,1], innov[,2], pch=16, cex=0.6,
     main="Innovations simulées", xlab="z1", ylab="z2")
plot(rets_sim[,1], rets_sim[,2], pch=16, cex=0.6,
     main="Rentabilités simulées", xlab="r1", ylab="r2")

w <- c(0.5, 0.5)

rp_sim <- as.numeric(rets_sim %*% w)

head(rp_sim)
length(rp_sim)

k_last <- h
idx_train <- k_last:(k_last + window - 1)  

w <- c(0.5, 0.5)

rp_train <- as.numeric(rets[idx_train, 1:2] %*% w)
rp_train_xts <- xts(rp_train, order.by = index(rets)[idx_train])
rp_train_last10 <- tail(rp_train_xts, 10)

rp_sim <- as.numeric(rets_sim %*% w)

t1_date <- tail(index(H_roll_xts), 1)

ylim_ <- range(c(rp_train_last10, rp_sim), na.rm = TRUE)
par(mfrow=c(1,1))
plot(index(rp_train_last10), as.numeric(rp_train_last10),
     type = "b", pch = 16,
     xlim = c(index(rp_train_last10)[1], t1_date),
     ylim = ylim_,
     main = "Portefeuille 50/50 — 10 derniers rendements de la fenêtre + scénarios T+1",
     xlab = "Date", ylab = "Rendement portefeuille")

abline(h = 0, lty = 2)

points(rep(t1_date, length(rp_sim)), rp_sim,
       pch = 16, col = rgb(1, 0, 0, 0.25))
points(t1_date, mean(rp_sim), pch = 18, cex = 1.6, col = "blue")
points(t1_date, quantile(rp_sim, 0.05), pch = 17, cex = 1.6, col = "black")

legend("topleft",
       legend = c("Portefeuille (10 derniers de la fenêtre)", "1000 scénarios T+1", "Moyenne", "VaR 95%"),
       pch = c(16, 16, 18, 17),
       col = c("black", rgb(1,0,0,0.5), "blue", "black"),
       bty = "n")

par(mfrow=c(1,2), mar=c(4,4,3,1))
plot(index(rp_last10), rp_last10,
     type="l", lwd=2,
     main="Portefeuille 50/50\n10 derniers rendements",
     xlab="Date", ylab="Rendement")
VaR_95 <- quantile(rp_sim, 0.05)
VaR_95


