library("here","zoo","xts","PerformanceAnalytics")
source(here("Function", "f_forecast_var.R"))

load(here("Data", "processed", "prices_processed.rda"))
head(prices_processed)
tail(prices_processed)
plot(prices_processed)

rets_ <- Return.calculate(prices= prices_processed,
                  method= "log")
head(rets_)
rets <- rets_[-1, ]

head(rets)
tail(rets)
plot(rets)

level <- 0.95
var_fcst <- f_forecast_var(rets[,1], level)
str(var_fcst)
names(var_fcst)
plot(var_fcst$VaR_Forecast, type="l", main="VaR Forecast (GARCH, 95%)")
plot(var_fcst$ConditionalVariances, type="l", main="Conditional Variance (GARCH)")

T <- 1000
level <- 0.95

rets_training <- rets[1:T, ]

VaR_T <- sapply(1:ncol(rets_training), function(j){
  out <- f_forecast_var(rets_training[, j], level)
  tail(out$VaR_Forecast, 1)
})

names(VaR_T) <- colnames(rets_training)

VaR_T
names(which.min(VaR_T))  

window <- 1000
h <- 1000
level <- 0.95

n_assets <- ncol(ret)


VaR_roll <- matrix(NA, nrow = h, ncol = n_assets)

for(j in 1:n_assets){ 
  
  for(k in 1:h){          
    
    y_window <- ret[k:(window+k-1), j]
    
    out <- f_forecast_var(y_window, level)
    
    VaR_roll[k, j] <- tail(out$VaR_Forecast, 1)
  }
}

colnames(VaR_roll) <- colnames(ret)

dates_var <- index(ret)[(window+1):(window+h)]

VaR_roll_xts <- xts(VaR_roll, order.by = dates_var)

plot(VaR_roll_xts,
     main = "Rolling VaR 95% (1-step ahead)",
     legend.loc = "bottomleft")

#Vérification de la VaR @ 95%
level <- 0.95
p <- 1 - level
r_real <- rets[,1][index(VaR_roll_xts)]  
VaR_pred <- VaR_roll_xts
viol <- (r_real < VaR_pred)
phat <- mean(viol, na.rm=TRUE)
phat


stopifnot(ncol(rets) >= 2, ncol(VaR_roll_xts) >= 2)

dat <- merge(rets[, 1:2], VaR_roll_xts[, 1:2], join = "inner")
dat <- na.omit(dat)


colnames(dat) <- c(paste0(colnames(rets)[1:2], "_ret"),
                   paste0(colnames(VaR_roll_xts)[1:2], "_VaR"))


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

dev.off()

cat("PNG enregistré : rets_vs_VaR_95_two_series.png\n")


