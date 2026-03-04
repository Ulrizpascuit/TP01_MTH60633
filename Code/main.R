library("here","zoo","xts","PerformanceAnalytics")
source(here("Function", "f_forecast_var.R"))

#Loader les data qui ont été filtrées
load(here("Data", "processed", "prices_processed.rda"))

#Calcul des log-rendements
rets_ <- PerformanceAnalytics::Return.calculate(prices= prices_processed,
                  method= "log")
#Enlever le NA en première position
rets <- rets_[-1, ]

#Sauvegarder figure des logRets pour SP500
png(filename = here("Output", "logReturnSP500.png"),
    width = 1600,
    height = 900,
    res = 150)
plot(index(rets), coredata(rets)[,1],
     type = "l",
     col = "navy",
     lwd = 2,
     main = paste("Log-returns —", colnames(rets)[1]),
     xlab = "Date",
     ylab = "Log-return")
dev.off()

#Sauvegarder figure des logRets pour FTSE100
png(filename = here("Output", "logReturnFTSE100.png"),
    width = 1600,
    height = 900,
    res = 150)
plot(index(rets), coredata(rets)[,2],
     type = "l",
     col = "navy",
     lwd = 2,
     main = paste("Log-returns —", colnames(rets)[2]),
     xlab = "Date",
     ylab = "Log-return")
dev.off()


#Isole les rendements des 1000 premieres journées
T_estimation <- 1000

y_sp500_static <- log_rets_sp500[1:T_estimation]
y_ftse100_static <- log_rets_ftse100[1:T_estimation]

# VaR forecast pour chaque indice au niveau de risque de 95%
estimation_var_sp500_95 <- f_forecast_var(y = y_sp500_static,
                                          level = 0.95)
estimation_var_ftse_95 <- f_forecast_var(y = y_ftse100_static,
                                         level = 0.95)

var_sp500_val <- estimation_var_sp500_95$VaR # -0.049857
var_ftse100_val <- estimation_var_ftse_95$VaR # -0.046695
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

n_assets <- ncol(rets)


VaR_roll <- matrix(NA, nrow = h, ncol = n_assets)

for(j in 1:n_assets){ 
  
  for(k in 1:h){          
    
    y_window <- rets[k:(window+k-1), j]
    
    out <- f_forecast_var(y_window, level)
    
    VaR_roll[k, j] <- tail(out$VaR_Forecast, 1)
  }
}

colnames(VaR_roll) <- colnames(rets)

dates_var <- index(rets)[(window+1):(window+h)]

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


png(filename = "rets_vs_VaR_95.png",
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

#cat("PNG enregistré : rets_vs_VaR_95_two_series.png\n")


