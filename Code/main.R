library("here","zoo","xts","PerformanceAnalytics")
source(here("Function", "f_forecast_var.R"))
source(here("Function", "f_var_roll_wndw.R"))

#Loader les data qui ont été filtrées
load(here("Data", "processed", "prices_processed.rda"))

#Calcul des log-rendements
logRets_ <- PerformanceAnalytics::Return.calculate(prices= prices_processed,
                                                   method= "log")
#Enlever le NA en première position
logRets <- logRets_[-1, ]

#Sauvegarder figure des logRets pour SP500
png(filename = here("Output", "logReturnSP500.png"),
    width = 1600,
    height = 900,
    res = 150)
plot(index(logRets), logRets[,1],
     type = "l",
     col = "navy",
     lwd = 2,
     main = paste("Log-returns —", colnames(logRets)[1]),
     xlab = "Date",
     ylab = "Log-return")
dev.off()

#Sauvegarder figure des logRets pour FTSE100
png(filename = here("Output", "logReturnFTSE100.png"),
    width = 1600,
    height = 900,
    res = 150)
plot(index(logRets), logRets[,2],
     type = "l",
     col = "navy",
     lwd = 2,
     main = paste("Log-returns —", colnames(logRets)[2]),
     xlab = "Date",
     ylab = "Log-return")
dev.off()


#Isole les rendements des 1000 premieres journées
T <- 1000

y_sp500 <- logRets[1:T,1]
y_ftse100 <- logRets[1:T,2]

# VaR forecast pour chaque indice au niveau de risque de 95%
estimation_var_sp500_95 <- f_forecast_var(y = y_sp500, level = 0.95)
estimation_var_ftse_95 <- f_forecast_var(y = y_ftse100, level = 0.95)

# Extraction de la VaR
var_sp500_val <- estimation_var_sp500_95$VaR_Forecast # -0.0694
var_ftse100_val <- estimation_var_ftse_95$VaR_Forecast  # -0.0624


#----- BACKTESTING fenetre glissante de 1000 pour T=1000 -----
window <- 1000
h <- 1000
level <- 0.95
## ---- verificationFichierVaR ----

#
file_var <- here("Output", "VaR_roll_xts.rda")
if (file.exists(file_var)) {
  load(file_var)
  valid_object <- exists("VaR_roll_xts") && 
                  nrow(VaR_roll_xts) > 0 &&
                  all(colnames(VaR_roll_xts) == colnames(logRets))
  if (!valid_object) {
    message("Fichier invalide. Recalcul de la VaR rolling.")
    VaR_roll_xts <- f_var_roll_wndw(logRets, window, h, level)
    save(VaR_roll_xts, file = file_var)
  }
} else {
  message("Fichier inexistant. Calcul de la VaR rolling.")
  VaR_roll_xts <- f_var_roll_wndw(logRets, window, h, level)
  save(VaR_roll_xts, file = file_var)
}
## ---- saveresultats ----

# Rendements réalisés sur la période backtesting (1000 prochains jours)
logRets_subset <- logRets[index(VaR_roll_xts)] 

#Sauvegarder figure des séries de rendements réalisés et les estimations de la VaR
png(filename = here("Output", "logRets_VS_VaR.png"),
    width = 1600,
    height = 900,
    res = 150)
par(mfrow = c(ncol(logRets), 1), mar = c(4, 4, 3, 6), xpd = TRUE)
for (j in 1:ncol(logRets)) {
  ylim_ <- range(c(logRets_subset[, j], VaR_roll_xts[, j]), na.rm = TRUE)
  plot(index(logRets_subset), as.numeric(logRets_subset[, j]),
       type = "l",
       lwd = 2,                 
       col = "black",
       main = paste0("Rendements réalisés et VaR 95% — ", colnames(logRets)[j]),
       xlab = "Date", ylab = "Rendement / VaR",
       ylim = ylim_)
  lines(index(VaR_roll_xts), as.numeric(VaR_roll_xts[, j]),
        lwd = 3,                  
        col = "red")
  legend("topright",     
         legend = c("Rendements réalisés", "VaR 95% (1-step ahead)"),
         lwd = c(2, 3),
         col = c("black", "red"),
         bty = "n")
}
dev.off()
par(mfrow = c(1, 1))


# #Vérification de la VaR @ 95%
# level <- 0.95
# p <- 1 - level
# r_real <- rets[,1][index(VaR_roll_xts)]
# VaR_pred <- VaR_roll_xts
# viol <- (r_real < VaR_pred)
# phat <- mean(viol, na.rm=TRUE)
# phat
# 
# 
# stopifnot(ncol(rets) >= 2, ncol(VaR_roll_xts) >= 2)
# 
# dat <- merge(rets[, 1:2], VaR_roll_xts[, 1:2], join = "inner")
# dat <- na.omit(dat)
# 
# 
# colnames(dat) <- c(paste0(colnames(rets)[1:2], "_ret"),
#                    paste0(colnames(VaR_roll_xts)[1:2], "_VaR"))
# 
# 
# png(filename = "rets_vs_VaR_95.png",
#     width = 1600, height = 900, res = 150)
# 
# par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
# 
# for (j in 1:2) {
#   ret_j <- dat[, j]
#   var_j <- dat[, j + 2]
# 
#   ylim_ <- range(c(ret_j, var_j), na.rm = TRUE)
# 
#   plot(index(dat), as.numeric(ret_j), type = "l",
#        main = paste0("Rendements réalisés et VaR 95% — ", colnames(rets)[j]),
#        xlab = "Date", ylab = "Rendement / VaR",
#        ylim = ylim_)
# 
#   lines(index(dat), as.numeric(var_j), lwd = 2)
# 
#   legend("topright",
#          legend = c("Rendements réalisés", "VaR 95% (quantile gauche)"),
#          lwd = c(1, 2), bty = "n")
# }
# 
# dev.off()
# 
# #cat("PNG enregistré : rets_vs_VaR_95_two_series.png\n")
# 
# 
