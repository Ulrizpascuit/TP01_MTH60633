library("here")
library("zoo")
library("xts")
library("PerformanceAnalytics")
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

# Chemin vers le fichier VaR_roll_xts
file_var <- here("Output", "VaR_roll_xts.rda")
# Condition d'existence du fichier
if (file.exists(file_var)) {
  load(file_var)
  # Condition si le fichier est du bon format
  valid_object <- exists("VaR_roll_xts") && 
                  nrow(VaR_roll_xts) > 0 &&
                  all(colnames(VaR_roll_xts) == colnames(logRets))
  # Si mauvais fichier, recalculer à partir de la fonction
  if (!valid_object) {
    message("Fichier invalide. Recalcul de la VaR rolling.")
    VaR_roll_xts <- f_var_roll_wndw(logRets, window, h, level)
    save(VaR_roll_xts, file = file_var)
  }
  # Si fichier inexistant, recalculer à partir de la fonction
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
par(mfrow = c(ncol(logRets), 1), mar = c(4, 4, 3, 6))
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


## ---- verificationVaR ----
level <- 0.95
p <- 1 - level

# Rendements réalisés alignés sur les dates de VaR_roll_xts
r_real_xts <- logRets[index(VaR_roll_xts), ]

# VaR prédites
VaR_pred_xts <- VaR_roll_xts

# Violations : rendement réalisé < VaR prédite
viol_xts <- (r_real_xts < VaR_pred_xts)

# p-hat par indice
phat_sp500  <- mean(viol_xts[, 1], na.rm = TRUE)
phat_ftse100 <- mean(viol_xts[, 2], na.rm = TRUE)

# sauvegarde des données pertinentes du TP dans un seul fichier .rda
backtest_results <- list(
  level = level,
  p_theorique = p,
  prices_processed = prices_processed,
  logRets = logRets,
  VaR_roll_xts = VaR_roll_xts,
  phat_sp500 = phat_sp500,
  phat_ftse100 = phat_ftse100,
  violations = viol_xts
)

# Sauvegarde dans le dossier Output
save(backtest_results,
     file = here("Output", "all_results.rda"))

