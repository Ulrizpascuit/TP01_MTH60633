library("xts")
library("zoo")
library("PerformanceAnalytics")
#selection de la plage janvier 2005 à dernière data
data_TP01 <- prices["2005-01-01/"]
#rendement log pour chaque indice
logr_SPX <- diff(log(data_TP01$SP500))
logr_UKX <- diff(log(data_TP01$FTSE100))
#suppression des NA (les première valeurs)
logr_SPX <- logr_SPX[!is.na(logr_SPX)]
logr_UKX <- logr_UKX[!is.na(logr_UKX)]

#verification de taille des deux séries 
length(logr_UKX)
length(logr_SPX)

#fonctions

T <- 1000
logr_SPX_trim <- as.numeric(logr_SPX[1:T])
logr_UKX_trim <- as.numeric(logr_UKX[1:T])

res_SPX <- f_forecast_var(logr_SPX_trim, 0.95)
res_UKX <- f_forecast_var(logr_UKX_trim, 0.95)

res_SPX$VaR_Forecast
res_UKX$VaR_Forecast
#Plus grosse VaR pour le SPX à 6.942% contre 6.237% pour le UKX. 

#conversion en as.numeric pour gagner de la performance 
logr_SPX_num <- as.numeric(logr_SPX)
logr_UKX_num <- as.numeric(logr_UKX)
# Initialisation des vecteurs de VaR et création de celui des rendement réalisés
retSPX<- Return.calculate(prices = data_TP01$SP500, method = "discrete")[T:(T+T-1)]
retUKX<- Return.calculate(prices = data_TP01$FTSE100, method = "discrete")[T:(T+T-1)]
VaRSPX <- rep(NA, T)
VaRUKX <- rep(NA, T)

# Boucle pour les 1000 prochains jours
for (i in 1:T) {

  # Fenêtre roulante
  ySPX <- logr_SPX_num[i:(i + T - 1)]
  yUKX <- logr_UKX_num[i:(i + T - 1)]

  # VaR à t+1
  VaRSPX[i] <- f_forecast_var(ySPX, 0.95)$VaR_Forecast
  VaRUKX[i] <- f_forecast_var(yUKX, 0.95)$VaR_Forecast
}

dates <- as.Date(index(logr_SPX))
dates_trim  <- dates[T:(T+T-1)]

# Plot des 4 séries
par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))

# Forecast des VaR
matplot(dates_trim,
        cbind(VaRSPX, VaRUKX),
        type = "l",
        col = c("black", "red"),
        lty = 1, lwd = 1,
        ylab = "VaR Forecast",
        xlab = "",
        main = "VaR forecasts")

legend("topright", legend = c("SP500", "FTSE100"),
       col = c("black", "red"), lty = 1, lwd = 1, bty = "n")

# Rendements réalisés normalisés à 1
matplot(dates_trim,
        cbind(idxSPX, idxUKX),
        type = "l",
        col = c("black", "red"),
        lty = 1, lwd = 1,
        ylab = "Index (base = 1)",
        xlab = "Date",
        main = "Rendements réalisés")

legend("bottomright", legend = c("SP500", "FTSE100"),
       col = c("black", "red"), lty = 1, lwd = 1, bty = "n")

par(mfrow = c(1, 1))
