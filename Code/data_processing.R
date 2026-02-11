library("xts")
library("zoo")
#selection de la plage janvier 2005 à dernière data
data_TP01 <- prices["2005-01-01/"]
#rendement log pour chaque indice
logr_SPX <- diff(log(data_TP01$SP500))
logr_UKX <- diff(log(data_TP01$FTSE100))
#suppression des NA (les première valeures)
logr_SPX <- logr_SPX[!is.na(logr_SPX)]
logr_UKX <- logr_UKX[!is.na(logr_UKX)]
#verification de taille des deux séries, pas forcément utile
length(logr_UKX)
length(logr_SPX)
