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
