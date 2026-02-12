library("here")
library("xts")
library("zoo")
library("PerformanceAnalytics")
source(here("Function", "f_forecast_var.R"))
load(here("Data", "processed", "prices_processed.rda"))
head(prices_processed)
tail(prices_processed)
plot(prices_processed)

ret_ <- Return.calculate(prices= prices_processed,
                  method= "log")
ret <- ret_[-1, ]

head(ret)
tail(ret)
plot(ret)

level <- 0.95
var_fcst <- f_forecast_var(ret, level)
str(var_fcst)
names(var_fcst)
plot(var_fcst$VaR_Forecast, type="l", main="VaR Forecast (GARCH, 95%)")
plot(var_fcst$ConditionalVariances, type="l", main="Conditional Variance (GARCH)")
