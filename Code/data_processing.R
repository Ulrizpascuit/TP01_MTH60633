library(here)
library("xts")
library("zoo")

load(file = here("data", "raw", "indices.rda"))
prices <- prices["2005-01-01/"]
log_rets_sp500 <- diff(log(prices$SP500))[-1, ]
log_rets_ftse100 <- diff(log(prices$FTSE100))[-1, ]
