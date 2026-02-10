library("here")
library("xts")
library("zoo")

load(here("Data/raw", "indices.rda"))
head(prices)
tail(prices)
summary(prices)
plot(prices)
range(index(prices))

prices <- prices["2005-01/"]
head(prices)
tail(prices)
summary(prices)
plot(prices)
range(index(prices))

