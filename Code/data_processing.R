library("here")
library("xts")
library("zoo")

load(here("Data", "raw", "indices.rda"))
head(prices)
tail(prices)
summary(prices)
plot(prices)
range(index(prices))

prices_processed <- prices["2005-01/"]
head(prices_processed)
tail(prices_processed)
summary(prices_processed)
plot(prices_processed)
range(index(prices_processed))

save(prices_processed,
     file = here("Data", "processed", "prices_processed.rda"))