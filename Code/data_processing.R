library("here")
library("xts")
library("zoo")

load(here("Data", "raw", "indices.rda"))
plot(prices)

prices_processed <- prices["2005-01/"]
plot(prices_processed)

save(prices_processed,
     file = here("Data", "processed", "prices_processed.rda"))