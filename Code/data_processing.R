library("here")
library("xts")
library("zoo")

load(here("Data", "raw", "indices.rda"))

prices_processed <- prices["2005-01/"]

save(prices_processed,
     file = here("Data", "processed", "prices_processed.rda"))