library("here")
library("xts")
library("zoo")

# Load les données depuis le bon folder
load(here("Data", "raw", "indices.rda"))

# Retenir les données à partir de 2005 seulement
prices_processed <- prices["2005-01/"]

# Sauvegarder les données dans un objet .rda afin de pouvoir les réutiliser directement par la suite
save(prices_processed,
     file = here("Data", "processed", "prices_processed.rda"))