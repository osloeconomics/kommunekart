library(sf)
library(dplyr)
library(stringr)

kom2020 <- read_sf("kommuner20_simple.geojson") %>%
  mutate(kommunenummer = str_pad(kommunenummer, 4, pad = "0"))

saveRDS(kom2020, "kommuner_2020.rds")