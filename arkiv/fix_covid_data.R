library(dplyr)
library(sf)
library(readr)

covid <- read_csv2("antall-meldte-tilfeller.csv") %>%
  setNames(c("navn", "antall")) %>%
  mutate(navn = replace(navn, navn == "Norde Follo", "Nordre Follo"),
         navn = replace(navn, grepl("Porsanger", navn), "Porsanger"),
         navn = replace(navn, grepl("Kautokeino", navn), "Kautokeino"),
         navn = replace(navn, grepl("Karasjok", navn), "Karasjok"),
         navn = replace(navn, grepl("Tana", navn), "Tana"),
         navn = replace(navn, grepl("Nesseby", navn), "Nesseby"),
         navn = replace(navn, grepl("Kåfjord", navn), "Kåfjord"))

kart <- readRDS("kommuner_2020_simplest.rds") %>%
  st_set_geometry(NULL)

covid <- left_join(covid, kart, by = "navn")

write_csv2(covid, "antall-meldte-tilfeller.csv")
