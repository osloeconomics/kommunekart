library(sf)
library(dplyr)
library(stringr)

kom2020 <- read_sf("kommuner20_simple.geojson") %>%
  mutate(kommunenummer = str_pad(kommunenummer, 4, pad = "0"),
         fylkenummer = substr(kommunenummer, 1, 2),
         fylke = case_when(fylkenummer == "30" ~ "Viken",
                           fylkenummer == "03" ~ "Oslo",
                           fylkenummer == "34" ~ "Innlandet",
                           fylkenummer == "38" ~ "Vestfold og Telemark",
                           fylkenummer == "42" ~ "Agder",
                           fylkenummer == "11" ~ "Rogaland",
                           fylkenummer == "46" ~ "Vestland",
                           fylkenummer == "15" ~ "Møre og Romsdal",
                           fylkenummer == "50" ~ "Trøndelag",
                           fylkenummer == "18" ~ "Nordland",
                           fylkenummer == "54" ~ "Troms og Finnmark"))

fyl2020 <- kom2020 %>%
  group_by(fylke, fylkenummer) %>%
  summarize()

saveRDS(kom2020, "kommuner_2020_simple.rds")
saveRDS(fyl2020, "fylker_2020_simple.rds")

kom2020 <- read_sf("kommuner20_simpler.geojson") %>%
  mutate(kommunenummer = str_pad(kommunenummer, 4, pad = "0"),
         fylkenummer = substr(kommunenummer, 1, 2),
         fylke = case_when(fylkenummer == "30" ~ "Viken",
                           fylkenummer == "03" ~ "Oslo",
                           fylkenummer == "34" ~ "Innlandet",
                           fylkenummer == "38" ~ "Vestfold og Telemark",
                           fylkenummer == "42" ~ "Agder",
                           fylkenummer == "11" ~ "Rogaland",
                           fylkenummer == "46" ~ "Vestland",
                           fylkenummer == "15" ~ "Møre og Romsdal",
                           fylkenummer == "50" ~ "Trøndelag",
                           fylkenummer == "18" ~ "Nordland",
                           fylkenummer == "54" ~ "Troms og Finnmark"))

fyl2020 <- kom2020 %>%
  group_by(fylke, fylkenummer) %>%
  summarize()

saveRDS(kom2020, "kommuner_2020_simpler.rds")
saveRDS(fyl2020, "fylker_2020_simpler.rds")

kom2020 <- read_sf("kommuner20_simplest.geojson") %>%
  mutate(kommunenummer = str_pad(kommunenummer, 4, pad = "0"),
         fylkenummer = substr(kommunenummer, 1, 2),
         fylke = case_when(fylkenummer == "30" ~ "Viken",
                           fylkenummer == "03" ~ "Oslo",
                           fylkenummer == "34" ~ "Innlandet",
                           fylkenummer == "38" ~ "Vestfold og Telemark",
                           fylkenummer == "42" ~ "Agder",
                           fylkenummer == "11" ~ "Rogaland",
                           fylkenummer == "46" ~ "Vestland",
                           fylkenummer == "15" ~ "Møre og Romsdal",
                           fylkenummer == "50" ~ "Trøndelag",
                           fylkenummer == "18" ~ "Nordland",
                           fylkenummer == "54" ~ "Troms og Finnmark"))

fyl2020 <- kom2020 %>%
  group_by(fylke, fylkenummer) %>%
  summarize()

saveRDS(kom2020, "kommuner_2020_simplest.rds")
saveRDS(fyl2020, "fylker_2020_simplest.rds")

kom2020 <- read_sf("kommuner20_simplest_2.geojson") %>%
  mutate(kommunenummer = str_pad(kommunenummer, 4, pad = "0"),
         fylkenummer = substr(kommunenummer, 1, 2),
         fylke = case_when(fylkenummer == "30" ~ "Viken",
                           fylkenummer == "03" ~ "Oslo",
                           fylkenummer == "34" ~ "Innlandet",
                           fylkenummer == "38" ~ "Vestfold og Telemark",
                           fylkenummer == "42" ~ "Agder",
                           fylkenummer == "11" ~ "Rogaland",
                           fylkenummer == "46" ~ "Vestland",
                           fylkenummer == "15" ~ "Møre og Romsdal",
                           fylkenummer == "50" ~ "Trøndelag",
                           fylkenummer == "18" ~ "Nordland",
                           fylkenummer == "54" ~ "Troms og Finnmark"))

fyl2020 <- kom2020 %>%
  group_by(fylke, fylkenummer) %>%
  summarize()

saveRDS(kom2020, "kommuner_2020_simplest_2.rds")
saveRDS(fyl2020, "fylker_2020_simplest_2.rds")
write_sf(fyl2020, "fylker_2020_simplest_2.geojson")
