library(rdwd)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

stations_final_ids <- stations_final %>%
  left_join(
    stations %>%
      select(Stationsname, Stations_ID),
    by = c("Station" = "Stationsname")
  ) %>%
  rename(Stations_ID = Stations_ID.x) %>%
  select(Station, Stations_ID, lat, lon)

# update to the most recent fileIndex (ca monthly):
rdwd::updateRdwd() 
# Load the package into library (needed in every R session):

ids <- stations_final_ids$Stations_ID

library(rdwd)
links_hist <- selectDWD(
  id = ids,
  res = "daily",
  var = "kl",
  per = "historical"
)

# Recent Links
links_rec <- selectDWD(
  id = ids,
  res = "daily",
  var = "kl",
  per = "recent"
)

# Links kombinieren
links_all <- c(links_hist, links_rec)

#Files Herunterladen
files_all <- dataDWD(links_all, read = FALSE)

#Einlesen und kombinieren
dat_list_all <- lapply(files_all, readDWD, varnames = TRUE)

# Kombiniere alle Stationen und Zeiträume in einem DataFrame:
all_data <- bind_rows(dat_list_all)

#Filter auf die letzten 10 Jahre:
library(lubridate)
all_data$date <- ymd(all_data$MESS_DATUM)

all_data_10 <- all_data %>%
  filter(date >= as.Date("2015-01-01") & date <= as.Date("2025-12-31"))
