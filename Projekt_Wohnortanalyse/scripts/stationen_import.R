#Paket laden
library(rdwd)

stations <- read.fwf(
  file = "data/stationsübersicht.txt",
  skip = 2,  # <-- Überspringt Header und Trennzeile
  widths = c(6, 9, 9, 14, 12, 10, 41, 23),
  col.names = c("Stations_ID", "von_datum", "bis_datum", "Stationshoehe",
                "geoBreite", "geoLaenge", "Stationsname", "Bundesland"),
  fileEncoding = "latin1",
  stringsAsFactors = FALSE,
  strip.white = TRUE
)