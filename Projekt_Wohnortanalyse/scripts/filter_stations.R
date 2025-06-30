library(dplyr)
stations_filtered <- stations

stations_filtered <- stations_filtered %>%
  filter(stations_filtered$Stationshoehe >= 600)

stations_filtered <- stations_filtered %>%
  filter(
    !grepl("Berlin|Hamburg|München|Frankfurt|Köln|Stuttgart|Düsseldorf|Leipzig|Essen|Zugspitze", Stationsname)
  )

wasserorte <- c("Konstanz", "Starnberg", "Norderney", "List", "Emden", 
                "Kiel", "Greifswald", "Lübeck", "Cuxhaven", "Plön", "Waren", "Schwerin")

stations_wasser <- stations %>%
  filter(grepl(paste(wasserorte, collapse = "|"), Stationsname, ignore.case = TRUE))

stations_kombiniert <- bind_rows(stations_filtered, stations_wasser) %>%
  distinct(Stations_ID, .keep_all = TRUE)

stations_kombiniert <- stations_kombiniert %>%
  mutate(
    datum_char = as.character(stations_kombiniert$bis_datum),
    bis_datum = as.numeric(substr(datum_char, 1, 4))
  )

stations_kombiniert <- stations_kombiniert %>%
  mutate(
    datum_char = as.character(stations_kombiniert$von_datum),
    von_datum = as.numeric(substr(datum_char, 1, 4))
  )

stations_kombiniert <- stations_kombiniert %>%
  filter(stations_kombiniert$bis_datum == 2025)

stations_sf <- st_as_sf(
  stations_kombiniert,
  coords = c("geoLaenge", "geoBreite"),
  crs = 4326
)

ggplot() +
  geom_sf(data = deutschland, fill = "gray95", color = "black") +
  geom_sf(data = stations_sf, color = "red", size = 2) +
  geom_sf_text(data = stations_sf, aes(label = Stationsname), size = 3, nudge_y = 0.1) +
  coord_sf(xlim = c(5, 15.5), ylim = c(47, 55.5), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Gefilterte DWD-Stationen auf der Deutschlandkarte",
    subtitle = "Für Fallstudie: Ideale Wohnortwahl bei stabiler Wetterlage",
    x = "Längengrad",
    y = "Breitengrad"
  )

#rm(list = ls())
