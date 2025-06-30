library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

stations_sf <- stations %>%
  mutate(
    lon = as.numeric(geoLaenge),
    lat = as.numeric(geoBreite)
  ) %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Weltweite Küstenlinie (hohe Auflösung optional: scale = "large")
coastline <- ne_download(scale = "medium", type = "coastline", category = "physical", returnclass = "sf")

# Große Seen
seen <- ne_download(scale = "medium", type = "lakes", category = "physical", returnclass = "sf")

# Optional: Deutschlandgrenzen (zur Eingrenzung)
deutschland <- ne_countries(country = "Germany", returnclass = "sf")

# Projektieren in ein metrisches Koordinatensystem (Meter statt Grad)
stations_m <- st_transform(stations_sf, crs = 3857)
coastline_m <- st_transform(coastline, crs = 3857)
seen_m <- st_transform(seen, crs = 3857)

# Buffer-Zonen um Wasserflächen (z. B. 20 km)
wasser_zone <- st_union(
  st_buffer(coastline_m, dist = 20000),
  st_buffer(seen_m, dist = 20000)
)

# Nur Stationen, die innerhalb dieser Zone liegen
stations_nahe_wasser <- stations_m[st_intersects(stations_m, wasser_zone, sparse = FALSE), ]
View(stations_nahe_wasser)
nrow(stations_nahe_wasser)