library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

stations_final <- data.frame(
  Station = c(
    "Cuxhaven", "Greifswald", "Berleburg, Bad-Stünzel", "Schmücke", "Konstanz",
    "Garmisch-Partenkirchen", "Großer Arber", "Schneifelforsthaus", "Stötten", "Brocken"
  ),
  lat = c(
    53.8713,     # Cuxhaven
    54.0967,     # Greifswald
    50.9837,     # Berleburg, Bad-Stünzel
    50.6545,     # Schmücke
    47.6952,     # Konstanz
    47.4830,     # Garmisch-Partenkirchen
    49.1129,     # Großer Arber
    50.2968,     # Schneifelforsthaus
    48.6656,     # Stötten
    51.7990      # Brocken
  ),
  lon = c(
    8.7060,      # Cuxhaven
    13.4056,     # Greifswald
    8.3683,      # Berleburg, Bad-Stünzel
    10.7696,     # Schmücke
    9.1307,      # Konstanz
    11.0621,     # Garmisch-Partenkirchen
    13.1338,     # Großer Arber
    6.4194,      # Schneifelforsthaus
    9.8648,      # Stötten
    10.6183      # Brocken
  )
)

deutschland <- ne_countries(country = "Germany", returnclass = "sf")

stations_sf <- st_as_sf(stations_final, coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = deutschland, fill = "gray90", color = "black") +
  geom_sf(data = stations_sf, color = "red", size = 3) +
  geom_sf_text(data = stations_sf, aes(label = Station), size = 3, nudge_y = 0.3) +
  coord_sf(xlim = c(5, 15.5), ylim = c(47, 55.5), expand = FALSE) +
  theme_minimal() +
  labs(title = "Ausgewählte 10 DWD-Stationen für die Fallstudie",
       subtitle = "Küsten-, Seen- und Mittelgebirgslagen in Deutschland",
       x = "Längengrad", y = "Breitengrad")

