library(tidyverse)

# Score berechnen: mittlere SD pro Parameter und Station über alle Jahre
variabilitaet_score <- data_trend %>%
  filter(Jahr >= 2015, Jahr <= 2025) %>%
  group_by(Station, Parameter) %>%
  summarise(mean_SD = mean(SD, na.rm = TRUE), .groups = "drop") %>%
  group_by(Station) %>%
  summarise(Variabilitaetsscore = mean(mean_SD, na.rm = TRUE)) %>%
  arrange(Variabilitaetsscore)
