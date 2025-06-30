library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)

##Plot der Lufttemperatur mit Variabilität
#Plot Heatmap für Temperatur
ggplot(temperature_variability, aes(x = Jahr, y = Station, fill = SD)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "SD (°C)", option = "C") +
  labs(
    title = "Heatmap: Variabilität der Tagesmitteltemperatur (TMK)",
    subtitle = "Standardabweichung je Station und Jahr (2015–2025)",
    x = "Jahr",
    y = "Station"
  ) +
  scale_x_continuous(breaks = 2015:2025) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid = element_blank()
  )

# Plot Heatmap für relative Feuchte
ggplot(rfk_variability, aes(x = Jahr, y = Station, fill = SD)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "SD (%)", option = "D") +
  labs(
    title = "Heatmap: Variabilität der Relativen Feuchte (UPM)",
    subtitle = "Standardabweichung je Station und Jahr (2015–2025)",
    x = "Jahr",
    y = "Station"
  ) +
  scale_x_continuous(breaks = 2015:2025) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid = element_blank()
  )


# Plot Heatmap für Windgeschwindigkeit
ggplot(fg_variability, aes(x = Jahr, y = Station, fill = SD)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "SD (m/s)", option = "J") +
  labs(
    title = "Heatmap: Variabilität der Windgeschwindigkeit (FM)",
    subtitle = "Standardabweichung je Station und Jahr (2015–2025)",
    x = "Jahr",
    y = "Station"
  ) +
  scale_x_continuous(breaks = 2015:2025) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid = element_blank()
  )

#Plot der Standardabweichung der Temperaturvariabilität
ggplot(tukey_df, aes(x = diff, y = reorder(Comparison, diff), color = Significant)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("Ja" = "firebrick", "Nein" = "steelblue")) +
  labs(
    title = "TukeyHSD: Paarweise Vergleiche der Temperaturvariabilität (TMK)",
    subtitle = "Vergleich der Standardabweichung (SD) zwischen den Stationen",
    x = "Differenz der mittleren SD",
    y = "Stationsvergleich",
    color = "Signifikant (p < 0.05)"
  ) +
  theme_minimal(base_size = 14)

#Plot der Standardabweichung der Relativen Feuchte
ggplot(tukey_rfk_df, aes(x = diff, y = reorder(Comparison, diff), color = Significant)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("Ja" = "firebrick", "Nein" = "steelblue")) +
  labs(
    title = "TukeyHSD: Paarweise Vergleiche der Feuchtevariabilität (UPM)",
    subtitle = "Vergleich der Standardabweichung (SD) zwischen den Stationen",
    x = "Differenz der mittleren SD",
    y = "Stationsvergleich",
    color = "Signifikant (p < 0.05)"
  ) +
  theme_minimal(base_size = 14)

#Plot der Standardabweichung der Windgeschwindigkeit
ggplot(tukey_fg_df, aes(x = diff, y = reorder(Comparison, diff), color = Significant)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("Ja" = "firebrick", "Nein" = "steelblue")) +
  labs(
    title = "TukeyHSD: Paarweise Vergleiche der Windvariabilität (FM)",
    subtitle = "Vergleich der Standardabweichung (SD) zwischen den Stationen",
    x = "Differenz der mittleren SD",
    y = "Stationsvergleich",
    color = "Signifikant (p < 0.05)"
  ) +
  theme_minimal(base_size = 14)

