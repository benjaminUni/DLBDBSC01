library(tidyverse)
library(ggplot2)

# Parameterliste definieren
parameter_list <- c("TMK.Lufttemperatur", "UPM.Relative_Feuchte", "FM.Windgeschwindigkeit")

# Prognosejahre definieren
future_years <- tibble(Jahr = 2026:2028)

# Schleife über alle Parameter
for (param_sel in parameter_list) {
  
# Historische Daten filtern
  df_hist <- data_trend %>%
    filter(Parameter == param_sel)
  
# Trendmodelle pro Station berechnen
  trend_models <- df_hist %>%
    group_by(Station) %>%
    filter(sum(!is.na(SD)) > 2) %>%
    nest() %>%
    mutate(model = map(data, ~ lm(SD ~ Jahr, data = .x)))
  
# Prognosen berechnen
  predictions <- trend_models %>%
    mutate(pred = map(model, ~ predict(.x, newdata = future_years, interval = "confidence") %>%
                        as_tibble() %>%
                        mutate(Jahr = future_years$Jahr))) %>%
    select(Station, pred) %>%
    unnest(pred) %>%
    rename(fit = fit, lwr = lwr, upr = upr)
  
# Plot erstellen
  p <- ggplot() +
# Historische Daten
    geom_line(data = df_hist, aes(x = Jahr, y = SD, color = Station, linetype = "Historisch"), size = 1) +
    geom_point(data = df_hist, aes(x = Jahr, y = SD, color = Station), size = 1) +
    
# Prognosedaten
    geom_line(data = predictions, aes(x = Jahr, y = fit, color = Station, linetype = "Prognose"), size = 1) +
    geom_point(data = predictions, aes(x = Jahr, y = fit, color = Station), shape = 1, size = 2) +
    
    scale_linetype_manual(
      name = "Datenquelle",
      values = c("Historisch" = "solid", "Prognose" = "dashed")
    ) +
    scale_color_brewer(palette = "Paired") +
    
    theme_minimal(base_size = 14) +
    labs(
      title = paste("Vergleich der Variabilität aller Stationen:", param_sel),
      subtitle = "Historische Daten (2015–2025) und Prognose (2026–2028)",
      x = "Jahr",
      y = "Variabilität (SD)",
      color = "Station"
    ) +
    geom_vline(xintercept = 2025.5, linetype = "dotted", color = "grey50")+
    theme(legend.position = "right")
  ggsave(
    filename = paste0("Output/Plot_", gsub("\\.", "_", param_sel), ".png"),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
  print(p)
}