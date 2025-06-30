library(tidyverse)
library(broom)
library(ggplot2)

# Annahme: data_trend enthält die Spalten Station, Parameter, Jahr, SD
# Modelle pro Station und Parameter berechnen
trend_models <- data_trend %>%
  group_by(Station, Parameter) %>%
  filter(sum(!is.na(SD)) > 0) %>%     # <-- nur Gruppen mit Daten zulassen
  nest() %>%
  mutate(
    model = map(data, ~ lm(SD ~ Jahr, data = .x))
  )

# Prognosejahre vorbereiten
future_years <- tibble(Jahr = 2026:2028)

# Prognosen generieren
predictions <- trend_models %>%
  mutate(
    future = map(model, ~ predict(.x, newdata = future_years, interval = "confidence") %>%
                   as_tibble() %>%
                   mutate(Jahr = future_years$Jahr))
  ) %>%
  select(Station, Parameter, future) %>%
  unnest(future)

# Plots mit Prognose erstellen und anzeigen, ohne zu speichern
data_trend %>%
  group_split(Station, Parameter) %>%
  walk(~ {
    df_past <- .x
    station_name <- unique(df_past$Station)
    parameter_name <- unique(df_past$Parameter)
    
    df_pred <- predictions %>%
      filter(Station == station_name, Parameter == parameter_name)
    
    p <- ggplot() +
      geom_point(data = df_past, aes(x = Jahr, y = SD), color = "steelblue", size = 2) +
      geom_smooth(data = df_past, aes(x = Jahr, y = SD), method = "lm", se = TRUE, color = "firebrick", linewidth = 1) +
      geom_point(data = df_pred, aes(x = Jahr, y = fit), color = "firebrick", size = 2) +
      geom_line(data = df_pred, aes(x = Jahr, y = fit), color = "firebrick", linetype = "dashed") +
      geom_ribbon(data = df_pred, aes(x = Jahr, ymin = lwr, ymax = upr), fill = "firebrick", alpha = 0.2) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Prognose der Variabilität:", parameter_name),
        subtitle = paste("Station:", station_name, "| Jahre 2026–2028 prognostiziert"),
        x = "Jahr",
        y = "Variabilität (SD)",
        caption = "Prognose basierend auf linearer Regression der Jahre 2015–2025"
      )
    
    print(p)
  })
