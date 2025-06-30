library(tidyverse)
#Zeitraum der letzten zehn Jahre auswählen
all_data$date <- ymd(all_data$MESS_DATUM)
all_data_10 <- all_data %>%
  filter(date >= as.Date("2015-01-01") & date <= as.Date("2025-12-31"))

#selected_station_id <- ids[1] # ID der Station auswählen
#station_data <- all_data_10 %>%
#  filter(STATIONS_ID == selected_station_id)

#stationsname <- stations_final_ids$Station[stations_final_ids$Stations_ID == selected_station_id]

all_data_10 <- all_data_10 %>%
  mutate(STATIONS_ID = as.character(STATIONS_ID)) %>%
  left_join(
    stations_final_ids %>% mutate(Stations_ID = as.character(Stations_ID)),
    by = c("STATIONS_ID" = "Stations_ID")
  )

long_data <- all_data_10 %>%
  pivot_longer(
    cols = c(TMK.Lufttemperatur, UPM.Relative_Feuchte, FM.Windgeschwindigkeit),
    names_to = "Parameter",
    values_to = "Wert"
  )

#Variabilität berechnen
variability_summary <- long_data %>%
  mutate(Jahr = year(date)) %>%
  group_by(Station, Parameter, Jahr) %>%
  summarise(
    Mittelwert = mean(Wert, na.rm = TRUE),
    SD = sd(Wert, na.rm = TRUE),
    .groups = "drop"
  )

#Variabilitätsscore und Filterung
variabilitaet_score <- variability_summary %>%
  filter(Jahr >= 2015, Jahr <= 2025) %>%
  group_by(Station, Parameter) %>%
  summarise(mean_SD = mean(SD, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Parameter, values_from = mean_SD) %>%
  mutate(
    Variabilitaetsscore = rowMeans(select(., starts_with("TMK"):starts_with("FM")), na.rm = TRUE)
  ) %>%
  arrange(Variabilitaetsscore)

print(variabilitaet_score)

##Berechnung der Variabilität der Temperatur
temperature_variability <- variability_summary %>%
  filter(Parameter == "TMK.Lufttemperatur")

#ANOVA Test
anova_result_Temp <- aov(SD ~ Station, data = temperature_variability)
summary(anova_result_Temp)

#TukeyHSD-Analyse
tukey_result_Temp <- TukeyHSD(anova_result_Temp)
tukey_df <- as.data.frame(tukey_result_Temp$Station)
tukey_df$Comparison <- rownames(tukey_df)
rownames(tukey_df) <- NULL
tukey_df <- tukey_df %>%
  mutate(Significant = ifelse(`p adj` < 0.05, "Ja", "Nein"))

##Berechnung der Variabilität der Relativen Luftfeuchtigkeit
rfk_variability <- variability_summary %>%
  filter(Parameter == "UPM.Relative_Feuchte")

#ANOVA Test
anova_rfk <- aov(SD ~ Station, data = rfk_variability)
summary(anova_rfk)

#TukeyHSD-Analyse
tukey_rfk <- TukeyHSD(anova_rfk)
tukey_rfk_df <- as.data.frame(tukey_rfk$Station)
tukey_rfk_df$Comparison <- rownames(tukey_rfk_df)
rownames(tukey_rfk_df) <- NULL

tukey_rfk_df <- tukey_rfk_df %>%
  mutate(Significant = ifelse(`p adj` < 0.05, "Ja", "Nein"))

##Berechnung der Variabilität der Windgeschwindigkeit
fg_variability <- variability_summary %>%
  filter(Parameter == "FM.Windgeschwindigkeit")

#ANOVA Test
anova_fg <- aov(SD ~ Station, data = fg_variability)
summary(anova_fg)

#TukeyHSD-Analyse
tukey_fg <- TukeyHSD(anova_fg)
tukey_fg_df <- as.data.frame(tukey_fg$Station)
tukey_fg_df$Comparison <- rownames(tukey_fg_df)
rownames(tukey_fg_df) <- NULL

tukey_fg_df <- tukey_fg_df %>%
  mutate(Significant = ifelse(`p adj` < 0.05, "Ja", "Nein"))
