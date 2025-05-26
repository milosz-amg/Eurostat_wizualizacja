library(dplyr)
library(eurostat)

# Pobierz dane
oze_products <- get_eurostat("nrg_cb_rw", time_format = "date", cache = TRUE)

# Filtr: tylko produkcja (np. PRIMARYPROD), tylko UE, tylko TJ
oze_sources_clean <- oze_products %>%
  filter(
    nrg_bal == "GEP",        # Gross Electricity Production (lub sprawdź inne: PRIMARYPROD, TRANSFPROD, ...)
    unit == "TJ",
    geo %in% eurostat::eu_countries$code
  ) %>%
  group_by(geo, TIME_PERIOD, product) %>%
  summarise(energia = sum(values, na.rm = TRUE), .groups = "drop")

# Zapisz do pliku
saveRDS(oze_sources_clean, "data/oze_sources_by_product.rds")
