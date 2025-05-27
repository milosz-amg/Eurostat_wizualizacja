library(eurostat)
library(dplyr)
library(lubridate)

# Pobierz surową tabelę bez filtrowania
co2_raw <- get_eurostat("env_air_gge", time_format = "date")

cat("== 1) Nazwy kolumn =========================\n")
print(names(co2_raw))

cat("\n== 2) Pierwsze 8 wierszy ==================\n")
print(head(co2_raw, 8))

cat("\n== 3) Zakres lat w TIME_PERIOD ============\n")
print(range(co2_raw$TIME_PERIOD))

# Sprawdźmy, czy TIME_PERIOD jest typu Date:
cat("\n== Klasa TIME_PERIOD =======================\n")
print(class(co2_raw$TIME_PERIOD))

# Utwórz 'rok' i pokaż strukturę
co2_raw <- co2_raw %>% mutate(rok = year(TIME_PERIOD))

cat("\n== 4) Czy 'rok' istnieje i przykładowe wartości?\n")
print(names(co2_raw))
print(head(co2_raw %>% select(geo, TIME_PERIOD, rok), 5))
