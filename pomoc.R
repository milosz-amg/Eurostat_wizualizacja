library(dplyr)

# Load your dataset if needed
oze_sources_all <- get_eurostat("nrg_bal_peh", time_format = "date")
saveRDS(oze_sources_all, "data/oze_sources_by_product.rds")

# 1. Summary of available countries
cat("Countries in dataset:\n")
print(unique(oze_sources_all$geo))

# 2. Year range
oze_sources_all <- oze_sources_all %>%
  mutate(rok = lubridate::year(TIME_PERIOD))

cat("\nYears available:\n")
print(range(oze_sources_all$rok))

# 3. Count records per country-year
cat("\nRecord counts by country and year:\n")
print(oze_sources_all %>%
  count(geo, rok) %>%
  arrange(geo, rok))

# 4. View available siec codes for a selected country and year
selected_country <- "PL"
selected_year <- 2022

cat("\nSIEC codes for selected country and year:\n")
oze_sources_all %>%
  filter(geo == selected_country, rok == selected_year) %>%
  count(siec, sort = TRUE) %>%
  print(n = 100)

# 5. Optional: check which of your target codes are present
target_codes <- c("RA300", "RA410", "RA420", "R5110", "R5200", "R5310")  # example
cat("\nTarget SIEC codes present in data:\n")
present_codes <- oze_sources_all %>%
  filter(geo == selected_country, rok == selected_year) %>%
  filter(siec %in% target_codes) %>%
  distinct(siec)
print(present_codes)
