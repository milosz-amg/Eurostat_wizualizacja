# Załaduj dane
oze_sources_all <- readRDS("data/oze_sources_by_product.rds")

# Podejrzyj strukturę danych
str(oze_sources_all)

# Zobacz unikalne kolumny
colnames(oze_sources_all)

# Sprawdź unikalne wartości w kolumnie 'unit'
unique(oze_sources_all$unit)

# Opcjonalnie: kilka pierwszych wierszy
head(oze_sources_all)
