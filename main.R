library(shiny)
library(shinydashboard)
library(DT)
library(eurostat)
library(dplyr)
library(leaflet)
library(sf)
library(plotly)

# === Dołącz zakładki z osobnych plików ===
source("wizualizacja.R")
source("pkb.R")
source("zatrudnienie.R")
source("energia.R")
# ----------------------------------------------------------------------------
source("demografia.R")
source("emisje.R")

# === UI ===
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Eurostat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapa", tabName = "wizualizacja", icon = icon("globe-europe")),
      menuItem("Finanse", tabName = "pkb", icon = icon("money-bill")),
      menuItem("Zatrudnienie", tabName = "zatrudnienie", icon = icon("user-tie")),
      menuItem("Energia", tabName = "energia", icon = icon("bolt")),
      menuItem("Demografia", tabName = "demografia", icon = icon("users")),
      menuItem("Emisje CO₂",    tabName = "emisje",        icon = icon("cloud"))
    )
  ),
  dashboardBody(
    tabItems(
      tab_wizualizacja_ui,
      tab_pkb_ui,
      tab_zatrudnienie_ui,
      tab_energia_ui,
      tab_demografia_ui,
      tab_emisje_ui
    )
  )
)

# === SERVER ===
server <- function(input, output, session) {
  
  # Dane dla zakładki PKB
  dane_pkb <- reactive({
    get_eurostat("nama_10_pc", time_format = "date")
  })

  # Dane dla zatrudnienia
  earn_data <- reactive({
    get_eurostat("earn_ses_monthly", time_format = "date", cache = TRUE)
  })

  # Wywołanie funkcji serwerowych poszczególnych zakładek
  tab_wizualizacja_server(input, output, session)
  tab_pkb_server(input, output, session, dane_pkb)
  tab_zatrudnienie_server(input, output, session)
  tab_energia_server(input, output, session)
  tab_demografia_server(input, output, session)
  tab_emisje_server(input, output, session)
}

# === URUCHOM APLIKACJĘ ===
shinyApp(ui, server)
