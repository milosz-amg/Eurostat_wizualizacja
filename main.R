library(shiny)
library(shinydashboard)
library(DT)
library(eurostat)
library(dplyr)
library(leaflet)
library(sf)

# === Dołącz zakładki z osobnych plików ===
source("wizualizacja.R")
source("pkb.R")

# === UI ===
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Eurostat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Wizualizacja", tabName = "wizualizacja", icon = icon("chart-line")),
      menuItem("Finanse", tabName = "pkb", icon = icon("money-bill"))
    )
  ),
  dashboardBody(
    tabItems(
      tab_wizualizacja_ui,
      tab_pkb_ui
    )
  )
)

# === SERVER ===
server <- function(input, output, session) {
  dane_pkb <- reactive({
    get_eurostat("nama_10_pc", time_format = "date")
  })

  tab_wizualizacja_server(input, output, session)
  tab_pkb_server(input, output, session, dane_pkb)
}

shinyApp(ui, server)
