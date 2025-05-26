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

# === UI ===
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Eurostat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Wizualizacja", tabName = "wizualizacja", icon = icon("chart-line")),
      menuItem("Finanse", tabName = "pkb", icon = icon("money-bill")),
      menuItem("Zatrudnienie", tabName = "zatrudnienie", icon = icon("user-tie")),
      menuItem("Energia", tabName = "energia", icon = icon("bolt"))
    )
  ),
  dashboardBody(
    tabItems(
      tab_wizualizacja_ui,
      tab_pkb_ui,
      tab_zatrudnienie_ui,
      tab_energia_ui
    )
  )
)

# === SERVER ===
server <- function(input, output, session) {
  dane_pkb <- reactive({
    get_eurostat("nama_10_pc", time_format = "date")
  })
  earn_data <- reactive({
  eurostat::get_eurostat("earn_ses_monthly", time_format = "date", cache = TRUE)
})


  tab_wizualizacja_server(input, output, session, dane_pkb)
  tab_pkb_server(input, output, session, dane_pkb)
  tab_zatrudnienie_server(input, output, session)
  tab_energia_server(input, output, session)
}

shinyApp(ui, server)
