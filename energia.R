# energia.R ------------------------------------------------------------
# Zakładka Shiny: Energia/OZE

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(eurostat)
library(plotly)


oze_data_all <- readRDS("data/oze_data.rds")
oze_struct_all <- readRDS("data/oze_struct_all.rds")
oze_sources_all <- readRDS("data/oze_sources_by_product.rds")

# --- UI -----------------------------------------------------------------

tab_energia_ui <- tabItem(
  tabName = "energia",
  h2("Odnawialne źródła energii (OZE) w krajach UE"),
  fluidRow(
    box(width = 6,
        selectInput("energy_country", "Kraj:",
                    choices = eurostat::eu_countries$code,
                    selected = "PL")),
    box(width = 6,
        sliderInput("energy_years", "Zakres lat:",
                    min = 2004, max = 2022, value = c(2010, 2022),
                    step = 1, sep = ""))
  ),
  fluidRow(
    valueBoxOutput("oze_vb_share",   width = 4),
    valueBoxOutput("oze_vb_eu_goal", width = 4),
    valueBoxOutput("oze_vb_change",  width = 4)
  ),
  fluidRow(box(width = 12, plotlyOutput("oze_trend", height = "400px"))),
  fluidRow(
    box(width = 6, plotlyOutput("oze_pie",         height = "300px")),
    box(width = 6, plotlyOutput("oze_pie_sources", height = "300px"))
  ),
  br(),
  fluidRow(
    column(4,
           sliderInput("oze_bar_year", "Rok do porównania krajów UE:",
                       min = 2004, max = 2022, value = 2022, step = 1, sep = ""))
  ),
  fluidRow(box(width = 12, plotlyOutput("oze_bar_countries", height = "400px")))
)

# --------------------------------------------------------------------------
#  (4) SERVER
# --------------------------------------------------------------------------

tab_energia_server <- function(input, output, session) {

  # 4-a) Trend data --------------------------------------------------------
  oze_data <- reactive({
    oze_data_all |>
      filter(unit == "PC", nrg_bal == "REN") |>
      select(geo, TIME_PERIOD, values) |>
      mutate(TIME_PERIOD = as.Date(TIME_PERIOD))
  })

  oze_trend <- reactive({
    req(oze_data())
    oze_data() |>
      filter(
        geo == input$energy_country,
        between(year(TIME_PERIOD), input$energy_years[1], input$energy_years[2])
      ) |>
      arrange(TIME_PERIOD)
  })

  # 4-b) ValueBoxes --------------------------------------------------------
  output$oze_vb_share <- renderValueBox({
    latest <- tail(oze_trend(), 1)
    if (nrow(latest) == 0 || is.na(latest$values)) {
      return(valueBox("Brak danych", "Udział OZE", icon("ban"), color = "black"))
    }
    valueBox(sprintf("%.1f %%", latest$values),
             paste("Udział OZE w", year(latest$TIME_PERIOD)),
             icon = icon("seedling"), color = "green")
  })

  output$oze_vb_change <- renderValueBox({
    dane <- oze_trend()
    if (nrow(dane) < 2) {
      return(valueBox("Brak danych", "Zmiana udziału OZE", icon("ban"), color = "black"))
    }
    delta <- tail(dane, 1)$values - head(dane, 1)$values
    valueBox(sprintf("%+.1f p.p.", delta),
             paste("Zmiana", input$energy_years[1], "-", input$energy_years[2]),
             icon = icon("chart-line"),
             color = ifelse(delta >= 0, "green", "red"))
  })

  output$oze_vb_eu_goal <- renderValueBox({
    latest_val <- tail(oze_trend(), 1)$values
    cel_ue  <- 32
    rok_cel <- 2030
    if (is.na(latest_val)) {
      return(valueBox("Brak danych",
                      paste("Cel UE", cel_ue, "% w", rok_cel),
                      icon("ban"), color = "black"))
    }
    valueBox(paste0(cel_ue, " %"),
             paste("Cel UE do", rok_cel,
                   "– różnica:", sprintf("%+.1f p.p.", latest_val - cel_ue)),
             icon = icon("flag"),
             color = ifelse(latest_val >= cel_ue, "green", "orange"))
  })

  # 4-c) Trend line plot ---------------------------------------------------
  output$oze_trend <- renderPlotly({
    dane <- oze_trend()
    if (nrow(dane) == 0) {
      return(plot_ly() |> layout(title = "Brak danych do wyświetlenia"))
    }
    plot_ly(dane, x = ~TIME_PERIOD, y = ~values,
            type = "scatter", mode = "lines+markers",
            line = list(color = "forestgreen", width = 2),
            marker = list(size = 8)) |>
      layout(title = paste("Udział OZE:", input$energy_country),
             xaxis = list(title = "Rok"), yaxis = list(title = "%"))
  })

  # 4-d) Sector pie (unchanged) -------------------------------------------
  output$oze_pie <- renderPlotly({
    dane <- oze_data_all |>
      filter(unit == "PC",
             geo  == input$energy_country,
             year(TIME_PERIOD) == input$energy_years[2],
             nrg_bal %in% c("REN_ELC", "REN_HEAT_CL", "REN_TRA"))
    if (nrow(dane) == 0)
      return(plot_ly() |> layout(title = "Brak danych o sektorach OZE"))

    pie_dane <- dane |>
      mutate(sektor = recode(nrg_bal,
                             REN_ELC    = "Energia elektryczna",
                             REN_HEAT_CL = "Ogrzewanie i chłodzenie",
                             REN_TRA    = "Transport")) |>
      select(sektor, values)

    plot_ly(pie_dane, labels = ~sektor, values = ~values,
            type = "pie", textinfo = "label+percent",
            textposition = "inside", insidetextorientation = "radial") |>
      layout(title = paste("Udział OZE według sektora –",
                           input$energy_country, input$energy_years[2]),
             legend = list(orientation = "h", x = 0.1, y = -0.1))
  })

  # 4-e) Country bar -------------------------------------------------------
  output$oze_bar_countries <- renderPlotly({
    dane <- oze_data() |>
      filter(year(TIME_PERIOD) == input$oze_bar_year) |>
      select(geo, values) |>
      left_join(eurostat::eu_countries, by = c("geo" = "code")) |>
      arrange(desc(values))
    plot_ly(dane, x = ~reorder(name, values), y = ~values,
            type = "bar",
            text = ~sprintf("Kraj: %s<br>OZE: %.1f%%", name, values),
            hoverinfo = "text",
            marker = list(color = "rgba(34,139,34,0.7)",
                          line   = list(color = "rgba(34,139,34,1)", width = 1.5))) |>
      layout(title = paste("Udział OZE w UE –", input$oze_bar_year),
             xaxis = list(title = "Kraj", tickangle = -45),
             yaxis = list(title = "%"), bargap = 0.2)
  })

  # 4-f) Grouped-source pie -----------------------------------------------

  # ── Stała kolejność kategorii i jednolita paleta ─────────────────────────
  src_levels <- c("Wiatr",
                  "Słońce",
                  "Woda",
                  "Geotermia",
                  "Ciepło otoczenia",
                  "Pływy/Ocean",
                  "Biomasa drzewna",
                  "Biomasa inna",
                  "Biogaz",
                  "Biopaliwa ciekłe")

  src_cols <- c("#1f77b4",  # Wiatr
                "#2ca02c",  # Słońce
                "#0072B2",  # Woda
                "#d55e00",  # Geotermia
                "#cc79a7",  # Ciepło otoczenia
                "#17becf",  # Pływy/Ocean
                "#ff7f0e",  # Biomasa drzewna
                "#8c564b",  # Biomasa inna
                "#9467bd",  # Biogaz
                "#7f7f7f")  # Biopaliwa ciekłe

  # 4-f) Grouped-source pie  -------------------------------------------------
  oze_sources_data <- reactive({
    req(input$energy_country, input$energy_years)

    oze_sources_all |>
      filter(geo == input$energy_country,
            year(TIME_PERIOD) == input$energy_years[2]) |>
      mutate(grupa = case_when(
        grepl("^RA300", siec)             ~ "Wiatr",
        grepl("^RA410|^RA420", siec)      ~ "Słońce",
        grepl("^RA100|^RA130", siec)      ~ "Woda",
        grepl("^RA200", siec)             ~ "Geotermia",
        grepl("^RA600|^RA620", siec)      ~ "Ciepło otoczenia",
        grepl("^RA500", siec)             ~ "Pływy/Ocean",
        grepl("^R511", siec)              ~ "Biomasa drzewna",
        grepl("^R512|^R514|^R515", siec)  ~ "Biomasa inna",
        grepl("^R530|^R531|^R532", siec)  ~ "Biogaz",
        grepl("^R52",  siec)              ~ "Biopaliwa ciekłe",
        TRUE                              ~ NA_character_
      )) |>
      filter(!is.na(grupa)) |>
      group_by(grupa) |>
      summarise(energia = sum(values, na.rm = TRUE), .groups = "drop") |>
      mutate(grupa = factor(grupa, levels = src_levels)) |>
      arrange(grupa)                      # pilnuje kolejności = kolejność palety
  })

  output$oze_pie_sources <- renderPlotly({
    dane <- oze_sources_data()
    if (nrow(dane) == 0)
      return(plot_ly() |> layout(title = "Brak danych o źródłach OZE"))

    plot_ly(dane,
            labels = ~grupa,
            values = ~energia,
            type   = "pie",
            textinfo = "none",
            hoverinfo = "label+percent",
            marker = list(colors = src_cols[levels(dane$grupa)])) |>
      layout(title  = paste("Źródła OZE –",
                            input$energy_country, input$energy_years[2]),
            legend = list(orientation = "h", x = 0.1, y = -0.1))
  })
}