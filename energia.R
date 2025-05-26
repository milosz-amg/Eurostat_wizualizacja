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

# --- UI -----------------------------------------------------------------

tab_energia_ui <- tabItem(
  tabName = "energia",
  h2("Odnawialne źródła energii (OZE) w krajach UE"),
  fluidRow(
    box(width = 6,
        selectInput("energy_country", "Kraj:",
                    choices = eurostat::eu_countries$code,
                    selected = "PL")
    ),
    box(width = 6,
        sliderInput("energy_years", "Zakres lat:",
                    min = 2004, max = 2022, value = c(2010, 2022), step = 1, sep = "")
    )
  ),
  fluidRow(
    valueBoxOutput("oze_vb_share", width = 4),
    valueBoxOutput("oze_vb_eu_goal", width = 4),
    valueBoxOutput("oze_vb_change", width = 4)
  ),
  fluidRow(
    box(width = 812, plotlyOutput("oze_trend", height = "400px")),
  ),
  fluidRow(
    box(width = 6, plotlyOutput("oze_pie", height = "300px")),           # sektor
    box(width = 6, plotlyOutput("oze_pie_sources", height = "300px"))    # źródła
  ),
  br(),
  fluidRow(
    column(4,
           sliderInput("oze_bar_year", "Rok do porównania krajów UE:",
                       min = 2004, max = 2022, value = 2022, step = 1, sep = "")
    )
  ),
  fluidRow(
    box(width = 12, plotlyOutput("oze_bar_countries", height = "400px"))
  )
)

# --- SERVER -------------------------------------------------------------

tab_energia_server <- function(input, output, session) {

  oze_data_all <- readRDS("data/oze_data.rds")
  oze_struct_all <- readRDS("data/oze_struct_all.rds")
  oze_sources_all <- readRDS("data/oze_sources_by_product.rds")


  siec_labels <- c(
    "RA200"   = "Geothermal",
    "RA410"   = "Solar thermal",
    "RA600"   = "Ambient heat (heat pumps)",
    "W6000RI" = "Renewable fraction of industrial waste",
    "R5110"   = "Fuelwood, wood residues and byproducts",
    "R5120"   = "Bagasse",
    "R5130"   = "Animal waste",
    "R5140"   = "Black liquor",
    "R5150"   = "Other vegetal material and residues",
    "R5200"   = "Liquid biofuels",
    "R5210E"  = "Bioethanol",
    "R5210P"  = "Pure biogasoline",
    "R5220P"  = "Pure biodiesels",
    "R5230P"  = "Pure bio jet kerosene",
    "R5290"   = "Other liquid biofuels",
    "R5300"   = "Biogases",
    "R5310"   = "Biogases from anaerobic fermentation",
    "R5311"   = "Landfill gas",
    "R5312"   = "Sewage sludge gas",
    "R5320"   = "Biogases from thermal processes",
    "R5110-5150_W6000RI" = "Primary solid biofuels",
    "W6100" = "Industrial waste (non-renewable)",
    "W6220" = "Non-renewable municipal waste",
    "W6210" = "Renewable municipal waste",
    "R5319" = "Other biogases from anaerobic fermentation"
  )



  oze_data <- reactive({
    oze_data_all %>%
      filter(unit == "PC", nrg_bal == "REN") %>%
      select(geo, TIME_PERIOD, values) %>%
      mutate(TIME_PERIOD = as.Date(TIME_PERIOD))
  })


  oze_struct_data <- reactive({
    oze_struct_all %>%
      filter(
        geo == input$energy_country,
        year(TIME_PERIOD) == input$energy_years[2]
      )
  })


  # --- Trend (liniowy) ---
  oze_trend <- reactive({
    req(oze_data())
    oze_data() %>%
      filter(
        geo == input$energy_country,
        between(year(TIME_PERIOD), input$energy_years[1], input$energy_years[2])
      ) %>%
      arrange(TIME_PERIOD)
  })

  oze_sources_data <- reactive({
    req(input$energy_country, input$energy_years)

  oze_sources_all %>%
    filter(
      geo == input$energy_country,
      year(TIME_PERIOD) == input$energy_years[2],
      grepl("^R", siec)  # tylko odnawialne źródła
    ) %>%
      mutate(źródło = recode(siec, !!!siec_labels)) %>%
      group_by(źródło) %>%
      summarise(energia = sum(values, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(źródło), energia > 0)
  })

  # --- ValueBox: udział OZE w finalnym zużyciu energii (ostatni rok) ---
  output$oze_vb_share <- renderValueBox({
    dane <- oze_trend()
    latest <- tail(dane, 1)
    if (nrow(latest) == 0 || is.na(latest$values)) {
      return(valueBox("Brak danych", subtitle = "Udział OZE", icon = icon("ban"), color = "black"))
    }
    valueBox(
      sprintf("%.1f %%", latest$values),
      subtitle = paste("Udział OZE w", year(latest$TIME_PERIOD)),
      icon = icon("seedling"),
      color = "green"
    )
  })

  # --- ValueBox: Zmiana udziału OZE (początek vs koniec zakresu) ---
  output$oze_vb_change <- renderValueBox({
    dane <- oze_trend()
    if (nrow(dane) < 2) {
      return(valueBox("Brak danych", subtitle = "Zmiana udziału OZE", icon = icon("ban"), color = "black"))
    }
    delta <- tail(dane, 1)$values - head(dane, 1)$values
    valueBox(
      sprintf("%+.1f p.p.", delta),
      subtitle = paste("Zmiana", input$energy_years[1], "-", input$energy_years[2]),
      icon = icon("chart-line"),
      color = ifelse(delta >= 0, "green", "red")
    )
  })

  # --- ValueBox: Porównanie z celem UE ---
  output$oze_vb_eu_goal <- renderValueBox({
    latest_val <- tail(oze_trend(), 1)$values
    cel_ue <- 32.0
    rok_celu <- 2030
    if (is.na(latest_val)) {
      return(valueBox("Brak danych", subtitle = paste("Cel UE", cel_ue, "% w", rok_celu), icon = icon("ban"), color = "black"))
    }
    valueBox(
      paste0(sprintf("%.1f", cel_ue), " %"),
      subtitle = paste("Cel UE do", rok_celu, "– różnica:", sprintf("%+.1f p.p.", latest_val - cel_ue)),
      icon = icon("flag"),
      color = ifelse(latest_val >= cel_ue, "green", "orange")
    )
  })

  # --- Wykres trendu ---
  output$oze_trend <- renderPlotly({
    dane <- oze_trend()
    if (nrow(dane) == 0) {
      plot_ly() %>% layout(title = "Brak danych do wyświetlenia")
    } else {
      plot_ly(
        x = dane$TIME_PERIOD,
        y = dane$values,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "forestgreen", width = 2),
        marker = list(size = 8)
      ) %>%
        layout(
          title = sprintf("Udział OZE: %s", input$energy_country),
          xaxis = list(title = "Rok"),
          yaxis = list(title = "%")
        )
    }
  })

  # --- Wykres słupkowy: Porównanie krajów UE ---
    output$oze_bar_countries <- renderPlotly({
    dane <- oze_data()
    rok <- input$oze_bar_year
    dane_rok <- dane %>%
        filter(year(TIME_PERIOD) == rok) %>%
        select(geo, values) %>%
        na.omit()
    countries_labels <- eurostat::eu_countries %>% dplyr::select(code, name)
    dane_rok <- dane_rok %>% left_join(countries_labels, by = c("geo" = "code")) %>%
        arrange(desc(values))
    
    p <- plot_ly(
        data = dane_rok,
        x = ~reorder(name, values),
        y = ~values,
        type = "bar",
        text = ~paste0("Kraj: ", name, "<br>OZE: ", sprintf("%.1f", values), "%"),
        hoverinfo = "text",
        marker = list(color = 'rgba(34,139,34,0.7)', line = list(color = 'rgba(34,139,34,1)', width = 1.5)),
        source = "bar"
    ) %>%
        layout(
        title = paste("Udział OZE w UE w roku", rok),
        xaxis = list(title = "Kraj", tickangle = -45),
        yaxis = list(title = "%"),
        bargap = 0.2
        )
    
    event_register(p, 'plotly_click')
    })

  # --- Wykres kołowy: Struktura OZE ---
  output$oze_pie <- renderPlotly({
    dane <- oze_data_all %>%
      filter(unit == "PC") %>%
      filter(geo == input$energy_country) %>%
      mutate(rok = year(TIME_PERIOD)) %>%
      filter(rok == input$energy_years[2]) %>%
      filter(nrg_bal %in% c("REN_ELC", "REN_HEAT_CL", "REN_TRA"))

    if (nrow(dane) == 0) {
      return(plot_ly() %>% layout(title = "Brak danych o sektorach OZE"))
    }

    pie_dane <- dane %>%
      mutate(sektor = recode(nrg_bal,
                            "REN_ELC" = "Energia elektryczna",
                            "REN_HEAT_CL" = "ogrzewanie i chłodzenie",
                            "REN_TRA" = "Transport")) %>%
      select(sektor, values)

    plot_ly(
      data = pie_dane,
      labels = ~sektor,
      values = ~values,
      type = "pie",
      textinfo = "label+percent",
      textposition = "inside",
      insidetextorientation = "radial",
      hoverinfo = "label+percent"
    ) %>%
      layout(
        title = paste("Udział OZE według sektora –", input$energy_country, input$energy_years[2]),
        legend = list(orientation = "h", x = 0.1, y = -0.1)
      )
  })

  # --- Wykres kołowy: Struktura OZE według źródeł ---
  oze_sources_data <- reactive({
    req(input$energy_country, input$energy_years)

  oze_sources_all %>%
    filter(
      geo == input$energy_country,
      year(TIME_PERIOD) == input$energy_years[2],
      grepl("^R", siec)  # tylko odnawialne źródła
    ) %>%
      mutate(źródło = recode(siec, !!!siec_labels)) %>%
      group_by(źródło) %>%
      summarise(energia = sum(values, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(źródło), energia > 0)
  })

  # Wykres
  output$oze_pie_sources <- renderPlotly({
    dane <- oze_sources_data()
    if (nrow(dane) == 0) {
      return(plot_ly() %>% layout(title = "Brak danych o źródłach OZE"))
    }

    plot_ly(
      data = dane,
      labels = ~źródło,
      values = ~energia,
      type = "pie",
      textinfo = "none",
      insidetextorientation = "radial"
    ) %>%
      layout(
        title = paste("Źródła OZE –", input$energy_country, input$energy_years[2]),
        legend = list(orientation = "h", x = 0.1, y = -0.1)
      )
  })
}

