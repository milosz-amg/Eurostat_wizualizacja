# demografia.R ------------------------------------------------------------
# Zakładka Shiny: Demografia/Populacja

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(eurostat)
library(plotly)

# --- UI -----------------------------------------------------------------

tab_demografia_ui <- tabItem(
  tabName = "demografia",
  h2("Demografia i populacja w krajach UE"),
  fluidRow(
    box(width = 6,
        selectInput("pop_country", "Kraj:",
                    choices = eurostat::eu_countries$code,
                    selected = "PL")
    ),
    box(width = 6,
        sliderInput("pop_years", "Zakres lat:",
                    min = 2000, max = 2023, value = c(2010, 2023), step = 1, sep = "")
    )
  ),
  fluidRow(
    valueBoxOutput("pop_vb_total", width = 3),
    valueBoxOutput("pop_vb_growth", width = 3),
    valueBoxOutput("pop_vb_fertility", width = 3),
    valueBoxOutput("pop_vb_median_age", width = 3)
  ),
  fluidRow(
    box(width = 8, plotlyOutput("pop_trend", height = "400px")),
    box(width = 4, plotlyOutput("pop_pyramid", height = "400px"))
  ),
  fluidRow(
    box(width = 6, plotlyOutput("birth_death_trend", height = "300px")),
    box(width = 6, plotlyOutput("fertility_trend", height = "300px"))
  ),
  br(),
  fluidRow(
    column(4,
           sliderInput("pop_comparison_year", "Rok do porównania krajów UE:",
                       min = 2000, max = 2023, value = 2023, step = 1, sep = "")
    ),
    column(4,
           selectInput("pop_indicator", "Wskaźnik do porównania:",
                       choices = list(
                         "Populacja (mln)" = "population",
                         "Przyrost naturalny (‰)" = "natural_growth",
                         "Współczynnik dzietności" = "fertility",
                         "Wiek mediana" = "median_age"
                       ),
                       selected = "population")
    )
  ),
  fluidRow(
    box(width = 12, plotlyOutput("pop_comparison", height = "400px"))
  )
)

# --- SERVER -------------------------------------------------------------

tab_demografia_server <- function(input, output, session) {

  # Nazwy krajów w języku polskim
  nazwy_krajow_pl <- c(
    AT = "Austria", BE = "Belgia", BG = "Bułgaria", HR = "Chorwacja",
    CY = "Cypr", CZ = "Czechy", DK = "Dania", EE = "Estonia",
    FI = "Finlandia", FR = "Francja", DE = "Niemcy", GR = "Grecja",
    HU = "Węgry", IE = "Irlandia", IT = "Włochy", LV = "Łotwa",
    LT = "Litwa", LU = "Luksemburg", MT = "Malta", NL = "Holandia",
    PL = "Polska", PT = "Portugalia", RO = "Rumunia", SK = "Słowacja",
    SI = "Słowenia", ES = "Hiszpania", SE = "Szwecja", NO = "Norwegia",
    CH = "Szwajcaria", IS = "Islandia", UK = "Wielka Brytania"
  )

  # --- DANE REAKTYWNE ---
  
  # Populacja całkowita
  pop_data <- reactive({
    get_eurostat("demo_pjan", time_format = "date") %>%
      filter(sex == "T", age == "TOTAL") %>%
      select(geo, TIME_PERIOD, values) %>%
      mutate(year = year(TIME_PERIOD)) %>%
      filter(between(year, input$pop_years[1], input$pop_years[2]))
  })

  # Struktura wiekowa
  pop_age_structure <- reactive({
    get_eurostat("demo_pjan", time_format = "date") %>%
      filter(geo == input$pop_country, sex == "T", age != "TOTAL") %>%
      mutate(year = year(TIME_PERIOD)) %>%
      filter(year == input$pop_years[2]) %>%
      mutate(
        age_group = case_when(
          age %in% c("Y_LT5", "Y5-9", "Y10-14") ~ "0-14",
          age %in% c("Y15-19", "Y20-24", "Y25-29", "Y30-34", "Y35-39", 
                     "Y40-44", "Y45-49", "Y50-54", "Y55-59", "Y60-64") ~ "15-64",
          TRUE ~ "65+"
        )
      ) %>%
      group_by(age_group) %>%
      summarise(population = sum(values, na.rm = TRUE), .groups = "drop")
  })

  # Urodzenia i zgony
birth_death_data <- reactive({
  tryCatch({
    get_eurostat("demo_r_gind3", time_format = "date") %>%
      filter(geo == input$pop_country, indic_de == "NATGRO") %>%
      mutate(year = year(TIME_PERIOD)) %>%
      select(geo, TIME_PERIOD, natural_growth = values, year) %>%
      filter(between(year, input$pop_years[1], input$pop_years[2]))
  }, error = function(e) {
    data.frame(geo = character(), TIME_PERIOD = as.Date(character()), 
               natural_growth = numeric(), year = numeric())
  })
})


  # Dzietność
fertility_data <- reactive({
  tryCatch({
    get_eurostat("demo_tfr", time_format = "date") %>%
      filter(geo == input$pop_country) %>%
      select(geo, TIME_PERIOD, fertility = values) %>%
      mutate(year = year(TIME_PERIOD)) %>%
      filter(between(year, input$pop_years[1], input$pop_years[2]))
  }, error = function(e) {
    data.frame(geo = character(), TIME_PERIOD = as.Date(character()), 
               fertility = numeric(), year = numeric())
  })
})


  # Wiek mediana
  median_age_data <- reactive({
    tryCatch({
      get_eurostat("demo_pjanind", time_format = "date") %>%
        filter(geo == input$pop_country, indic_de == "MEDAGEPOP") %>%
        select(geo, TIME_PERIOD, median_age = values) %>%
        mutate(year = year(TIME_PERIOD)) %>%
        filter(between(year, input$pop_years[1], input$pop_years[2]))
    }, error = function(e) {
      data.frame(geo = character(), TIME_PERIOD = as.Date(character()), 
                median_age = numeric(), year = numeric())
    })
  })

  # --- VALUE BOXES ---
  
  output$pop_vb_total <- renderValueBox({
    dane <- pop_data() %>% filter(geo == input$pop_country)
    latest <- tail(dane, 1)
    if (nrow(latest) == 0 || is.na(latest$values)) {
      return(valueBox("Brak danych", subtitle = "Populacja", icon = icon("users"), color = "blue"))
    }
    pop_mln <- round(latest$values / 1000000, 2)
    valueBox(
      paste0(pop_mln, " mln"),
      subtitle = paste("Populacja", latest$year),
      icon = icon("users"),
      color = "blue"
    )
  })

  output$pop_vb_growth <- renderValueBox({
    dane <- birth_death_data()
    if (nrow(dane) == 0) {
      return(valueBox("Brak danych", subtitle = "Przyrost naturalny", icon = icon("chart-line"), color = "green"))
    }
    latest <- tail(dane, 1)
    if (is.na(latest$natural_growth)) {
      return(valueBox("Brak danych", subtitle = "Przyrost naturalny", icon = icon("chart-line"), color = "green"))
    }
    valueBox(
      sprintf("%.1f ‰", latest$natural_growth),
      subtitle = paste("Przyrost naturalny", latest$year),
      icon = icon("chart-line"),
      color = ifelse(latest$natural_growth >= 0, "green", "red")
    )
  })

  output$pop_vb_fertility <- renderValueBox({
    dane <- fertility_data()
    if (nrow(dane) == 0) {
      return(valueBox("Brak danych", subtitle = "Współczynnik dzietności", icon = icon("baby"), color = "orange"))
    }
    latest <- tail(dane, 1)
    if (is.na(latest$fertility)) {
      return(valueBox("Brak danych", subtitle = "Współczynnik dzietności", icon = icon("baby"), color = "orange"))
    }
    valueBox(
      sprintf("%.2f", latest$fertility),
      subtitle = paste("Dzietność", latest$year),
      icon = icon("baby"),
      color = ifelse(latest$fertility >= 2.1, "green", "orange")
    )
  })

  output$pop_vb_median_age <- renderValueBox({
    dane <- median_age_data()
    if (nrow(dane) == 0) {
      return(valueBox("Brak danych", subtitle = "Wiek mediana", icon = icon("calendar"), color = "purple"))
    }
    latest <- tail(dane, 1)
    if (is.na(latest$median_age)) {
      return(valueBox("Brak danych", subtitle = "Wiek mediana", icon = icon("calendar"), color = "purple"))
    }
    valueBox(
      sprintf("%.1f lat", latest$median_age),
      subtitle = paste("Wiek mediana", latest$year),
      icon = icon("calendar"),
      color = "purple"
    )
  })

  # --- WYKRESY ---

  # Trend populacji
  output$pop_trend <- renderPlotly({
    dane <- pop_data() %>% filter(geo == input$pop_country)
    if (nrow(dane) == 0) {
      return(plot_ly() %>% layout(title = "Brak danych do wyświetlenia"))
    }
    
    dane$pop_mln <- dane$values / 1000000
    
    plot_ly(
      data = dane,
      x = ~TIME_PERIOD,
      y = ~pop_mln,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "steelblue", width = 2),
      marker = list(size = 6)
    ) %>%
      layout(
        title = paste("Trend populacji -", nazwy_krajow_pl[[input$pop_country]] %||% input$pop_country),
        xaxis = list(title = "Rok"),
        yaxis = list(title = "Populacja (mln)")
      )
  })

  # Piramida wieku
  output$pop_pyramid <- renderPlotly({
    dane <- pop_age_structure()
    if (nrow(dane) == 0) {
      return(plot_ly() %>% layout(title = "Brak danych o strukturze wiekowej"))
    }
    
    plot_ly(
      data = dane,
      labels = ~age_group,
      values = ~population,
      type = "pie",
      textinfo = "label+percent",
      textposition = "inside"
    ) %>%
      layout(
        title = paste("Struktura wiekowa", input$pop_years[2]),
        legend = list(orientation = "v", x = 1.1, y = 0.5)
      )
  })

  # Trend urodzeń i zgonów
  output$birth_death_trend <- renderPlotly({
    dane <- birth_death_data()
    if (nrow(dane) == 0) {
      return(plot_ly() %>% layout(title = "Brak danych o urodzeniach i zgonach"))
    }
    
    p <- plot_ly(data = dane, x = ~TIME_PERIOD) %>%
      add_trace(y = ~births, type = "scatter", mode = "lines+markers", 
                name = "Urodzenia (‰)", line = list(color = "green")) %>%
      add_trace(y = ~deaths, type = "scatter", mode = "lines+markers", 
                name = "Zgony (‰)", line = list(color = "red")) %>%
      add_trace(y = ~natural_growth, type = "scatter", mode = "lines+markers", 
                name = "Przyrost naturalny (‰)", line = list(color = "blue"))
    
    p %>% layout(
      title = "Urodzenia, zgony i przyrost naturalny",
      xaxis = list(title = "Rok"),
      yaxis = list(title = "Na 1000 mieszkańców"),
      legend = list(x = 0, y = 1)
    )
  })

  # Trend dzietności
  output$fertility_trend <- renderPlotly({
    dane <- fertility_data()
    if (nrow(dane) == 0) {
      return(plot_ly() %>% layout(title = "Brak danych o dzietności"))
    }
    
    plot_ly(
      data = dane,
      x = ~TIME_PERIOD,
      y = ~fertility,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "orange", width = 2),
      marker = list(size = 6)
    ) %>%
      add_hline(y = 2.1, line = list(color = "red", dash = "dash"), 
                annotation = list(text = "Poziom zastępowalności (2.1)", x = 0.5, y = 2.1)) %>%
      layout(
        title = "Współczynnik dzietności",
        xaxis = list(title = "Rok"),
        yaxis = list(title = "Dzieci na kobietę")
      )
  })

  # Porównanie krajów
  output$pop_comparison <- renderPlotly({
    rok <- input$pop_comparison_year
    indicator <- input$pop_indicator
    
    if (indicator == "population") {
      dane <- pop_data() %>%
      filter(year == rok) %>%
      mutate(value = values / 1000000, unit = "mln") %>%
      arrange(desc(value)) %>%
      slice(-1:-3)  # usuń 3 największe

      title_text <- paste("Populacja w krajach UE -", rok)
      y_title <- "Populacja (mln)"
      
    } else if (indicator == "natural_growth") {
      dane <- get_eurostat("demo_gind", time_format = "date") %>%
        filter(indic_de %in% c("GRATE", "DRATE")) %>%
        mutate(year = year(TIME_PERIOD)) %>%
        filter(year == rok) %>%
        pivot_wider(names_from = indic_de, values_from = values) %>%
        mutate(value = GRATE - DRATE, unit = "‰") %>%
        select(geo, value, unit) %>%
        na.omit() %>%
        arrange(desc(value))
      title_text <- paste("Przyrost naturalny w krajach UE -", rok)
      y_title <- "Przyrost naturalny (‰)"
      
    } else if (indicator == "fertility") {
      dane <- get_eurostat("demo_tfr", time_format = "date") %>%
        mutate(year = year(TIME_PERIOD)) %>%
        filter(year == rok) %>%
        mutate(value = values, unit = "dzieci/kobieta") %>%
        select(geo, value, unit) %>%
        na.omit() %>%
        arrange(desc(value))
      title_text <- paste("Współczynnik dzietności w krajach UE -", rok)
      y_title <- "Dzieci na kobietę"
      
    } else if (indicator == "median_age") {
      dane <- tryCatch({
        get_eurostat("demo_pjanind", time_format = "date") %>%
          filter(indic_de == "MEDAGEPOP") %>%
          mutate(year = year(TIME_PERIOD)) %>%
          filter(year == rok) %>%
          mutate(value = values, unit = "lat") %>%
          select(geo, value, unit) %>%
          na.omit() %>%
          arrange(desc(value))
      }, error = function(e) {
        data.frame(geo = character(), value = numeric(), unit = character())
      })
      title_text <- paste("Wiek mediana w krajach UE -", rok)
      y_title <- "Wiek mediana (lat)"
    }
    
    if (nrow(dane) == 0) {
      return(plot_ly() %>% layout(title = "Brak danych do wyświetlenia"))
    }
    
    # Dodaj nazwy krajów
    dane$nazwa <- nazwy_krajow_pl[dane$geo]
    dane$nazwa[is.na(dane$nazwa)] <- dane$geo[is.na(dane$nazwa)]
    
    plot_ly(
      data = dane,
      x = ~reorder(nazwa, value),
      y = ~value,
      type = "bar",
      text = ~paste0("Kraj: ", nazwa, "<br>Wartość: ", sprintf("%.2f", value), " ", unit),
      hoverinfo = "text",
      marker = list(color = 'rgba(70,130,180,0.7)', 
                   line = list(color = 'rgba(70,130,180,1)', width = 1.5))
    ) %>%
      layout(
        title = title_text,
        xaxis = list(title = "Kraj", tickangle = -45),
        yaxis = list(title = y_title),
        bargap = 0.2
      )
  })
}