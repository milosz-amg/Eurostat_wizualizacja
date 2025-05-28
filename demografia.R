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
    box(width = 12, plotlyOutput("pop_trend", height = "400px"))
  ),
  fluidRow(
    box(width = 12, plotlyOutput("birth_death_trend", height = "300px")),
    # box(width = 6, plotlyOutput("fertility_trend", height = "300px"))
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

  # Urodzenia i zgony - POPRAWIONE - używa różnych tabel z Eurostat
  birth_death_data <- reactive({
    # Spróbuj kilka różnych źródeł danych

    # Opcja 1: demo_r_gind3 - najbardziej kompletne dane
    result1 <- tryCatch({
      births <- get_eurostat("demo_r_gind3", time_format = "date") %>%
        filter(geo == input$pop_country, indic_de == "GBIRTHRT") %>%
        mutate(year = year(TIME_PERIOD)) %>%
        select(geo, TIME_PERIOD, births = values, year) %>%
        filter(between(year, input$pop_years[1], input$pop_years[2]))

      deaths <- get_eurostat("demo_r_gind3", time_format = "date") %>%
        filter(geo == input$pop_country, indic_de == "GDEATHRT") %>%
        mutate(year = year(TIME_PERIOD)) %>%
        select(geo, TIME_PERIOD, deaths = values, year) %>%
        filter(between(year, input$pop_years[1], input$pop_years[2]))

      natural_growth <- get_eurostat("demo_r_gind3", time_format = "date") %>%
        filter(geo == input$pop_country, indic_de == "NATGRO") %>%
        mutate(year = year(TIME_PERIOD)) %>%
        select(geo, TIME_PERIOD, natural_growth = values, year) %>%
        filter(between(year, input$pop_years[1], input$pop_years[2]))

      # Połącz wszystkie dane
      merged <- births %>%
        full_join(deaths, by = c("geo", "TIME_PERIOD", "year")) %>%
        full_join(natural_growth, by = c("geo", "TIME_PERIOD", "year")) %>%
        arrange(TIME_PERIOD)

      # Jeśli nie ma bezpośrednich danych o przyroście, oblicz z różnicy
      if (all(is.na(merged$natural_growth)) && !all(is.na(merged$births)) && !all(is.na(merged$deaths))) {
        merged$natural_growth <- merged$births - merged$deaths
      }

      return(merged)
    }, error = function(e) NULL)

    if (!is.null(result1) && nrow(result1) > 0) return(result1)

    # Opcja 2: demo_gind z innymi kodami wskaźników
    result2 <- tryCatch({
      births <- get_eurostat("demo_gind", time_format = "date") %>%
        filter(geo == input$pop_country, indic_de == "BIRTH") %>%
        mutate(year = year(TIME_PERIOD)) %>%
        select(geo, TIME_PERIOD, births = values, year) %>%
        filter(between(year, input$pop_years[1], input$pop_years[2]))

      deaths <- get_eurostat("demo_gind", time_format = "date") %>%
        filter(geo == input$pop_country, indic_de == "DEATH") %>%
        mutate(year = year(TIME_PERIOD)) %>%
        select(geo, TIME_PERIOD, deaths = values, year) %>%
        filter(between(year, input$pop_years[1], input$pop_years[2]))

      merged <- full_join(births, deaths, by = c("geo", "TIME_PERIOD", "year")) %>%
        mutate(natural_growth = births - deaths) %>%
        arrange(TIME_PERIOD)

      return(merged)
    }, error = function(e) NULL)

    if (!is.null(result2) && nrow(result2) > 0) return(result2)

    # Opcja 3: Tylko przyrost naturalny z demo_r_gind3
    result3 <- tryCatch({
      get_eurostat("demo_r_gind3", time_format = "date") %>%
        filter(geo == input$pop_country, indic_de == "NATGRO") %>%
        mutate(year = year(TIME_PERIOD)) %>%
        select(geo, TIME_PERIOD, natural_growth = values, year) %>%
        filter(between(year, input$pop_years[1], input$pop_years[2])) %>%
        mutate(births = NA_real_, deaths = NA_real_) %>%
        arrange(TIME_PERIOD)
    }, error = function(e) NULL)

    if (!is.null(result3) && nrow(result3) > 0) return(result3)

    # Opcja 4: Spróbuj demo_gind z różnymi kodami
    result4 <- tryCatch({
      # Sprawdź jakie wskaźniki są dostępne
      available_indicators <- get_eurostat("demo_gind", time_format = "date") %>%
        filter(geo == input$pop_country) %>%
        distinct(indic_de) %>%
        pull(indic_de)

      # Znajdź odpowiednie wskaźniki
      birth_codes <- c("GBIRTHRT", "BIRTH_RT", "CBR", "GRATE")
      death_codes <- c("GDEATHRT", "DEATH_RT", "CDR", "DRATE")

      birth_code <- intersect(birth_codes, available_indicators)[1]
      death_code <- intersect(death_codes, available_indicators)[1]

      if (!is.na(birth_code) && !is.na(death_code)) {
        births <- get_eurostat("demo_gind", time_format = "date") %>%
          filter(geo == input$pop_country, indic_de == birth_code) %>%
          mutate(year = year(TIME_PERIOD)) %>%
          select(geo, TIME_PERIOD, births = values, year) %>%
          filter(between(year, input$pop_years[1], input$pop_years[2]))

        deaths <- get_eurostat("demo_gind", time_format = "date") %>%
          filter(geo == input$pop_country, indic_de == death_code) %>%
          mutate(year = year(TIME_PERIOD)) %>%
          select(geo, TIME_PERIOD, deaths = values, year) %>%
          filter(between(year, input$pop_years[1], input$pop_years[2]))

        merged <- full_join(births, deaths, by = c("geo", "TIME_PERIOD", "year")) %>%
          mutate(natural_growth = births - deaths) %>%
          arrange(TIME_PERIOD)

        return(merged)
      }

      return(NULL)
    }, error = function(e) NULL)

    if (!is.null(result4) && nrow(result4) > 0) return(result4)

    # Jeśli nic nie działa, zwróć pustą ramkę danych
    return(data.frame(
      geo = character(),
      TIME_PERIOD = as.Date(character()),
      births = numeric(),
      deaths = numeric(),
      natural_growth = numeric(),
      year = numeric()
    ))
  })

  # Dzietność - UPROSZCZONE z lepszą obsługą błędów
  fertility_data <- reactive({
    # Główna tabela dzietności
    result1 <- tryCatch({
      get_eurostat("demo_tfr", time_format = "date") %>%
        filter(geo == input$pop_country) %>%
        select(geo, TIME_PERIOD, fertility = values) %>%
        mutate(year = year(TIME_PERIOD)) %>%
        filter(between(year, input$pop_years[1], input$pop_years[2])) %>%
        arrange(TIME_PERIOD)
    }, error = function(e) NULL)

    if (!is.null(result1) && nrow(result1) > 0) return(result1)

    # Alternatywna tabela
    result2 <- tryCatch({
      get_eurostat("demo_find", time_format = "date") %>%
        filter(geo == input$pop_country) %>%
        select(geo, TIME_PERIOD, fertility = values) %>%
        mutate(year = year(TIME_PERIOD)) %>%
        filter(between(year, input$pop_years[1], input$pop_years[2])) %>%
        arrange(TIME_PERIOD)
    }, error = function(e) NULL)

    if (!is.null(result2) && nrow(result2) > 0) return(result2)

    # Pusta ramka danych
    return(data.frame(
      geo = character(),
      TIME_PERIOD = as.Date(character()),
      fertility = numeric(),
      year = numeric()
    ))
  })

  # Wiek mediana - UPROSZCZONE
  median_age_data <- reactive({
    tryCatch({
      get_eurostat("demo_pjanind", time_format = "date") %>%
        filter(geo == input$pop_country, indic_de == "MEDAGEPOP") %>%
        select(geo, TIME_PERIOD, median_age = values) %>%
        mutate(year = year(TIME_PERIOD)) %>%
        filter(between(year, input$pop_years[1], input$pop_years[2])) %>%
        arrange(TIME_PERIOD)
    }, error = function(e) {
      data.frame(
        geo = character(),
        TIME_PERIOD = as.Date(character()),
        median_age = numeric(),
        year = numeric()
      )
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
      return(valueBox("Brak danych", subtitle = "Przyrost naturalny", icon = icon("chart-line"), color = "yellow"))
    }

    # Znajdź najnowszy dostępny rok z danymi
    latest <- dane %>%
      filter(!is.na(natural_growth)) %>%
      tail(1)

    if (nrow(latest) == 0) {
      return(valueBox("Brak danych", subtitle = "Przyrost naturalny", icon = icon("chart-line"), color = "yellow"))
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

  # Trend urodzeń i zgonów - POPRAWIONE z lepszą obsługą danych
  output$birth_death_trend <- renderPlotly({
    dane <- birth_death_data()
    if (nrow(dane) == 0) {
      return(plot_ly() %>% layout(title = "Brak danych o urodzeniach i zgonach"))
    }

    p <- plot_ly(data = dane, x = ~TIME_PERIOD)

    # Dodaj linie tylko jeśli dane istnieją i nie są wszystkie NA
    if ("births" %in% names(dane) && !all(is.na(dane$births))) {
      p <- p %>% add_trace(y = ~births, type = "scatter", mode = "lines+markers",
                          name = "Urodzenia (‰)", line = list(color = "green"))
    }

    if ("deaths" %in% names(dane) && !all(is.na(dane$deaths))) {
      p <- p %>% add_trace(y = ~deaths, type = "scatter", mode = "lines+markers",
                          name = "Zgony (‰)", line = list(color = "red"))
    }

    if ("natural_growth" %in% names(dane) && !all(is.na(dane$natural_growth))) {
      p <- p %>% add_trace(y = ~natural_growth, type = "scatter", mode = "lines+markers",
                          name = "Przyrost naturalny (‰)", line = list(color = "blue"))
    }

    p %>% layout(
      title = "Urodzenia, zgony i przyrost naturalny",
      xaxis = list(title = "Rok"),
      yaxis = list(title = "Na 1000 mieszkańców"),
      legend = list(
        orientation = "h",  # pozioma orientacja
        x = 0.5,           # wyśrodkowanie poziomo
        xanchor = "center", # punkt zakotwiczenia na środku
        y = -0.2,          # umieszczenie pod osią X
        yanchor = "top"    # zakotwiczenie u góry legendy
      ),
      margin = list(b = 80)  # zwiększ dolny margines dla legendy
    )
  })

  # Trend dzietności
  output$fertility_trend <- renderPlotly({
  dane <- fertility_data()
  if (nrow(dane) == 0) {
    return(plot_ly() %>% layout(title = "Brak danych o dzietności"))
  }

  # tylko wybrany kraj
  dane <- dane %>% filter(geo == input$pop_country)

  plot_ly(
    data = dane,
    x = ~TIME_PERIOD,
    y = ~fertility,
    type = "scatter",
    mode = "lines+markers",
    name = "Dzietność",
    line = list(color = "orange", width = 2),
    marker = list(size = 6, color = "orange")
  ) %>%
    add_trace(
      x = c(min(dane$TIME_PERIOD), max(dane$TIME_PERIOD)),
      y = c(2.1, 2.1),
      type = "scatter",
      mode = "lines",
      name = "Poziom zastępowalności (2.1)",
      line = list(dash = "dash", color = "red", width = 2)
    ) %>%
    layout(
      title = paste("Współczynnik dzietności -", input$pop_country),
      xaxis = list(title = "Rok"),
      yaxis = list(title = "Dzieci na kobietę", rangemode = "tozero"),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)
    )
})

  # Porównanie krajów - POPRAWIONE z lepszą obsługą danych
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
      dane <- tryCatch({
        # Spróbuj demo_r_gind3 najpierw
        result <- get_eurostat("demo_r_gind3", time_format = "date") %>%
          filter(indic_de == "NATGRO") %>%
          mutate(year = year(TIME_PERIOD)) %>%
          filter(year == rok) %>%
          mutate(value = values, unit = "‰") %>%
          select(geo, value, unit) %>%
          na.omit() %>%
          arrange(desc(value))

        if (nrow(result) == 0) {
          # Jeśli brak danych, spróbuj obliczyć z różnicy
          births <- get_eurostat("demo_r_gind3", time_format = "date") %>%
            filter(indic_de == "GBIRTHRT") %>%
            mutate(year = year(TIME_PERIOD)) %>%
            filter(year == rok) %>%
            select(geo, births = values)

          deaths <- get_eurostat("demo_r_gind3", time_format = "date") %>%
            filter(indic_de == "GDEATHRT") %>%
            mutate(year = year(TIME_PERIOD)) %>%
            filter(year == rok) %>%
            select(geo, deaths = values)

          result <- inner_join(births, deaths, by = "geo") %>%
            mutate(value = births - deaths, unit = "‰") %>%
            select(geo, value, unit) %>%
            na.omit() %>%
            arrange(desc(value))
        }

        return(result)
      }, error = function(e) {
        data.frame(geo = character(), value = numeric(), unit = character())
      })
      title_text <- paste("Przyrost naturalny w krajach UE -", rok)
      y_title <- "Przyrost naturalny (‰)"

    } else if (indicator == "fertility") {
      dane <- tryCatch({
        get_eurostat("demo_tfr", time_format = "date") %>%
          mutate(year = year(TIME_PERIOD)) %>%
          filter(year == rok) %>%
          mutate(value = values, unit = "dzieci/kobieta") %>%
          select(geo, value, unit) %>%
          na.omit() %>%
          arrange(desc(value))
      }, error = function(e) {
        data.frame(geo = character(), value = numeric(), unit = character())
      })
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
