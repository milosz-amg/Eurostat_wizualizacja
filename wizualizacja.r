# wizualizacja.R

library(eurostat)
library(dplyr)
library(leaflet)
library(shinydashboard)

tab_wizualizacja_ui <- tabItem(tabName = "wizualizacja",
  fluidRow(
    # Kolumna z mapą (szerokość 8/12)
    column(8,
      leafletOutput("mapaKraje", height = "600px", width = "100%")
    ),
    # Kolumna z kafelkami danych (szerokość 4/12)
    column(4,
      uiOutput("krajNazwa"),
      br(),
      uiOutput("krajPKB"),
      br(),
      uiOutput("krajPopulacja"),
      br(),
      uiOutput("krajBezrobocie"),
      br(),
      uiOutput("krajPowierzchnia")
    )
  ),
  br(),
  textOutput("wybranyKraj")
)

tab_wizualizacja_server <- function(input, output, session) {

  # === REACTIVE: Kliknięty kraj ===
  wybrany_kraj <- reactiveVal("PL")

  observeEvent(input$mapaKraje_shape_click, {
    klik <- input$mapaKraje_shape_click
    if (!is.null(klik$id)) {
      wybrany_kraj(klik$id)
    }
  })

  # === POBIERANIE DANYCH ===
  granice_krajow <- reactive({
    get_eurostat_geospatial(nuts_level = 0, resolution = "60", output_class = "sf")
  })

  output$mapaKraje <- renderLeaflet({
    leaflet(granice_krajow()) %>%
      addTiles() %>%
      setView(lng = 15, lat = 54, zoom = 4) %>%  # Przybliżenie na Europę
      addPolygons(
        layerId = ~geo,
        label = ~as.character(geo),
        color = "darkblue",
        weight = 1,
        fillOpacity = 0.5,
        highlight = highlightOptions(
          weight = 2,
          color = "black",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      )
  })

  dane_pkb <- reactive({
    get_eurostat("nama_10_pc", time_format = "date")
  })

  dane_pop <- reactive({
    get_eurostat("demo_pjan", time_format = "date")
  })

  dane_bezrobocie <- reactive({
    get_eurostat("une_rt_a", time_format = "date")
  })

  dane_pow <- reactive({
  get_eurostat("reg_area3", time_format = "num") %>%
    filter(nchar(geo) == 2) %>%     # tylko kraje (NUTS 0)
    group_by(geo) %>%
    summarise(powierzchnia_km2 = sum(values, na.rm = TRUE), .groups = "drop")
})

  
  # Alternatywne źródło danych o powierzchni - dane geograficzne
  dane_geo <- reactive({
    tryCatch({
      # Pobieramy granice krajów które zawierają informacje o powierzchni
      geom_data <- get_eurostat_geospatial(nuts_level = 0, resolution = "60", output_class = "sf")
      # Obliczamy powierzchnię z geometrii
      if("geometry" %in% names(geom_data)) {
        geom_data$area_km2 <- as.numeric(sf::st_area(geom_data)) / 1000000  # konwersja z m² na km²
      }
      geom_data
    }, error = function(e) {
      NULL
    })
  })

  nazwy_krajow_pl <- c(
  AT = "Austria",
  BE = "Belgia",
  BG = "Bułgaria",
  HR = "Chorwacja",
  CY = "Cypr",
  CZ = "Czechy",
  DK = "Dania",
  EE = "Estonia",
  FI = "Finlandia",
  FR = "Francja",
  DE = "Niemcy",
  GR = "Grecja",
  HU = "Węgry",
  IE = "Irlandia",
  IT = "Włochy",
  LV = "Łotwa",
  LT = "Litwa",
  LU = "Luksemburg",
  MT = "Malta",
  NL = "Holandia",
  PL = "Polska",
  PT = "Portugalia",
  RO = "Rumunia",
  SK = "Słowacja",
  SI = "Słowenia",
  ES = "Hiszpania",
  SE = "Szwecja",
  NO = "Norwegia",
  CH = "Szwajcaria",
  IS = "Islandia",
  UK = "Wielka Brytania",
  TR = "Turcja",
  ME = "Czarnogóra",
  RS = "Serbia",
  MK = "Macedonia Północna",
  EL = "Grecja",
  AL = "Albania"
)


  # === Wartości ==
  output$krajNazwa <- renderUI({
  req(wybrany_kraj())
  kod <- wybrany_kraj()

  nazwa <- nazwy_krajow_pl[[kod]]
  if (is.null(nazwa)) nazwa <- kod

  valueBox(nazwa, "Kraj", icon = icon("flag"), color = "blue", width = 12)
})



  output$krajPKB <- renderUI({
  req(dane_pkb(), wybrany_kraj())

  tryCatch({
    df <- dane_pkb() %>%
      filter(geo == wybrany_kraj(), na_item == "B1GQ", unit == "CP_EUR_HAB") %>%
      arrange(desc(TIME_PERIOD)) %>%
      slice(1)

    if (nrow(df) == 0 || is.na(df$values)) {
      return(valueBox("Brak danych", "PKB per capita", icon = icon("money-bill"), color = "light-blue", width = 12))
    }

    pkb <- round(df$values)
    valueBox(paste0(pkb, " €"), "PKB per capita (ostatni rok)", icon = icon("money-bill"), color = "green", width = 12)
  }, error = function(e) {
    valueBox("Błąd", "PKB", icon = icon("money-bill"), color = "light-blue", width = 12)
  })
})

  output$krajPopulacja <- renderUI({
    req(dane_pop(), wybrany_kraj())
    
    tryCatch({
      # Próbujemy różne kombinacje filtrów dla populacji
      df <- dane_pop() %>%
        filter(geo == wybrany_kraj()) %>%
        filter(age == "TOTAL") %>%
        filter(sex == "T") %>%
        arrange(desc(TIME_PERIOD)) %>%
        slice(1)

      # Jeśli nie ma danych, próbujemy bez filtra wieku
      if (nrow(df) == 0) {
        df <- dane_pop() %>%
          filter(geo == wybrany_kraj()) %>%
          filter(sex == "T") %>%
          arrange(desc(TIME_PERIOD)) %>%
          slice(1)
      }
      
      # Ostatnia próba - bez żadnych dodatkowych filtrów
      if (nrow(df) == 0) {
        df <- dane_pop() %>%
          filter(geo == wybrany_kraj()) %>%
          arrange(desc(TIME_PERIOD)) %>%
          slice(1)
      }

      if (nrow(df) == 0 || is.na(df$values) || df$values == 0) {
        return(valueBox("Brak danych", "Populacja", icon = icon("users"), color = "light-blue", width = 12))
      }
      
      populacja <- format(df$values, big.mark = " ", scientific = FALSE)
      valueBox(populacja, "Populacja", icon = icon("users"), color = "orange", width = 12)

    }, error = function(e) {
      valueBox(paste("Błąd:", e$message), "Populacja", icon = icon("users"), color = "light-blue", width = 12)
    })
  })

  output$krajBezrobocie <- renderUI({
    req(dane_bezrobocie(), wybrany_kraj())
    
    tryCatch({
      # Najpierw próbujemy z rocznych danych bezrobocia
      df <- dane_bezrobocie() %>%
        filter(geo == wybrany_kraj()) %>%
        filter(sex == "T") %>%
        filter(age == "TOTAL") %>%
        arrange(desc(TIME_PERIOD)) %>%
        slice(1)

      # Jeśli nie ma danych, próbujemy bez filtra wieku
      if (nrow(df) == 0) {
        df <- dane_bezrobocie() %>%
          filter(geo == wybrany_kraj()) %>%
          filter(sex == "T") %>%
          arrange(desc(TIME_PERIOD)) %>%
          slice(1)
      }
      
      # Ostatnia próba - bez dodatkowych filtrów
      if (nrow(df) == 0) {
        df <- dane_bezrobocie() %>%
          filter(geo == wybrany_kraj()) %>%
          arrange(desc(TIME_PERIOD)) %>%
          slice(1)
      }

      if (nrow(df) == 0 || is.na(df$values)) {
        return(valueBox("Brak danych", "Bezrobocie", icon = icon("chart-line"), color = "light-blue", width = 12))
      }
      
      valueBox(paste0(round(df$values, 1), " %"), "Bezrobocie", icon = icon("chart-line"), color = "red", width = 12)
    }, error = function(e) {
      valueBox(paste("Błąd:", e$message), "Bezrobocie", icon = icon("chart-line"), color = "light-blue", width = 12)
    })
  })

  output$krajPowierzchnia <- renderUI({
  req(dane_pow(), wybrany_kraj())
  
  tryCatch({
    df <- dane_pow() %>% filter(geo == wybrany_kraj())

    if (nrow(df) == 0 || is.na(df$powierzchnia_km2)) {
      return(valueBox("Brak danych", "Powierzchnia", icon = icon("globe"), color = "light-blue", width = 12))
    }

    powierzchnia <- format(round(df$powierzchnia_km2), big.mark = " ", scientific = FALSE)
    valueBox(paste0(powierzchnia, " km²"), "Powierzchnia", icon = icon("globe"), color = "purple", width = 12)
  }, error = function(e) {
    valueBox(paste("Błąd:", e$message), "Powierzchnia", icon = icon("globe"), color = "light-blue", width = 12)
  })
})

}