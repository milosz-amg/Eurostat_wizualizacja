library(eurostat)
library(dplyr)
library(leaflet)
library(shinydashboard)

tab_wizualizacja_ui <- tabItem(tabName = "wizualizacja",
  fluidRow(
    column(8,
      leafletOutput("mapaKraje", height = "600px", width = "100%")
    ),
    column(4,
      uiOutput("krajNazwa"),
      br(),
      uiOutput("krajPKB"),
      br(),
      uiOutput("krajPopulacja"),
      br(),
      uiOutput("krajBezrobocie"),
      br(),
      uiOutput("krajZatrudnienie"),  # NOWY wskaźnik
      br(),
      uiOutput("krajPowierzchnia")
    )
  ),
  br(),
  textOutput("wybranyKraj")
)

tab_wizualizacja_server <- function(input, output, session) {

  wybrany_kraj <- reactiveVal("PL")

  observeEvent(input$mapaKraje_shape_click, {
    klik <- input$mapaKraje_shape_click
    if (!is.null(klik$id)) {
      wybrany_kraj(klik$id)
    }
  })

  granice_krajow <- reactive({
    get_eurostat_geospatial(nuts_level = 0, resolution = "60", output_class = "sf")
  })

  output$mapaKraje <- renderLeaflet({
    leaflet(granice_krajow()) %>%
      addTiles() %>%
      setView(lng = 15, lat = 54, zoom = 4) %>%
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

  dane_pkb <- reactive({ get_eurostat("nama_10_pc", time_format = "date") })
  dane_pop <- reactive({ get_eurostat("demo_pjan", time_format = "date") })
  dane_bezrobocie <- reactive({ 
    readRDS("data/une_data.rds") %>%
      filter(unit == "PC_ACT", geo %in% eurostat::eu_countries$code)
  })
  dane_zatrudnienie <- reactive({ get_eurostat("lfsi_emp_a", time_format = "date") })  # NOWE
  dane_pow <- reactive({
    get_eurostat("reg_area3", time_format = "num") %>%
      filter(nchar(geo) == 2) %>%
      group_by(geo) %>%
      summarise(powierzchnia_km2 = sum(values, na.rm = TRUE), .groups = "drop")
  })

  dane_geo <- reactive({
    tryCatch({
      geom_data <- get_eurostat_geospatial(nuts_level = 0, resolution = "60", output_class = "sf")
      if ("geometry" %in% names(geom_data)) {
        geom_data$area_km2 <- as.numeric(sf::st_area(geom_data)) / 1e6
      }
      geom_data
    }, error = function(e) {
      NULL
    })
  })

  nazwy_krajow_pl <- c(
    AT = "Austria", BE = "Belgia", BG = "Bułgaria", HR = "Chorwacja", CY = "Cypr", CZ = "Czechy",
    DK = "Dania", EE = "Estonia", FI = "Finlandia", FR = "Francja", DE = "Niemcy", GR = "Grecja",
    HU = "Węgry", IE = "Irlandia", IT = "Włochy", LV = "Łotwa", LT = "Litwa", LU = "Luksemburg",
    MT = "Malta", NL = "Holandia", PL = "Polska", PT = "Portugalia", RO = "Rumunia",
    SK = "Słowacja", SI = "Słowenia", ES = "Hiszpania", SE = "Szwecja", NO = "Norwegia",
    CH = "Szwajcaria", IS = "Islandia", UK = "Wielka Brytania", TR = "Turcja", ME = "Czarnogóra",
    RS = "Serbia", MK = "Macedonia Północna", EL = "Grecja", AL = "Albania"
  )

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
      df <- dane_pop() %>%
        filter(geo == wybrany_kraj(), sex == "T", age == "TOTAL") %>%
        arrange(desc(TIME_PERIOD)) %>%
        slice(1)
      if (nrow(df) == 0) {
        df <- dane_pop() %>%
          filter(geo == wybrany_kraj(), sex == "T") %>%
          arrange(desc(TIME_PERIOD)) %>%
          slice(1)
      }
      if (nrow(df) == 0 || is.na(df$values) || df$values == 0) {
        return(valueBox("Brak danych", "Populacja", icon = icon("users"), color = "light-blue", width = 12))
      }
      populacja <- format(df$values, big.mark = " ", scientific = FALSE)
      valueBox(populacja, "Populacja", icon = icon("users"), color = "orange", width = 12)
    }, error = function(e) {
      valueBox("Błąd", "Populacja", icon = icon("users"), color = "light-blue", width = 12)
    })
  })

  output$krajBezrobocie <- renderUI({
  req(dane_bezrobocie(), wybrany_kraj())
  tryCatch({
    df <- dane_bezrobocie() %>%
      filter(geo == wybrany_kraj(), sex == "T", age == "Y15-74") %>%  # Zmiana z "TOTAL" na "Y15-74"
      arrange(desc(TIME_PERIOD)) %>%
      slice(1)
    if (nrow(df) == 0 || is.na(df$values)) {
      return(valueBox("Brak danych", "Bezrobocie", icon = icon("chart-line"), color = "light-blue", width = 12))
    }
    valueBox(paste0(round(df$values, 1), " %"), "Bezrobocie", icon = icon("chart-line"), color = "red", width = 12)
  }, error = function(e) {
    valueBox("Błąd", "Bezrobocie", icon = icon("chart-line"), color = "light-blue", width = 12)
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
      valueBox("Błąd", "Powierzchnia", icon = icon("globe"), color = "light-blue", width = 12)
    })
  })
}
