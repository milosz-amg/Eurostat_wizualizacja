# wizualizacja.R

tab_wizualizacja_ui <- tabItem(tabName = "wizualizacja",
  h2("Wizualizacja"),
  leafletOutput("mapaKraje", height = "300px"),
  br(),
  textOutput("wybranyKraj")
)

tab_wizualizacja_server <- function(input, output, session, dane_pkb) {
  granice_krajow <- reactive({
    get_eurostat_geospatial(nuts_level = 0, resolution = "60", output_class = "sf")
  })

  kraje_z_danych <- reactive({
    unique(dane_pkb()$geo)
  })

  output$mapaKraje <- renderLeaflet({
    req(kraje_z_danych())
    mapa <- granice_krajow() %>% filter(geo %in% kraje_z_danych())

    leaflet(mapa) %>%
      addTiles() %>%
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

  wybrany_kraj <- reactiveVal(NULL)

  observeEvent(input$mapaKraje_shape_click, {
    klik <- input$mapaKraje_shape_click
    if (!is.null(klik$id)) {
      wybrany_kraj(klik$id)
    }
  })

  output$wybranyKraj <- renderText({
    if (is.null(wybrany_kraj())) {
      "Kliknij na kraj na mapie, aby zobaczyć jego kod."
    } else {
      paste("Wybrano kraj:", wybrany_kraj())
    }
  })
}
