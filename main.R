library(shiny)
library(shinydashboard)
library(DT)
library(eurostat)
library(dplyr)
library(leaflet)
library(sf)


ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Eurostat"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Wizualizacja", tabName = "wizualizacja", icon = icon("chart-line")),
      menuItem("PKB", tabName = "pkb", icon = icon("money-bill")),
      menuItem("Zatrudnienie", tabName = "zatrudnienie", icon = icon("user-tie")),
      menuItem("Odnawialna energia", tabName = "oze", icon = icon("leaf")),
      menuItem("Emisje CO₂", tabName = "emisje", icon = icon("cloud")),
      menuItem("Populacja", tabName = "populacja", icon = icon("users")),
      menuItem("Dane źródłowe", tabName = "dane", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "wizualizacja",
              h2("Wizualizacja"),
              leafletOutput("mapaKraje", height = "300px"),
              br(),
              textOutput("wybranyKraj"),
      ),
      tabItem(tabName = "pkb",
              h2("PKB"),
              fluidRow(
                column(4,
                       selectInput("country_pkb", "Wybierz kraj:",
                                   choices = c("PL", "DE", "FR", "IT", "ES"), selected = "PL")
                ),
                column(4,
                       selectInput("na_item_pkb", "Wybierz zmienną (na_item):",
                                   choices = NULL, selected = "B1GQ")
                ),
                column(4,
                       selectInput("unit_pkb", "Wybierz jednostkę (unit):",
                                   choices = NULL, selected = "CP_EUR_HAB")
                )
              ),
              DT::dataTableOutput("pkbTabela"),
              br(),
              plotOutput("pkbWykres")
      ),
      tabItem(tabName = "zatrudnienie",
              h2("Zakładka: Zatrudnienie"),
              selectInput("country_zatr", "Wybierz kraj:",
                          choices = c("PL", "DE", "FR", "IT", "ES"), selected = "PL")
      ),
      tabItem(tabName = "oze",
              h2("Zakładka: Odnawialna energia"),
              selectInput("country_oze", "Wybierz kraj:",
                          choices = c("PL", "DE", "FR", "IT", "ES"), selected = "PL")
      ),
      tabItem(tabName = "emisje",
              h2("Zakładka: Emisje CO₂"),
              selectInput("country_emisje", "Wybierz kraj:",
                          choices = c("PL", "DE", "FR", "IT", "ES"), selected = "PL")
      ),
      tabItem(tabName = "populacja",
              h2("Zakładka: Populacja"),
              selectInput("country_pop", "Wybierz kraj:",
                          choices = c("PL", "DE", "FR", "IT", "ES"), selected = "PL")
      ),
      tabItem(tabName = "dane",
              h2("Zakładka: Dane źródłowe"),
              p("Tutaj będą widoczne dane wykorzystane do budowy dashboardu."),
              p("Możesz tu umieścić np. tabele z pakietu DT, pliki CSV lub linki do źródeł.")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # === DANE PKB (pobrane raz) ===
  dane_pkb <- reactive({
    get_eurostat("nama_10_pc", time_format = "date")
  })
  
  # === TABELA PKB (filtrowana po kraju i na_item) ===
  output$pkbTabela <- DT::renderDataTable({
    req(dane_pkb())
    dane_pkb() %>%
      filter(geo == input$country_pkb,
             na_item == input$na_item_pkb,
             unit == input$unit_pkb)
  })
  
  
  # === WYKRES PKB w czasie ===
  output$pkbWykres <- renderPlot({
    dane <- dane_pkb() %>%
      filter(geo == input$country_pkb,
             na_item == input$na_item_pkb,
             unit == input$unit_pkb) %>%
      drop_na(values) %>%
      mutate(rok = format(TIME_PERIOD, "%Y"))%>%
      group_by(rok) %>%
      summarise(srednia = mean(values, na.rm = TRUE)) %>%
      mutate(rok = as.numeric(rok)) %>%
      arrange(rok)
    
    if (nrow(dane) == 0) {
      plot.new()
      title("Brak danych do wyświetlenia")
    } else {
      plot(dane$rok, dane$srednia,
           type = "l", col = "darkgreen", lwd = 2,
           main = paste("Zmienna:", input$na_item_pkb,
                        "| Jednostka:", input$unit_pkb,
                        "| Kraj:", input$country_pkb),
           xlab = "Rok", ylab = "Wartość")
    }
  })
  
  
  # === UZUPEŁNIENIE LISTY OPCJI na_item PO POBRANIU DANYCH ===
  observe({
    req(dane_pkb())
    updateSelectInput(session, "na_item_pkb",
                      choices = unique(dane_pkb()$na_item),
                      selected = "B1GQ")
    
    updateSelectInput(session, "unit_pkb",
                      choices = unique(dane_pkb()$unit),
                      selected = "CP_EUR_HAB")
  })
  
  
  # === MAPA: DANE GRANIC KRAJÓW UE ===
  granice_krajow <- reactive({
    get_eurostat_geospatial(nuts_level = 0, resolution = "60", output_class = "sf")
  })
  
  kraje_z_danych <- reactive({
    unique(dane_pkb()$geo)
  })
  
  output$mapaKraje <- renderLeaflet({
    mapa <- granice_krajow() %>%
      filter(geo %in% kraje_z_danych())
    
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
  
  # === OBSŁUGA KLIKAJĄCEGO KRAJU NA MAPIE ===
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



shinyApp(ui, server)