# pkb.R
library(tidyr)

tab_pkb_ui <- tabItem(tabName = "pkb",
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
)

tab_pkb_server <- function(input, output, session, dane_pkb) {

  observe({
    req(dane_pkb())
    updateSelectInput(session, "na_item_pkb",
                      choices = unique(dane_pkb()$na_item),
                      selected = "B1GQ")
    
    updateSelectInput(session, "unit_pkb",
                      choices = unique(dane_pkb()$unit),
                      selected = "CP_EUR_HAB")
  })

  output$pkbTabela <- DT::renderDataTable({
    req(dane_pkb())
    dane_pkb() %>%
      filter(geo == input$country_pkb,
             na_item == input$na_item_pkb,
             unit == input$unit_pkb)
  })

  output$pkbWykres <- renderPlot({
    dane <- dane_pkb() %>%
      filter(geo == input$country_pkb,
             na_item == input$na_item_pkb,
             unit == input$unit_pkb) %>%
      drop_na(values) %>%
      mutate(rok = format(TIME_PERIOD, "%Y")) %>%
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
}
