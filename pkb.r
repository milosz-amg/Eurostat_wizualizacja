# pkb.R
library(tidyr)


# pkb.R
tab_pkb_ui <- tabItem(tabName = "pkb",
  h2("Finanse"),
  fluidRow(
    column(4,
          selectInput("porownaj_kraje", "Wybierz kraje:",
            choices = c("PL", "DE", "FR", "IT", "ES"),
            selected = "PL", multiple = TRUE)
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
  fluidRow(
  column(6,
         sliderInput("pkb_lata", "Zakres lat:",
                     min = 1995, max = 2023, value = c(2010, 2023), sep = "")
  ),
),
  br(),
 fluidRow(
  column(12,
         plotOutput("pkbWykres"),
         div(style = "text-align: right; margin-top: 10px;",
             downloadButton("pobierzWykres", "Pobierz wykres")
         )
  )
),
  br(),
h4("Opis:"),
verbatimTextOutput("pkbOpis"),
  br(),
  DT::dataTableOutput("pkbTabela"),


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
    
    # Automatyczne ustawienie zakresu lat na podstawie danych
    lata <- dane_pkb() %>%
      pull(TIME_PERIOD) %>%
      format("%Y") %>%
      as.numeric()
    
    updateSliderInput(session, "pkb_lata",
                      min = min(lata, na.rm = TRUE),
                      max = max(lata, na.rm = TRUE),
                      value = c(max(min(lata), 2010), max(lata)))
  })

  output$pkbTabela <- DT::renderDataTable({
    req(dane_pkb())
    dane_pkb() %>%
      filter(geo == input$porownaj_kraje,
             na_item == input$na_item_pkb,
             unit == input$unit_pkb) %>%
      mutate(rok = as.numeric(format(TIME_PERIOD, "%Y"))) %>%
      filter(rok >= input$pkb_lata[1], rok <= input$pkb_lata[2])
  })

output$pkbWykres <- renderPlot({
  dane <- dane_pkb() %>%
    filter(geo %in% input$porownaj_kraje,
           na_item == input$na_item_pkb,
           unit == input$unit_pkb) %>%
    drop_na(values) %>%
    mutate(rok = format(TIME_PERIOD, "%Y")) %>%
    group_by(geo, rok) %>%
    summarise(srednia = mean(values, na.rm = TRUE), .groups = "drop") %>%
    mutate(rok = as.numeric(rok)) %>%
    filter(rok >= input$pkb_lata[1], rok <= input$pkb_lata[2]) %>%
    arrange(geo, rok)

  if (nrow(dane) == 0) {
    plot.new()
    title("Brak danych do wyświetlenia")
  } else {
    plot(range(dane$rok), range(dane$srednia), type = "n",
         xlab = "Rok", ylab = "Wartość",
         main = paste("Kraje:", paste(input$porownaj_kraje, collapse = ", "),
             "| Zmienna:", input$na_item_pkb,
             "| Jednostka:", input$unit_pkb)
    )

    kraje <- unique(dane$geo)
    kolory <- rainbow(length(kraje))

    for (i in seq_along(kraje)) {
      dane_kraj <- dane %>% filter(geo == kraje[i])
      lines(dane_kraj$rok, dane_kraj$srednia, col = kolory[i], lwd = 2)
    }

    legend("topleft", legend = kraje, col = kolory, lwd = 2)
  }
})


  output$pobierzWykres <- downloadHandler(
  filename = function() {
    paste0("wykres_pkb_", input$porownaj_kraje, "_", Sys.Date(), ".png")
  },
  content = function(file) {
    # Otwarcie urządzenia graficznego PNG
    png(filename = file, width = 900, height = 600)

    dane <- dane_pkb() %>%
      filter(geo == input$porownaj_kraje,
             na_item == input$na_item_pkb,
             unit == input$unit_pkb) %>%
      drop_na(values) %>%
      mutate(rok = format(TIME_PERIOD, "%Y")) %>%
      group_by(rok) %>%
      summarise(srednia = mean(values, na.rm = TRUE)) %>%
      mutate(rok = as.numeric(rok)) %>%
      filter(rok >= input$pkb_lata[1], rok <= input$pkb_lata[2]) %>%
      arrange(rok)

    if (nrow(dane) == 0) {
      plot.new()
      title("Brak danych do wyświetlenia")
    } else {
      plot(dane$rok, dane$srednia,
           type = "l", col = "darkgreen", lwd = 2,
           main = paste("Zmienna:", input$na_item_pkb,
                        "| Jednostka:", input$unit_pkb,
                        "| Kraj:", input$porownaj_kraje),
           xlab = "Rok", ylab = "Wartość")
    }

    dev.off()
  }
)
output$pkbOpis <- renderText({
  # Opisy 'na_item'
opisy_na_item <- list(
  B1GQ = "Produkt krajowy brutto (PKB) – wartość końcowa dóbr i usług wytworzonych w kraju.",
  D1 = "Wynagrodzenia brutto – płace i świadczenia wypłacane pracownikom.",
  P3 = "Spożycie indywidualne – całkowite wydatki gospodarstw domowych.",
  P31_S13 = "Spożycie indywidualne sektora instytucji rządowych (S13).",
  P31_S14 = "Spożycie indywidualne gospodarstw domowych (S14).",
  P31_S14_S15 = "Spożycie indywidualne gospodarstw domowych (S14) i instytucji niekomercyjnych działających na ich rzecz (S15).",
  P31_S15 = "Spożycie indywidualne instytucji niekomercyjnych (S15).",
  P32_S13 = "Spożycie zbiorowe instytucji rządowych (S13) – np. obrona, administracja.",
  P3_S13 = "Spożycie ogółem sektora instytucji rządowych (S13).",
  P41 = "Nakłady brutto na środki trwałe – inwestycje w aktywa trwałe (budynki, maszyny itd.).",
  B2A3G = "Nadwyżka operacyjna brutto + dochód mieszkaniowy – zysk brutto sektora instytucji."
)


  # Opisy 'unit'
opisy_unit <- list(
  CP_EUR_HAB = "Euro na mieszkańca w cenach bieżących.",
  CLV10_EUR_HAB = "Euro na mieszkańca w cenach stałych (rok bazowy 2010).",
  CLV15_EUR_HAB = "Euro na mieszkańca w cenach stałych (rok bazowy 2015).",
  CLV20_EUR_HAB = "Euro na mieszkańca w cenach stałych (rok bazowy 2020).",
  CP_NAC_HAB = "Wartości narodowe (NAC) na mieszkańca w cenach bieżących.",
  CP_PPS_EU27_2020_HAB = "Standard siły nabywczej (PPS) na mieszkańca, UE27 = 100 (rok 2020).",
  PC_EU27_2020_HAB_MEUR_CP = "Miliony euro na mieszkańca (UE27, ceny bieżące, rok 2020).",
  PC_EU27_2020_HAB_MPPS_CP = "Miliony PPS na mieszkańca (UE27, ceny bieżące, rok 2020).",
  CLV_PCH_PRE_HAB = "Roczna zmiana procentowa w cenach stałych na mieszkańca (w stosunku do roku poprzedniego).",
  CLV_I10_HAB = "Indeks (rok 2010 = 100), ceny stałe, na mieszkańca.",
  CLV_I15_HAB = "Indeks (rok 2015 = 100), ceny stałe, na mieszkańca.",
  CLV_I20_HAB = "Indeks (rok 2020 = 100), ceny stałe, na mieszkańca."
)


  opis_na_item <- opisy_na_item[[input$na_item_pkb]]
  if (is.null(opis_na_item)) opis_na_item <- paste("Kod zmiennej:", input$na_item_pkb)

  opis_unit <- opisy_unit[[input$unit_pkb]]
  if (is.null(opis_unit)) opis_unit <- paste("Kod jednostki:", input$unit_pkb)

  paste0(
  "Zmienna (na_item): ", input$na_item_pkb, " – ", opis_na_item, "\n",
  "Jednostka (unit): ", input$unit_pkb, " – ", opis_unit
)

})


}
