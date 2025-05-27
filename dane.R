# dane.R — zakładka z opisem danych i źródeł

library(shiny)
library(shinydashboard)

tab_dane_ui <- tabItem(
  tabName = "dane",

    fluidRow(
    column(width = 12,
        div(
          img(src = "https://ec.europa.eu/eurostat/cache/infographs/statexpl-info/limited-employment/img/logoEstat.png", 
              alt = "Kot", 
              style = "max-width: 100%; height: auto; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
              onerror = "this.style.display='none'; this.nextElementSibling.style.display='block';"),
          
          
        )
    )
  ),

  fluidRow(
    box(width = 12, status = "primary", solidHeader = TRUE, title = "Opis projektu",
        p("Aplikacja prezentuje przekrojowe dane statystyczne krajów Unii Europejskiej (i wybranych innych państw europejskich) pozyskane z portalu "),
        a("Eurostat", href = "https://ec.europa.eu/eurostat", target = "_blank"), 
        p("Źródła danych są pobierane dynamicznie za pomocą pakietu ", code("eurostat"), " i przetwarzane w czasie rzeczywistym.")
    )
  ),

  fluidRow(
    box(width = 12, title = "Zakres danych i wykorzystane zbiory", status = "info", solidHeader = TRUE,
        tags$ul(
          tags$li("📍 ", strong("Demografia:"), " populacja, współczynnik dzietności, przyrost naturalny, wiek mediana (zbiory: ", code("demo_pjan, demo_tfr, demo_r_gind3"), ")"),
          tags$li("📉 ", strong("Finanse (PKB):"), " dane z ", code("nama_10_pc"), " z możliwością wyboru zmiennej i jednostki."),
          tags$li("👷 ", strong("Zatrudnienie i bezrobocie:"), " dane z ", code("lfsi_emp_a, une_rt_a, earn_ses_monthly, yth_data"), ", dane miesięczne lub roczne z podziałem na wiek/płeć."),
          tags$li("🌱 ", strong("Energia odnawialna:"), " udział OZE ogółem, wg sektora i źródeł (dane lokalne z plików RDS przygotowanych wcześniej: ", code("oze_data.rds, oze_struct_all.rds"), ")."),
          tags$li("🌍 ", strong("Emisje CO₂:"), " dane z ", code("env_air_gge"), ", emisje całkowite, sektorowe, cele klimatyczne (Fit-for-55)."),
          tags$li("🗺️ ", strong("Wizualizacja mapowa:"), " mapa krajów z możliwością kliknięcia, dane ogólne o kraju (PKB, populacja, zatrudnienie, powierzchnia).")
        )
    )
  ),

  fluidRow(
    box(width = 12, title = "Pakiety R użyte w aplikacji", status = "success", solidHeader = TRUE,
        tags$ul(
          tags$li(code("shiny"), " – silnik aplikacji"),
          tags$li(code("shinydashboard"), " – układ paneli i dashboardu"),
          tags$li(code("eurostat"), " – pobieranie danych z portalu Eurostat"),
          tags$li(code("dplyr, tidyr, lubridate"), " – transformacje danych"),
          tags$li(code("leaflet"), " – interaktywna mapa Europy"),
          tags$li(code("plotly"), " – wykresy interaktywne"),
          tags$li(code("DT"), " – tabele danych z filtrowaniem i sortowaniem")
        )
    )
  ),

  fluidRow(
    box(width = 12, title = "Uwagi techniczne", status = "warning", solidHeader = TRUE,
        tags$ul(
          tags$li("Dane są pobierane na żywo przy uruchomieniu aplikacji lub ładowane lokalnie (np. dane o OZE, zatrudnienie i wynagrodzenia)."),
          tags$li("Niektóre wskaźniki, jak PKB, dostępne są w wielu jednostkach (PPS, euro, ceny stałe)."),
          tags$li("Dla wielu wykresów zastosowano automatyczne filtrowanie zakresu lat i krajów."),
          tags$li("Aplikacja zawiera fallbacki obsługujące brakujące dane (np. alternatywne źródła lub komunikaty 'Brak danych').")
        )
    )
  ),

    # Dodanie zdjęcia kota na górze
  fluidRow(
    box(width = 12, status = "primary", solidHeader = TRUE, title = "🐱 Kot maskotka projektu",
        div(
          style = "text-align: center; padding: 20px;",
          # Opcja 1: Jeśli masz plik lokalnie, umieść go w folderze www/
          # img(src = "kot.jpg", alt = "Kot", style = "max-width: 100%; height: auto; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);"),
          
          # Opcja 2: Inny serwis z kotami (może działać lepiej)
          img(src = "https://cataas.com/cat/cute?width=400&height=300", 
              alt = "Kot", 
              style = "max-width: 100%; height: auto; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
              onerror = "this.style.display='none'; this.nextElementSibling.style.display='block';"),
          
          # Opcja 3: Fallback - emoji kota jeśli zdjęcie się nie załaduje
          div(style = "display: none; font-size: 100px; margin: 20px;", "🐱"),
          
          br(),
          em("Kot nadzorujący dane")
        )
    )
  )
)

tab_dane_server <- function(input, output, session) {
  # obecnie nie wymaga dynamicznego przetwarzania
}