# zatrudnienie.R ------------------------------------------------------------
# Zakładka Shiny "Zatrudnienie"

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(eurostat)
library(plotly)

tab_zatrudnienie_ui <- tabItem(
  tabName = "zatrudnienie",
  h2("Zatrudnienie w krajach UE"),

  fluidRow(
    column(4,
      sliderInput("rok_bar", "Rok do porównania krajów UE:",
                  min = 2009, max = 2024, value = 2023, step = 1, sep = "")
    )
  ),
  fluidRow(
    box(width = 12, plotlyOutput("bar_countries", height = "400px"))
  ),
  br(),
  fluidRow(
    box(width = 12,
        column(3,
               selectInput("country", "Kraj:",
                           choices = eurostat::eu_countries$code,
                           selected = "PL")),
        column(3,
               selectInput("sex", "Płeć:",
                           choices = c("Ogółem" = "T",
                                       "Mężczyźni" = "M",
                                       "Kobiety" = "F"),
                           selected = "T")),
        column(3,
               selectInput("age", "Grupa wiekowa:",
                           choices = NULL)),
        column(3,
               sliderInput("years", "Zakres lat:",
                           min = 2009, max = 2024,
                           value = c(2009, 2024), step = 1, sep = ""))
    )
  ),
  fluidRow(
    valueBoxOutput("vb_latest", width = 3),
    valueBoxOutput("vb_change", width = 3),
    valueBoxOutput("vb_youth",  width = 3),
    valueBoxOutput("vb_salary", width = 3)
  ),
  fluidRow(
    box(width = 8, plotlyOutput("trend_plot", height = "300px")),
    box(width = 4, DTOutput("table"))
  )
)


tab_zatrudnienie_server <- function(input, output, session) {

  
  earn_data <- readRDS("data/earn_small.rds")
  une_data_all <- readRDS("data/une_data.rds")
  yth_data_all <- readRDS("data/yth_data.rds")
  
  une_data <- reactive({
    une_data_all %>%
      filter(unit == "PC_ACT", geo %in% eurostat::eu_countries$code)
  })

  yth_data <- reactive({
    yth_data_all %>%
      filter(geo %in% eurostat::eu_countries$code)
  })

  observeEvent(une_data(), {
    updateSelectInput(session, "age",
                      choices = unique(une_data()$age),
                      selected = "Y15-74")
  })

  filt <- reactive({
    req(une_data())
    une_data() %>%
      filter(geo == input$country,
             sex == input$sex,
             age == input$age,
             between(year(TIME_PERIOD), input$years[1], input$years[2])) %>%
      arrange(TIME_PERIOD)
  })

  output$vb_latest <- renderValueBox({
    req(filt())
    latest <- tail(filt(), 1)
    if (nrow(latest) == 0 || is.na(latest$values)) {
      return(valueBox("Brak danych", subtitle = "Stopa bezrobocia", icon = icon("ban"), color = "black"))
    }
    valueBox(
      sprintf("%.1f %%", latest$values),
      subtitle = paste("Stopa bezrobocia w", year(latest$TIME_PERIOD)),
      icon = icon("user-tie"),
      color = "aqua"
    )
  })

  output$vb_change <- renderValueBox({
    req(filt(), nrow(filt()) >= 2)
    delta <- tail(filt(), 1)$values - head(filt(), 1)$values
    valueBox(
      sprintf("%+.1f p.p.", delta),
      subtitle = paste("Zmiana", input$years[1], "-", input$years[2]),
      icon = icon("chart-line"),
      color = ifelse(delta < 0, "green", "red")
    )
  })

  output$vb_youth <- renderValueBox({
    req(yth_data())
    y_filt <- yth_data() %>%
      filter(
        geo == input$country,
        between(year(TIME_PERIOD), input$years[1], input$years[2])
      ) %>%
      arrange(desc(TIME_PERIOD))

    latest <- head(y_filt, 1)

    if (nrow(latest) == 0 || is.na(latest$values)) {
      return(valueBox("Brak danych", subtitle = "NEET", icon = icon("ban"), color = "black"))
    }

    valueBox(
      sprintf("%.1f %%", latest$values),
      subtitle = paste("NEET (", year(latest$TIME_PERIOD), ")"),
      icon = icon("user-graduate"),
      color = "purple"
    )
  })

  output$vb_salary <- renderValueBox({
    salary <- earn_data %>%
      filter(
        geo == input$country,
        sex == input$sex,
        between(year(TIME_PERIOD), input$years[1], input$years[2])
      ) %>%
      arrange(desc(TIME_PERIOD))

    latest <- salary %>% filter(!is.na(values)) %>% head(1)

    if (nrow(latest) == 0) {
      return(valueBox("Brak danych", subtitle = "Średnie wynagrodzenie (EUR)", icon = icon("ban"), color = "black"))
    }

    valueBox(
      sprintf("%.0f EUR", latest$values),
      subtitle = paste("Średnie wynagrodzenie (", year(latest$TIME_PERIOD), ")"),
      icon = icon("euro-sign"),
      color = "yellow"
    )
  })


  output$trend_plot <- renderPlotly({
    req(filt())
    dane <- filt()
    if (nrow(dane) == 0) {
      plot_ly() %>% layout(title = "Brak danych do wyświetlenia")
    } else {
      plot_ly(
        x = dane$TIME_PERIOD,
        y = dane$values,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "navy", width = 2),
        marker = list(size = 8)
      ) %>%
        layout(
          title = sprintf("Bezrobocie: %s | %s | %s", input$country, input$sex, input$age),
          xaxis = list(title = "Rok"),
          yaxis = list(title = "%")
        )
    }
  })

  output$table <- renderDT({
    req(filt())
    filt() %>%
      mutate(Rok = year(TIME_PERIOD)) %>%
      select(Rok, Wartosc = values) %>%
      arrange(desc(Rok)) %>%
      datatable(options = list(pageLength = 10, order = list(list(0, "desc"))),
                rownames = FALSE) %>%
      formatRound("Wartosc", 1)
  })

  output$bar_countries <- renderPlotly({
    dane <- une_data()
    req(dane)
    rok <- input$rok_bar
    dane_rok <- dane %>%
      filter(lubridate::year(TIME_PERIOD) == rok, sex == "T", age == "Y15-74") %>%
      select(geo, values) %>%
      na.omit()

    countries_labels <- eurostat::eu_countries %>% dplyr::select(code, name)
    dane_rok <- dane_rok %>% left_join(countries_labels, by = c("geo" = "code"))
    dane_rok <- dane_rok %>% arrange(desc(values))


    plot_ly(
      data = dane_rok,
      x = ~reorder(name, values),
      y = ~values,
      type = "bar",
      text = ~paste0("Kraj: ", name, "<br>Stopa bezrobocia: ", sprintf("%.2f", values), "%"),
      hoverinfo = "text",
      marker = list(color = 'rgba(30,144,255,0.7)', line = list(color = 'rgba(30,144,255,1)', width = 1.5)),
      source = "bar"

    ) %>%
      layout(
        title = paste("Stopa bezrobocia w UE w roku", rok),
        xaxis = list(title = "Kraj", tickangle = -45),
        yaxis = list(title = "%"),
        bargap = 0.2
      )
  })

  observeEvent(event_data("plotly_click", source = "bar"), {
    ed <- event_data("plotly_click", source = "bar")
    if (!is.null(ed)) {
      kraj_nazwa <- ed$x
      countries_labels <- eurostat::eu_countries %>% dplyr::select(code, name)
      code <- countries_labels$code[match(kraj_nazwa, countries_labels$name)]
      if (!is.na(code)) {
        updateSelectInput(session, "country", selected = code)
      }
    }
  })

}
