# emisje.R ----------------------------------------------------------------
# Zakładka Shiny: EMISJE CO₂

library(dplyr)
library(lubridate)
library(eurostat)
library(plotly)
library(shiny)
library(shinydashboard)

# ────────────────────────────────────────────────────────────────────────
# 1. Pobranie i normalizacja env_air_gge (cache w .GlobalEnv)
# ────────────────────────────────────────────────────────────────────────
if (!exists("co2_all", envir = .GlobalEnv) ||
    !"rok" %in% names(get("co2_all", envir = .GlobalEnv))) {

  message("Pobieram env_air_gge …")
  co2_all <- get_eurostat("env_air_gge", time_format = "date") |>
    filter(airpol == "CO2", unit == "THS_T") |>
    mutate(
      rok = year(TIME_PERIOD),
      sektor = case_when(
        grepl("^CRF1", src_crf) ~ "Energia",
        grepl("^CRF2", src_crf) ~ "Przemysł",
        grepl("^CRF3", src_crf) ~ "Rolnictwo",
        grepl("^CRF4", src_crf) ~ "Zm. Gruntowe",
        grepl("^CRF5", src_crf) ~ "Odpady",
        TRUE                    ~ "Inne")
    )
  assign("co2_all", co2_all, envir = .GlobalEnv)
}

co2_all <- get("co2_all", envir = .GlobalEnv)

# ────────────────────────────────────────────────────────────────────────
# 2. UI
# ────────────────────────────────────────────────────────────────────────
tab_emisje_ui <- tabItem(
  tabName = "emisje",
  h2("Emisje CO₂ w krajach UE"),
  fluidRow(
    box(width = 6,
        selectInput("co2_country", "Kraj:",
                    choices = eurostat::eu_countries$code, selected = "PL")),
    box(width = 6,
        sliderInput("co2_years", "Zakres lat:",
                    min = min(co2_all$rok), max = max(co2_all$rok),
                    value = c(2005, max(co2_all$rok)),
                    step = 1, sep = ""))
  ),
  fluidRow(
    valueBoxOutput("co2_vb_latest", width = 4),
    valueBoxOutput("co2_vb_change", width = 4),
    valueBoxOutput("co2_vb_target", width = 4)
  ),
  fluidRow(box(width = 12, plotlyOutput("co2_trend", height = "400px"))),
  fluidRow(
    box(width = 9, plotlyOutput("co2_pie_sector",   height = "400px")),
  ),
  br(),
  fluidRow(
    box(width = 12, plotlyOutput("co2_bar_countries", height = "500px")
    )
)

# ────────────────────────────────────────────────────────────────────────
# 3. SERVER
# ────────────────────────────────────────────────────────────────────────
tab_emisje_server <- function(input, output, session) {

  # ---------- całkowite emisje (sumujemy po sektorach) ------------------
  co2_total <- reactive({
    co2_all |>
      group_by(geo, rok) |>
      summarise(total = sum(values, na.rm = TRUE), .groups = "drop")
  })

  # ---------- trend dla kraju + zakresu ---------------------------------
  co2_trend_df <- reactive({
    co2_total() |>
      filter(geo == input$co2_country,
             between(rok, input$co2_years[1], input$co2_years[2])) |>
      arrange(rok)
  })

  # ---------- sektorowa struktura w ostatnim roku -----------------------
  co2_sector_df <- reactive({
    co2_all |>
      filter(geo == input$co2_country, rok == input$co2_years[2]) |>
      group_by(sektor) |>
      summarise(emisje = sum(values, na.rm = TRUE), .groups = "drop") |>
      filter(emisje > 0)
  })

  # ---------- valueBox: ostatni rok -------------------------------------
  output$co2_vb_latest <- renderValueBox({
    latest <- tail(co2_trend_df(), 1)
    if (nrow(latest) == 0)
      return(valueBox("Brak danych", "Emisje łączne", icon("ban")))
    valueBox(formatC(latest$total, big.mark = " ", format="f", digits=0),
             paste("kt CO₂ –", latest$rok),
             icon = icon("cloud"), color = "orange")
  })

  # ---------- valueBox: zmiana w zakresie -------------------------------
  output$co2_vb_change <- renderValueBox({
    d <- co2_trend_df()
    if (nrow(d) < 2) return(valueBox("Brak danych", "Zmiana emisji", icon("ban")))
    delta <- tail(d,1)$total - head(d,1)$total
    valueBox(paste0(formatC(delta, format="f", digits=0, big.mark=" "), " kt"),
             paste("Zmiana", input$co2_years[1], "–", input$co2_years[2]),
             icon = icon("chart-line"),
             color = ifelse(delta < 0, "green", "red"))
  })

  # ---------- valueBox: Fit-for-55 vs 1990 ------------------------------
  output$co2_vb_target <- renderValueBox({
    base90 <- co2_total() |> filter(geo == input$co2_country, rok == 1990) |> pull(total)
    latest <- tail(co2_trend_df(), 1)$total
    if (length(base90)==0 || is.na(latest))
      return(valueBox("Brak danych", "Cel Fit-for-55", icon("ban")))
    pct <- (latest / base90 - 1) * 100
    valueBox(sprintf("%+.1f %% vs 1990", pct),
             "Fit-for-55 (-55 %)",
             icon = icon("flag-checkered"),
             color = ifelse(pct <= -55, "green", "orange"))
  })

  # ---------- trend wykres ----------------------------------------------
  output$co2_trend <- renderPlotly({
    d <- co2_trend_df()
    if (nrow(d)==0) return(plot_ly() |> layout(title="Brak danych"))
    plot_ly(d, x=~rok, y=~total,
            type="scatter", mode="lines+markers",
            line=list(color="firebrick")) |>
      layout(title=paste("Emisje CO₂ –", input$co2_country),
             yaxis=list(title="kt CO₂"))
  })

  # ---------- pie sektor -------------------------------------------------
  output$co2_pie_sector <- renderPlotly({
    d <- co2_sector_df()
    if (nrow(d)==0) return(plot_ly() |> layout(title="Brak danych sektorowych"))
    plot_ly(d, labels=~sektor, values=~emisje,
            type="pie", textinfo="label+percent",
            insidetextorientation="radial") |>
      layout(title=paste("Struktura emisji CO₂ –",
                         input$co2_country, input$co2_years[2]),
             legend=list(orientation="h", x=0.1, y=-0.1))
  })

  # ---------- bar kraje --------------------------------------------------
  output$co2_bar_countries <- renderPlotly({
    d <- co2_total() |>
      filter(rok == input$co2_years[2]) |>
      left_join(eurostat::eu_countries, by = c("geo" = "code")) |>
      arrange(total)

    plot_ly(
      d,
      x = ~total,
      y = ~reorder(name, total),
      type = "bar",
      orientation = "h",
      source = "bar_co2",                          # ← NEW
      marker = list(color="rgba(178,34,34,0.7)",
                    line  = list(color="rgba(178,34,34,1)", width = 1))
    ) |>
      layout(title = paste("Emisje CO₂ –", input$co2_years[2]),
            xaxis = list(title = "kt CO₂"),
            yaxis = list(title = ""))
  })

  # klik w słupek  →  zmiana pola „Kraj”
  observeEvent(event_data("plotly_click", source = "bar_co2"), {
    ed <- event_data("plotly_click", source = "bar_co2")
    if (!is.null(ed)) {
      kraj <- ed$y                                      # oś Y (poziome bary)
      code <- eurostat::eu_countries$code[
                match(kraj, eurostat::eu_countries$name)]
      if (!is.na(code)) {
        updateSelectInput(session, "co2_country", selected = code)
      }
    }
  })
}
