# app.R
library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(thematic)
library(readr)
library(lubridate) 

# Data Parsing Function ---------------------------------------------------
clean_monitor_data <- function(path, location_name) {
  df <- read.csv(path)
  df <- df |>
    mutate(
      parsed_pt_date = with_tz(ymd_hms(time_stamp), "America/Los_Angeles"),
      hour_of_day = hour(parsed_pt_date),
      day  = day(parsed_pt_date),
      dow  = wday(parsed_pt_date),
      location = location_name
    )
  df
}

# Read in Data --------------------------------------------------------------------
oakland_mon <- clean_monitor_data("apps/weekly_data/PurpleAir_Week_Oct19_Oct26_2025/EastOaklandRoseOct19to26.csv", "East Oakland Sensor")
pittsburg <- clean_monitor_data("apps/weekly_data/PurpleAir_Week_Oct19_Oct26_2025/PittsburgRoseOct19to26.csv", "Pittsburg Sensor")
pittsburg_high <- clean_monitor_data("apps/weekly_data/PurpleAir_Week_Oct19_Oct26_2025/PittsburgHighRoseOct19to26.csv", "Pittsburg High Sensor")
richmond <- clean_monitor_data("apps/weekly_data/PurpleAir_Week_Oct19_Oct26_2025/PointRichmondRoseOct19to26.csv", "Point Richmond Sensor")

df_all <- bind_rows(oakland_mon, pittsburg, pittsburg_high, richmond)

# Label helpers 

labels_12h <- c(
  "12 AM","1 AM","2 AM","3 AM","4 AM","5 AM",
  "6 AM","7 AM","8 AM","9 AM","10 AM","11 AM",
  "12 PM","1 PM","2 PM","3 PM","4 PM","5 PM",
  "6 PM","7 PM","8 PM","9 PM","10 PM","11 PM"
)

labels_dow <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")

# -----------------------------
# Theme (works on older bslib)
# -----------------------------
theme_youth <- bs_theme(
  version = 5,
  bootswatch = "minty",
  base_font  = font_google("Rubik",            wght = 400),
  heading_font = font_google("Barlow Condensed", wght = 700),
  primary = "#D5E7F0",
  secondary = "#9DBF3B",
  success = "#fa846d"
)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  theme = theme_youth,
  tags$head(
    tags$title("New Voices Air Monitoring Network - Weekly Review"),
    tags$style(HTML("
      :root{
      --bs-heading-color:#002942;
      }
      .card {
        background: #ffffff;
        border-radius: 16px;
        padding: 16px 18px;
        box-shadow: 0 6px 16px rgba(0,0,0,0.06);
        margin-bottom: 16px;
        color: #002942;
      }
      .valuebox {
        display: flex; align-items: center; gap: 12px;
      }
      .valuebox .icon {
        font-size: 28px; line-height: 1;
      }
      .valuebox .text {
        color: #002942; display: flex; flex-direction: column;
      }
      .valuebox .value {
        font-weight: 700; font-size: 26px;
      }
      .valuebox .subtitle {
        color: #002942; font-size: 12px; margin-top: -2px;
      }
      .page-title {
        margin: 12px 0 8px 0;
      }
    ")),
    # Tiny helper to set text content of spans by id
  ),
  
  titlePanel(div(class = "page-title", 
                 h2("New Voices Air Monitoring Network - Weekly Review 🌤️"))),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      selectInput("area", "Choose location",
                  choices = c(unique(df_all$location)),
                  selected = "East Oakland Sensor"),
      sliderInput("nth", "Show every Nth hour label", min = 1, max = 6, value = 2),
      helpText("Tip: Hover points to see exact values."),
      width = 3
    ),
    mainPanel(
      fluidRow(
        column(
          width = 6,
          div(class = "card",
              h4("Weekly Average PM2.5"),
              div(class = "valuebox",
                  span(class = "icon", "🫁"),
                  div(class = "text",
                      span(class = "value", textOutput("avgBox", inline = TRUE)),
                      span(class = "subtitle", "µg/m³ (selected range)")
                  )
              )
          )
        ),
        column(
          width = 6,
          div(class = "card",
              h4("Peak Hour"),
              div(class = "valuebox",
                  span(class = "icon", "🕒"),
                  div(class = "text",
                      span(class = "value", textOutput("peakHour", inline = TRUE)),
                      span(class = "subtitle", "Local time (PT)")
                  )
              )
          )
        )
      ),
      div(class = "card",
          h4("Average PM2.5 by Day of Week"),
          plotlyOutput("dowPlot", height = "420px")
      ),
      div(class = "card",
          h4("Average PM2.5 by Hour of Day"),
          plotlyOutput("hourPlot", height = "420px")
      ),
      div(class = "card",
          h4("What does PM2.5 mean?"),
          p("PM2.5 are tiny particles ≤ 2.5 µm that can reach deep into the lungs and bloodstream.",
            " Lower is better. Try to keep long exposures below ~12–15 µg/m³ when possible.")
      ),
      width = 9
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  thematic_shiny()
  
  # Location-only filtered data (one week's worth already)
  df_filtered <- reactive({
    req(nrow(df_all) > 0, input$area)
    df_all |> filter(location == input$area)
  })
  
  # ---- summaries ----
  df_hour <- reactive({
    x <- df_filtered()
    x |>
      group_by(hour_of_day) |>
      summarise(hour_of_day_avg = mean(`pm2.5_atm`, na.rm = TRUE), .groups = "drop") |>
      arrange(hour_of_day)
  })
  
  df_dow <- reactive({
    x <- df_filtered()
    x |>
      group_by(dow) |>
      summarise(dow_avg = mean(`pm2.5_atm`, na.rm = TRUE), .groups = "drop") |>
      mutate(weekday = factor(labels_dow[dow], levels = labels_dow)) |>
      arrange(dow)
  })
  
  # ---- hour plot ----
  output$hourPlot <- renderPlotly({
    dat <- df_hour()
    tick_idx <- seq(0, 23, by = input$nth)
    
    plot_ly(
      dat,
      x = ~hour_of_day, y = ~hour_of_day_avg,
      type = "scatter", mode = "lines+markers",
      fill = "tozeroy",
      text = ~paste0(
        "Hour: ", labels_12h[hour_of_day + 1],
        "<br>Avg: ", round(hour_of_day_avg, 1), " µg/m³"
      ),
      hoverinfo = "text"
    ) |>
      layout(
        xaxis = list(
          title = "Hour of Day (PT)",
          tickmode = "array",
          tickvals = tick_idx,
          ticktext = labels_12h[tick_idx + 1]
        ),
        yaxis = list(title = "Average PM2.5 (µg/m³)"),
        margin = list(l = 60, r = 20, t = 10, b = 60)
      ) |>
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","lasso2d"))
  })
  
  # ---- day-of-week plot (use the column!) ----
  output$dowPlot <- renderPlotly({
    dat <- df_dow()
    
    plot_ly(
      dat,
      x = ~weekday,              # <— use precomputed label column
      y = ~dow_avg,
      type = "bar",
      text = ~round(dow_avg, 1),
      textposition = "auto",
      insidetextfont = list(color = "white", family = "Rubik", size = 18),
      hoverinfo = "text",
      marker = list(color = "#9DBF3B", line = list(color = "#6A994E", width = 1.5))
    ) |>
      layout(
        title = list(text = NULL),
        xaxis = list(title = "", tickfont = list(family = "Rubik, sans-serif", size = 14, color = "#002942")),
        yaxis = list(
          title = "Average PM2.5 (µg/m³)",
          titlefont = list(family = "Rubik, sans-serif", size = 16, color = "#002942"),
          tickfont  = list(family = "Rubik, sans-serif", size = 13, color = "#002942"),
          gridcolor = "rgba(0,0,0,0.05)"
        ),
        margin = list(l = 70, r = 30, t = 60, b = 70)
      ) |>
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","lasso2d"))
  })
  output$avgBox <- renderText({
    dat <- df_hour()
    req(nrow(dat) > 0)
    round(mean(dat$hour_of_day_avg, na.rm = TRUE), 1)
  })
  
  output$peakHour <- renderText({
    dat <- df_hour()
    req(nrow(dat) > 0)
    labels_12h[dat$hour_of_day[which.max(dat$hour_of_day_avg)] + 1]
  })
  
}


shinyApp(ui, server)

