# app.R
library(shiny)
library(bslib)
library(dplyr)
library(thematic)
library(readr)
library(lubridate)
library(magrittr)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(httr)
library(sf)

# Data Parsing Function ---------------------------------------------------
clean_monitor_data <- function(path, location_name) {
  df <- read.csv(path)
  df <- df |>
    mutate(
      parsed_pt_date = with_tz(ymd_hms(time_stamp), "America/Los_Angeles"),
      hour_of_day = hour(parsed_pt_date),
      day = day(parsed_pt_date),
      dow = wday(parsed_pt_date),
      location = location_name
    )
  df
}

# Read in Data --------------------------------------------------------------------
oakland_mon <- clean_monitor_data("weekly_data/PurpleAir_Week_Oct19_Oct26_2025/EastOaklandRoseOct19to26.csv", "East Oakland Sensor")
pittsburg <- clean_monitor_data("weekly_data/PurpleAir_Week_Oct19_Oct26_2025/PittsburgRoseOct19to26.csv", "Pittsburg Sensor")
pittsburg_high <- clean_monitor_data("weekly_data/PurpleAir_Week_Oct19_Oct26_2025/PittsburgHighRoseOct19to26.csv", "Pittsburg High Sensor")
richmond <- clean_monitor_data("weekly_data/PurpleAir_Week_Oct19_Oct26_2025/PointRichmondRoseOct19to26.csv", "Point Richmond Sensor")

df_all <- bind_rows(oakland_mon, pittsburg, pittsburg_high, richmond)

# Label helpers

labels_12h <- c(
  "12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM",
  "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM",
  "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM",
  "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM"
)

labels_dow <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# -----------------------------
# Map Data Setup
# -----------------------------

# Helper: Convert PurpleAir JSON to data frame
make_tables <- function(location_name, json_text) {
  js <- fromJSON(json_text)
  df <- as.data.frame(js$data)
  names(df) <- js$fields
  df
}

# Monitor Data (from JSON blobs)
eo_txt <- '{"fields" : ["sensor_index","name","latitude","longitude"], "data" : [ [265689,"CBE24",37.731792,-122.19073], [266811,"CBE38",37.72728,-122.18157], [266840,"CBE46",37.739067,-122.1748], [267293,"CBE28",37.72791,-122.18439], [267541,"CBE08",37.726463,-122.18206], [267699,"CBE10",37.742733,-122.17396], [41399,"Stonehurst",37.7404,-122.17176], [77621,"2300 Davis St",37.71618,-122.18827], [87307,"Ashoka 1",37.738663,-122.163445], [112478,"Stoakes Ave",37.73171,-122.16329], [176699,"Gate 510",37.717243,-122.17996], [254813,"East Oakland Rose Monitor",37.73012,-122.18621] ]} '
east_oakland <- make_tables("east_oakland", eo_txt)

richmond_txt <- '{"fields" : ["sensor_index","name","latitude","longitude"], "data" : [ [61233,"Montana St",37.922974,-122.383064], [65413,"Kittyweyr",37.925434,-122.38814], [118991,"At Street",37.928528,-122.39458], [119785,"Near bay",37.92835,-122.394745], [152254,"154 collins GWR",37.93226,-122.37145], [190417,"GWR Marine st",37.930088,-122.391945], [196181,"GWR Golden Gate ave",37.927853,-122.390144], [202539,"Sanderling",37.908524,-122.38181], [219543,"Castro St GWR",37.928017,-122.388855], [219573,"GWR Chanslor Estate",37.932674,-122.36682], [219741,"343 River GWR",37.934875,-122.37269], [219775,"GWR Fire station 62",37.948196,-122.36503], [219773,"GWR Bridge Art space",37.928345,-122.36937], [243393,"Seacliff Pt Richmond",37.911755,-122.376144], [254831,"Point Richmond Rose Monitor",37.928593,-122.38631] ]} '
richmond <- make_tables("richmond", richmond_txt)

pittsburg_txt <- '{"fields" : ["sensor_index","name","latitude","longitude"], "data" : [ [278161,"Pittsburg High Rose Monitor",38.022194,-121.884964], [120703,"PittsburgOldTown",38.034157,-121.88032], [171605,"Annettes sensor",38.002666,-121.89663], [254776,"Pittsburg Rose Monitor",38.024426,-121.91051] ] }'
pittsburg <- make_tables("pittsburg", pittsburg_txt)

monitors <- bind_rows(east_oakland, richmond, pittsburg) |>
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

# Emitters (BAAQMD Permitted Facilities)
emissions_data <- read.csv(
  "~/Weekly_AQ/apps/2020_BAAQMD_Permitted_Stationary_Sources_20251104.csv",
  stringsAsFactors = FALSE
)

emissions_data_filtered <- emissions_data |>
  mutate(
    Cancer.Risk = as.numeric(Cancer.Risk),
    Chronic.Hazard = as.numeric(Chronic.Hazard),
    PM.2.5 = as.numeric(PM.2.5)
  ) |>
  filter(
    Cancer.Risk > 10 |
      Chronic.Hazard > 0.5 |
      PM.2.5 > 0.1
  ) |>
  filter(!is.na(Latitude) & !is.na(Longitude))

# Fetch AB617 Polygon Boundaries (sf)
ab617_url <- "https://services6.arcgis.com/xCuDJnRShQmTqPGL/arcgis/rest/services/UpdatedAB617Bounds071823/FeatureServer/0/query"
params <- list(where = "1=1", outFields = "*", f = "geojson", returnGeometry = "true")

ab617_boundaries <- NULL
tryCatch(
  {
    resp <- GET(ab617_url, query = params)
    if (status_code(resp) == 200) {
      ab617_boundaries <- st_read(content(resp, "text", encoding = "UTF-8"), quiet = TRUE)
      ab617_boundaries <- st_transform(ab617_boundaries, 4326)
    }
  },
  error = function(e) {
    message("Could not load AB617 boundaries: ", e$message)
  }
)

# -----------------------------
# Theme (works on older bslib)
# -----------------------------
theme_youth <- bs_theme(
  version = 5,
  bootswatch = "minty",
  base_font = font_google("Rubik", wght = 400),
  heading_font = font_google("Barlow Condensed", wght = 700),
  primary = "#D5E7F0",
  secondary = "#9DBF3B",
  success = "#fa846d"
)

# -----------------------------
# Window Maker Function
# -----------------------------

# Finding best times + exposure windows (uses base data.frame + dplyr joins)
find_best_worst_windows <- function(hour_df,
                                    window_size = 3,
                                    allowed_start = 6,
                                    allowed_end = 22,
                                    labels_in = NULL) {
  # expect hour_df has columns: hour_of_day (0-23) and hour_of_day_avg
  # create a full 0:23 frame and join
  full_hours <- data.frame(hour_of_day = 0:23)
  if (!("hour_of_day_avg" %in% names(hour_df))) {
    stop("hour_df must contain column 'hour_of_day_avg'")
  }
  full_hours <- full_hours |>
    dplyr::left_join(hour_df |> dplyr::select(hour_of_day, hour_of_day_avg),
      by = "hour_of_day"
    ) |>
    dplyr::arrange(hour_of_day)

  n <- nrow(full_hours)

  # compute rolling averages for windows that don't wrap (end <= 23)
  rolling_avg <- rep(NA_real_, n)
  for (i in 1:n) {
    idx_end <- i + window_size - 1
    if (idx_end > n) {
      rolling_avg[i] <- NA_real_
    } else {
      rolling_avg[i] <- mean(full_hours$hour_of_day_avg[i:idx_end], na.rm = TRUE)
      # if all values NA -> result is NaN -> set to NA
      if (is.nan(rolling_avg[i])) rolling_avg[i] <- NA_real_
    }
  }
  full_hours$rolling_avg <- rolling_avg

  # allowed starts (integers)
  allowed_starts <- allowed_start:allowed_end
  allowed_starts <- allowed_starts[(allowed_starts + window_size - 1) <= allowed_end]

  if (length(allowed_starts) == 0) {
    return(list(
      error = TRUE,
      message = paste0(
        "No valid ", window_size, "-hour windows fully inside ",
        allowed_start, "–", allowed_end, ". Try reducing window_size or expanding allowed range."
      )
    ))
  }

  candidates <- full_hours |> dplyr::filter(hour_of_day %in% allowed_starts)

  if (all(is.na(candidates$rolling_avg))) {
    return(list(
      error = TRUE,
      message = "No valid averages (maybe too many missing hourly values)."
    ))
  }

  # pick best (min) and worst (max)
  best_row <- candidates |> dplyr::slice_min(rolling_avg, n = 1, with_ties = FALSE)
  worst_row <- candidates |> dplyr::slice_max(rolling_avg, n = 1, with_ties = FALSE)

  make_label <- function(start_hr) {
    end_hr <- start_hr + window_size - 1
    # do not wrap in label (we only choose windows within the allowed range)
    paste0(labels_12h[start_hr + 1], " – ", labels_12h[(end_hr %% 24) + 1])
  }

  list(
    error = FALSE,
    window_size = window_size,
    allowed_range = paste0(allowed_start, "–", allowed_end),
    best = list(
      start_hour = best_row$hour_of_day,
      label = make_label(best_row$hour_of_day),
      avg = round(best_row$rolling_avg, 1)
    ),
    worst = list(
      start_hour = worst_row$hour_of_day,
      label = make_label(worst_row$hour_of_day),
      avg = round(worst_row$rolling_avg, 1)
    )
  )
}


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
      .container-fluid {
        padding-top: 40px;
        padding-bottom: 20px;
      }
      .sidebarLayout {
        margin-top: 10px;
      }
      .sidebarPanel, .mainPanel {
        padding-top: 10px;
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
      .highlight-card {
        border-radius: 20px;
        padding: 20px 22px;
        display: flex;
        flex-direction: column;
        gap: 14px;
        border: 1px solid rgba(0, 41, 66, 0.08);
      }
      .highlight-card.best-card {
        background: linear-gradient(135deg, #f0f7ec 0%, #ffffff 72%);
      }
      .highlight-card.worst-card {
        background: linear-gradient(135deg, #fff4ef 0%, #ffffff 72%);
      }
      .highlight-card__header {
        display: flex;
        align-items: center;
        gap: 12px;
      }
      .highlight-card__icon {
        width: 44px;
        height: 44px;
        border-radius: 14px;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 24px;
        background: rgba(0, 41, 66, 0.08);
      }
      .best-card .highlight-card__icon {
        background: rgba(157, 191, 59, 0.22);
      }
      .worst-card .highlight-card__icon {
        background: rgba(250, 132, 109, 0.22);
      }
      .highlight-card__titles {
        display: flex;
        flex-direction: column;
        gap: 4px;
      }
      .highlight-card__eyebrow {
        font-size: 11px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.6px;
        opacity: 0.72;
      }
      .highlight-card__header h4 {
        margin: 0;
        font-size: 22px;
        letter-spacing: -0.2px;
      }
      .highlight-card__content {
        display: flex;
        flex-direction: column;
        gap: 12px;
      }
      .highlight-card__window {
        display: flex;
        flex-direction: column;
        gap: 2px;
      }
      .highlight-card__window .label {
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        opacity: 0.7;
      }
      .highlight-card__window .value {
        font-size: 24px;
        font-weight: 700;
        letter-spacing: 0.2px;
      }
      .highlight-card__chip {
        display: inline-flex;
        align-items: baseline;
        gap: 6px;
        padding: 10px 14px;
        border-radius: 999px;
        font-weight: 600;
        font-size: 13px;
        width: fit-content;
      }
      .highlight-card__chip .chip-label {
        text-transform: uppercase;
        letter-spacing: 0.5px;
        font-size: 11px;
        opacity: 0.7;
      }
      .highlight-card__chip .chip-value {
        font-size: 18px;
        font-weight: 700;
      }
      .highlight-card__chip.good-chip {
        background: rgba(157, 191, 59, 0.18);
        color: #325704;
      }
      .highlight-card__chip.warn-chip {
        background: rgba(250, 132, 109, 0.22);
        color: #8c2e18;
      }
      .highlight-card__footer {
        font-size: 12px;
        color: #4a5b67;
        margin-top: 4px;
      }
      .map-legend {
        background: rgba(255, 255, 255, 0.94);
        border-radius: 12px;
        padding: 12px 14px;
        box-shadow: 0 6px 14px rgba(0, 0, 0, 0.1);
        border: 1px solid rgba(0, 41, 66, 0.08);
        font-size: 12px;
        color: #22313f;
        min-width: 170px;
      }
      .map-legend__title {
        font-weight: 700;
        margin-bottom: 8px;
      }
      .map-legend__item {
        display: flex;
        align-items: center;
        gap: 8px;
        margin-bottom: 6px;
      }
      .map-legend__item:last-child {
        margin-bottom: 0;
      }
      .map-legend__swatch {
        width: 14px;
        height: 14px;
        border-radius: 50%;
        display: inline-block;
      }
      .map-legend__swatch--emitters {
        background: #fa846d;
      }
      .map-legend__swatch--monitors {
        background: #9DBF3B;
      }
      .page-title {
        margin: 20px 0 24px 0;
        text-align: center;
        padding: 0 20px;
      }
      .page-title h2 {
        margin-bottom: 6px;
        color: #002942;
        font-weight: 700;
        letter-spacing: -0.5px;
      }
      .page-title h3 {
        margin-bottom: 16px;
        margin-top: 0;
      }
      .page-title .intro-text {
        color: #002942;
        font-size: 13px;
        line-height: 1.5;
        margin: 0;
        font-weight: 400;
        opacity: 0.85;
      }
      .info-section {
        background: #eef5f8;
        border-radius: 18px;
        padding: 24px 24px 12px 24px;
        margin-bottom: 24px;
        border: 1px solid rgba(0, 41, 66, 0.08);
      }
      .info-section-title {
        color: #002942;
        font-size: 18px;
        font-weight: 700;
        margin-bottom: 18px;
        letter-spacing: 0.2px;
      }
      .card.info-card {
        background: #ffffff;
        border: 1px solid rgba(0, 41, 66, 0.08);
        box-shadow: none;
      }
      .card.info-card h4 {
        color: #002942;
        margin-bottom: 10px;
      }
      .card.info-card p {
        color: #102b3c;
        font-size: 14px;
        line-height: 1.6;
      }
      .card.info-card p a {
        color: #0d4a8b;
        font-weight: 600;
      }
      .pm25-comparison {
        padding: 12px 0;
        text-align: center;
      }
      .pm25-comparison-title {
        margin-bottom: 6px;
        color: #002942;
        font-size: 20px;
        font-weight: 700;
      }
      .pm25-comparison-intro {
        margin-bottom: 12px;
        color: #0b1f2c;
        font-size: 13px;
        line-height: 1.4;
        max-width: 800px;
        margin-left: auto;
        margin-right: auto;
      }
      .pm25-comparison-subtitle {
        margin-bottom: 14px;
        margin-top: 2px;
        color: #666;
        font-size: 11px;
      }
      .pm25-description {
        padding: 10px 14px;
        background: #f8f9fa;
        border-radius: 5px;
        border-left: 3px solid #fa846d;
        color: #002942;
        font-size: 12px;
        line-height: 1.4;
        text-align: left;
        max-width: 700px;
        margin-left: auto;
        margin-right: auto;
        margin-bottom: 10px;
      }
      .pm25-info-boxes {
        display: flex;
        flex-direction: column;
        gap: 10px;
        max-width: 700px;
        margin: 12px auto 0;
      }
      .pm25-circles-container {
        display: flex;
        justify-content: center;
        align-items: flex-end;
        gap: 35px;
        padding: 12px 20px;
        flex-wrap: wrap;
      }
      .pm25-comparison-item {
        display: flex;
        flex-direction: column;
        align-items: center;
      }
      .pm25-comparison-circle {
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        margin-bottom: 6px;
      }
      .pm25-comparison-label {
        font-weight: 600;
        font-size: 13px;
        color: #002942;
        margin-bottom: 1px;
      }
      .pm25-comparison-size {
        font-size: 11px;
        color: #666;
      }
    ")),
    # Tiny helper to set text content of spans by id
  ),
  sidebarLayout(
    sidebarPanel(
      div(
        class = "page-title",
        style = paste(
          "text-align: left;",
          "margin-bottom: 20px;",
          "padding-bottom: 20px;",
          "border-bottom: 1px solid #e0e0e0;",
          sep = " "
        ),
        h2(
          style = paste(
            "font-size: 1.5em;",
            "margin-bottom: 6px;",
            sep = " "
          ),
          "New Voices Air Monitoring Network"
        ),
        h3(
          style = paste(
            "color: #9DBF3B;",
            "font-size: 1.5em;",
            "font-weight: 600;",
            "letter-spacing: 0.5px;",
            "margin-bottom: 12px;",
            "margin-top: 0;",
            sep = " "
          ),
          "Weekly Review"
        ),
        p(
          class = "intro-text", style = "text-align: left; margin: 0;",
          "New Voices Are Rising is measuring air pollution in the most impacted communities in the East Bay. Look at weekly data of sensors measuring PM2.5 in your area."
        )
      ),
      h4("Filters"),
      selectInput("area", "Choose location",
        choices = c(unique(df_all$location)),
        selected = "East Oakland Sensor"
      ),
      div(
        class = "card",
        h4("Weekly Average PM2.5"),
        div(
          class = "valuebox",
          span(class = "icon", "🫁"),
          div(
            class = "text",
            span(class = "value", textOutput("avgBox", inline = TRUE)),
            span(class = "subtitle", "µg/m³ (selected range)")
          )
        )
      ),
      width = 3
    ),
    mainPanel(
      div(
        class = "info-section",
        h4(class = "info-section-title", "Our sensors measure fine particulate matter (PM2.5)"),
        div(
          class = "card info-card",
          div(
            class = "pm25-comparison",
            h3(class = "pm25-comparison-title", "What is PM2.5?"),
            p(
              class = "pm25-comparison-intro",
              "This is fine particulate matter that so small it can reach deep into the lungs and bloodstream. Size comparison (to scale)"
            ),
            #p(class = "pm25-comparison-subtitle", "Size comparison (to scale)"),
            div(
              class = "pm25-circles-container",
              div(
                class = "pm25-comparison-item",
                div(
                  class = "pm25-comparison-circle",
                  style = "width: 70px; height: 70px; background: #002942;",
                  ""
                ),
                div(class = "pm25-comparison-label", "Human Hair"),
                div(class = "pm25-comparison-size", "70 µm")
              ),
              div(
                class = "pm25-comparison-item",
                div(
                  class = "pm25-comparison-circle",
                  style = "width: 11px; height: 11px; background: #4A90E2; border: 2px solid #2E5C8A;",
                  ""
                ),
                div(class = "pm25-comparison-label", "PM10"),
                div(class = "pm25-comparison-size", "10 µm")
              ),
              div(
                class = "pm25-comparison-item",
                div(
                  style = "position: relative; width: 44px; height: 44px; display: flex; align-items: center; justify-content: center; margin-bottom: 6px;",
                  div(style = "width: 44px; height: 44px; border-radius: 50%; border: 2px dashed #9DBF3B; background: rgba(157, 191, 59, 0.1); position: absolute; top: 0; left: 0;"),
                  div(
                    class = "pm25-comparison-circle",
                    style = "width: 3px; height: 3px; background: #fa846d; position: relative; z-index: 1;",
                    ""
                  )
                ),
                div(class = "pm25-comparison-label", "PM2.5"),
                div(class = "pm25-comparison-size", "2.5 µm")
              )
            )
          )
        ),
        div(
          class = "card info-card",
          h4("What is a safe level?"),
          p(
            "The California Air Resources Board recommends keeping long-term exposure near ≤ 12 µg/m³, and the U.S. EPA considers 35 µg/m³ in 24 hours unhealthy.
 Lower is better. ",
            tags$a(
              href = "https://ww2.arb.ca.gov/resources/inhalable-particulate-matter-and-health",
              target = "_blank",
              "Learn more here"
            )
          )
        )
      ),
      ## --- Best / Worst windows cards ---
      fluidRow(
        column(
          width = 6,
          div(
            class = "card highlight-card best-card",
            div(
              class = "highlight-card__header",
              span(class = "highlight-card__icon", "✅"),
              div(
                class = "highlight-card__titles",
                span(
                  class = "highlight-card__eyebrow",
                  "Best exposure"
                ),
                h4("Best Exposure Window")
              )
            ),
            div(
              class = "highlight-card__content",
              div(
                class = "highlight-card__window",
                span(
                  class = "label",
                  "Lowest measurements"
                ),
                span(
                  class = "value",
                  textOutput("bestWindow", inline = TRUE)
                )
              ),
              span(
                class = "highlight-card__chip good-chip",
                span(class = "chip-label", "Average PM2.5"),
                span(
                  class = "chip-value",
                  textOutput("bestWindowAvg", inline = TRUE)
                )
              ),
              p(
                class = "highlight-card__footer",
                "3-hour window, local time 6 AM–10 PM"
              )
            )
          )
        ),
        column(
          width = 6,
          div(
            class = "card highlight-card worst-card",
            div(
              class = "highlight-card__header",
              span(class = "highlight-card__icon", "⚠️"),
              div(
                class = "highlight-card__titles",
                span(
                  class = "highlight-card__eyebrow",
                  "Highest concern"
                ),
                h4("Worst Exposure Window")
              )
            ),
            div(
              class = "highlight-card__content",
              div(
                class = "highlight-card__window",
                span(
                  class = "label",
                  "Time to avoid"
                ),
                span(
                  class = "value",
                  textOutput("worstWindow", inline = TRUE)
                )
              ),
              span(
                class = "highlight-card__chip warn-chip",
                span(class = "chip-label", "Average PM2.5"),
                span(
                  class = "chip-value",
                  textOutput("worstWindowAvg", inline = TRUE)
                )
              ),
              p(
                class = "highlight-card__footer",
                "3-hour window, local time 6 AM–10 PM"
              )
            )
          )
        )
      ),
      div(
        class = "card",
        h4("Average PM2.5 by Day of Week"),
        plotlyOutput("dowPlot", height = "420px"),
        p(
          style = "margin-top: 8px; font-size: 11px; color: #4a5b67; font-style: italic;",
          "Tip: double-click inside the chart to reset the zoom."
        )
      ),
      div(
        class = "card",
        h4("Average PM2.5 by Hour of Day"),
        plotlyOutput("hourPlot", height = "420px"),
        p(
          style = "margin-top: 8px; font-size: 11px; color: #4a5b67; font-style: italic;",
          "Tip: double-click inside the chart to reset the zoom."
        )
      ),
      div(
        class = "card",
        h4("Air Monitoring Network Map"),
        p(
          style = "margin-bottom:10px; font-size:13px; color:#4a5b67; font-weight: 600;",
          "This map shows:"
        ),
        tags$ul(
          style = "margin-bottom:12px; font-size:12px; color:#3a4b57; padding-left: 20px;",
          tags$li(
            "Air quality monitors that New Voices Are Rising installed + ",
            "Purple Air monitors in a 1.5 mile radius of them"
          ),
          tags$li(
            "Stationary emission sources in Oakland, Alameda, Richmond, ",
            "San Pablo, Pittsburg, Rodeo, Bay Point and Antioch—",
            "communities that are overburdened by air quality issues"
          ),
          tags$li(
            tags$b("AB617 boundaries:"),
            " State-designated environmental justice communities prioritized ",
            "for air monitoring and pollution reduction"
          )
        ),
        p(
          style = "margin-bottom:12px; font-size:12px; color:#3a4b57;",
          "Emissions data comes from ",
          tags$a(
            href = "https://data.bayareametro.gov/Pollution/2020-BAAQMD-Permitted-Stationary-Sources/ugiv-shvv/explore/query/SELECT%0A%20%20%60facilityid%60%2C%0A%20%20%60facility_n%60%2C%0A%20%20%60address%60%2C%0A%20%20%60city%60%2C%0A%20%20%60st%60%2C%0A%20%20%60zip%60%2C%0A%20%20%60county%60%2C%0A%20%20%60lat_dd%60%2C%0A%20%20%60long_dd%60%2C%0A%20%20%60cancerrisk%60%2C%0A%20%20%60chronichi%60%2C%0A%20%20%60pm2_5%60%2C%0A%20%20%60source_det%60%2C%0A%20%20%60naics_code%60%2C%0A%20%20%60naics__cod%60%2C%0A%20%20%60naics__c_1%60%2C%0A%20%20%60naics__c_2%60%2C%0A%20%20%60point%60%0AWHERE%0A%20%20caseless_one_of%28%0A%20%20%20%20%60city%60%2C%0A%20%20%20%20%22Oakland%22%2C%0A%20%20%20%20%22Alameda%22%2C%0A%20%20%20%20%22Richmond%22%2C%0A%20%20%20%20%22San%20Pablo%22%2C%0A%20%20%20%20%22Pittsburg%22%2C%0A%20%20%20%20%22Rodeo%22%2C%0A%20%20%20%20%22Bay%20Point%22%2C%0A%20%20%20%20%22Antioch%22%0A%20%20%29/page/filter",
            target = "_blank",
            style = "color: #0d4a8b; font-weight: 600;",
            "2020 BAAQMD Permitted Stationary Sources"
          ),
          "."
        ),
        fluidRow(
          column(
            width = 12,
            checkboxGroupInput("map_layers", "Map Layers",
              choices = list(
                "Show Monitors" = "monitors",
                "Show Emission Sources" = "emitters",
                "Show AB617 Boundaries" = "ab617"
              ),
              selected = c("monitors", "emitters", "ab617"),
              inline = TRUE
            )
          )
        ),
        leafletOutput("map", height = "500px")
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
    req(nrow(dat) > 0)
    tick_idx <- seq(0, 23, by = 2)
    y_max <- suppressWarnings(max(dat$hour_of_day_avg, na.rm = TRUE))
    if (!is.finite(y_max)) {
      y_max <- 0
    }
    y_upper <- max(y_max, 9)
    plot_ly(
      dat,
      x = ~hour_of_day, y = ~hour_of_day_avg,
      type = "scatter", mode = "lines+markers",
      fill = "tozeroy",
      text = ~ paste0(
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
        margin = list(l = 60, r = 20, t = 10, b = 60),
        shapes = list(
          list(
            type = "rect",
            xref = "paper", x0 = 0, x1 = 1,
            yref = "y", y0 = 0, y1 = min(9, y_upper),
            fillcolor = "rgba(157, 191, 59, 0.18)",
            line = list(width = 0),
            layer = "below"
          ),
          list(
            type = "rect",
            xref = "paper", x0 = 0, x1 = 1,
            yref = "y", y0 = 9, y1 = y_upper,
            fillcolor = "rgba(255, 221, 87, 0.22)",
            line = list(width = 0),
            layer = "below"
          )
        )
      ) |>
      plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })


  # ---- day-of-week plot (use the column!) ----
  output$dowPlot <- renderPlotly({
    dat <- df_dow()
    req(nrow(dat) > 0)
    y_max <- suppressWarnings(max(dat$dow_avg, na.rm = TRUE))
    if (!is.finite(y_max)) {
      y_max <- 0
    }
    y_upper <- max(y_max, 9)
    plot_ly(
      dat,
      x = ~weekday, # <— use precomputed label column
      y = ~dow_avg,
      type = "bar",
      text = ~ round(dow_avg, 1),
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
          tickfont = list(family = "Rubik, sans-serif", size = 13, color = "#002942"),
          gridcolor = "rgba(0,0,0,0.05)"
        ),
        margin = list(l = 70, r = 30, t = 60, b = 70),
        shapes = list(
          list(
            type = "rect",
            xref = "paper", x0 = 0, x1 = 1,
            yref = "y", y0 = 0, y1 = min(9, y_upper),
            fillcolor = "rgba(157, 191, 59, 0.18)",
            line = list(width = 0),
            layer = "below"
          ),
          list(
            type = "rect",
            xref = "paper", x0 = 0, x1 = 1,
            yref = "y", y0 = 9, y1 = y_upper,
            fillcolor = "rgba(255, 221, 87, 0.22)",
            line = list(width = 0),
            layer = "below"
          )
        )
      ) |>
      plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })
  output$avgBox <- renderText({
    dat <- df_hour()
    req(nrow(dat) > 0)
    round(mean(dat$hour_of_day_avg, na.rm = TRUE), 1)
  })

  # ---- compute best/worst windows ----
  windows_res <- reactive({
    dat <- df_hour()
    # df_hour should have hour_of_day and hour_of_day_avg
    if (nrow(dat) == 0) {
      return(list(error = TRUE, message = "No hourly data"))
    }
    # call the function (you can change window_size / allowed range here)
    find_best_worst_windows(
      hour_df = dat,
      window_size = 3,
      allowed_start = 6,
      allowed_end = 22
    )
  })

  output$bestWindow <- renderText({
    res <- windows_res()
    if (is.null(res)) {
      return("")
    }
    if (isTRUE(res$error)) {
      return(res$message)
    }
    res$best$label
  })

  output$bestWindowAvg <- renderText({
    res <- windows_res()
    if (is.null(res)) {
      return("")
    }
    if (isTRUE(res$error)) {
      return("")
    }
    paste0("avg ", res$best$avg, " µg/m³")
  })

  output$worstWindow <- renderText({
    res <- windows_res()
    if (is.null(res)) {
      return("")
    }
    if (isTRUE(res$error)) {
      return(res$message)
    }
    res$worst$label
  })

  output$worstWindowAvg <- renderText({
    res <- windows_res()
    if (is.null(res)) {
      return("")
    }
    if (isTRUE(res$error)) {
      return("")
    }
    paste0("avg ", res$worst$avg, " µg/m³")
  })

  # ---- Map ----
  # Map center (combined)
  ctr_lat <- mean(c(monitors$latitude, emissions_data_filtered$Latitude), na.rm = TRUE)
  ctr_lon <- mean(c(monitors$longitude, emissions_data_filtered$Longitude), na.rm = TRUE)

  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = ctr_lon, lat = ctr_lat, zoom = 10) |>
      addScaleBar(position = "bottomleft") |>
      addFullscreenControl() |>
      addControl(
        html = htmltools::HTML(
          as.character(
            htmltools::tags$div(
              class = "map-legend",
              htmltools::tags$div(
                class = "map-legend__title",
                "Map Layers"
              ),
              htmltools::tags$div(
                class = "map-legend__item",
                htmltools::tags$span(
                  class = "map-legend__swatch map-legend__swatch--emitters"
                ),
                htmltools::tags$span(
                  class = "map-legend__label",
                  "Emissions sources"
                )
              ),
              htmltools::tags$div(
                class = "map-legend__item",
                htmltools::tags$span(
                  class = "map-legend__swatch map-legend__swatch--monitors"
                ),
                htmltools::tags$span(
                  class = "map-legend__label",
                  "Air quality monitors"
                )
              )
            )
          )
        ),
        position = "bottomright"
      )
  })

  observe({
    proxy <- leafletProxy("map")

    # ---- Monitors ----
    if ("monitors" %in% input$map_layers) {
      proxy |>
        clearGroup("Monitors") |>
        addCircleMarkers(
          data = monitors,
          lng = ~longitude, lat = ~latitude,
          radius = 7,
          weight = 1,
          color = "#9DBF3B",
          fillColor = "#9DBF3B",
          fillOpacity = 0.9,
          label = ~name,
          popup = ~ paste0(
            "<b>", name, "</b><br>ID: ", sensor_index, "<br>",
            round(latitude, 5), ", ", round(longitude, 5)
          ),
          group = "Monitors",
          clusterOptions = markerClusterOptions()
        )
    } else {
      proxy |> clearGroup("Monitors")
    }

    # ---- Emitters ----
    if ("emitters" %in% input$map_layers) {
      proxy |>
        clearGroup("Emitters") |>
        addCircleMarkers(
          data = emissions_data_filtered,
          lng = ~Longitude, lat = ~Latitude,
          radius = ~ pmin(20, sqrt(Cancer.Risk)),
          stroke = FALSE,
          fillColor = "#fa846d", fillOpacity = 0.8,
          label = ~Facility.Name,
          popup = ~ paste0(
            "<b>", Facility.Name, "</b><br/>",
            Address, ", ", City, "<br/>",
            "Cancer Risk: ", Cancer.Risk
          ),
          group = "Emitters",
          clusterOptions = markerClusterOptions()
        )
    } else {
      proxy |> clearGroup("Emitters")
    }

    # ---- AB617 Boundaries ----
    if (!is.null(ab617_boundaries) && "ab617" %in% input$map_layers) {
      proxy |>
        clearGroup("AB617") |>
        addPolygons(
          data = ab617_boundaries,
          fillColor = "#4CB9E7",
          fillOpacity = 0.12,
          color = "#4CB9E7",
          weight = 2,
          group = "AB617"
        )
    } else {
      proxy |> clearGroup("AB617")
    }

    # ---- layer controls ----
    proxy |> addLayersControl(
      overlayGroups = c("Monitors", "Emitters", "AB617"),
      options = layersControlOptions(collapsed = FALSE)
    )
  })
}


shinyApp(ui, server)
