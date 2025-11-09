
# ------------------------------------------------------------
# app.R — Monitors + Emitters + AB617 Boundaries
# ------------------------------------------------------------

library(shiny)
library(leaflet)
library(leaflet.extras)
library(readr)
library(dplyr)
library(jsonlite)
library(httr)
library(sf)

# ------------------------------------------------------------
# Helper: Convert PurpleAir JSON to data frame
# ------------------------------------------------------------
make_tables <- function(location_name, json_text) {
  js <- fromJSON(json_text)
  df <- as.data.frame(js$data)
  names(df) <- js$fields
  df
}

# ------------------------------------------------------------
# Monitor Data (from JSON blobs)
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# Emitters (BAAQMD Permitted Facilities)
# ------------------------------------------------------------
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

# ------------------------------------------------------------
# Fetch AB617 Polygon Boundaries (sf)
# ------------------------------------------------------------
ab617_url <- "https://services6.arcgis.com/xCuDJnRShQmTqPGL/arcgis/rest/services/UpdatedAB617Bounds071823/FeatureServer/0/query"
params <- list(where="1=1", outFields="*", f="geojson", returnGeometry="true")

ab617_boundaries <- NULL
resp <- GET(ab617_url, query = params)

if (status_code(resp) == 200) {
  ab617_boundaries <- st_read(content(resp, "text", encoding="UTF-8"), quiet = TRUE)
  ab617_boundaries <- st_transform(ab617_boundaries, 4326)
  message("✅ AB617 boundaries loaded: ", nrow(ab617_boundaries))
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Air Monitors + Emitters + AB617 Boundaries"),
  tags$head(tags$style(HTML(".leaflet-container { height: 90vh !important; }"))),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Map Layers"),
      checkboxInput("show_monitors", "Show Monitors", TRUE),
      checkboxInput("show_emitters", "Show Emission Sources", TRUE),
      checkboxInput("show_ab617", "Show AB617 Boundaries", TRUE),
      hr(),
      tags$small("Emitters are filtered by Cancer Risk > 10, Chronic Hazard > 0.5, or PM2.5 > 0.1.")
    ),
    
    mainPanel(
      leafletOutput("map", height = "90vh")
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # map center (combined)
  ctr_lat <- mean(c(monitors$latitude, emissions_data_filtered$Latitude), na.rm = TRUE)
  ctr_lon <- mean(c(monitors$longitude, emissions_data_filtered$Longitude), na.rm = TRUE)
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = ctr_lon, lat = ctr_lat, zoom = 10) |>
      addScaleBar(position = "bottomleft") |>
      addFullscreenControl()
  })
  
  observe({
    
    proxy <- leafletProxy("map")
    
    # ---- Monitors ----
    if (input$show_monitors) {
      proxy |>
        clearGroup("Monitors") |>
        addCircleMarkers(
          data = monitors,
          lng = ~longitude, lat = ~latitude,
          radius = 7, weight = 1, color = "#1F4E79",
          fillColor = "#20b2aa", fillOpacity = 0.95,
          label = ~name,
          popup = ~paste0("<b>", name, "</b><br>ID: ", sensor_index, "<br>",
                          round(latitude,5), ", ", round(longitude,5)),
          group = "Monitors",
          clusterOptions = markerClusterOptions()
        )
    } else proxy |> hideGroup("Monitors")
    
    
    # ---- Emitters ----
    if (input$show_emitters) {
      proxy |>
        clearGroup("Emitters") |>
        addCircleMarkers(
          data = emissions_data_filtered,
          lng = ~Longitude, lat = ~Latitude,
          radius = ~pmin(20, sqrt(Cancer.Risk)),
          stroke = FALSE,
          fillColor = "#E63946", fillOpacity = 0.8,
          label = ~Facility.Name,
          popup = ~paste0("<b>", Facility.Name, "</b><br/>",
                          Address, ", ", City, "<br/>",
                          "Cancer Risk: ", Cancer.Risk),
          group = "Emitters",
          clusterOptions = markerClusterOptions()
        )
    } else proxy |> hideGroup("Emitters")
    
    
    # ---- AB617 Boundaries ----
    if (!is.null(ab617_boundaries) && input$show_ab617) {
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
    } else proxy |> hideGroup("AB617")
    
    
    # ---- layer controls ----
    proxy |> addLayersControl(
      overlayGroups = c("Monitors", "Emitters", "AB617"),
      options = layersControlOptions(collapsed = FALSE)
    )
  })
}

shinyApp(ui, server)
