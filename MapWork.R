
# Install packages --------------------------------------------------------

library(leaflet)
library(leaflet.extras)
library(readr)
library(dplyr)
library(jsonlite)
library(shiny)


# Monitor Data

make_tables <- function(location_name, json_text, save_csv = TRUE) {
  js <- fromJSON(json_text)
  df <- as.data.frame(js$data)
  names(df) <- js$fields
  
  file_name <- paste0(tolower(gsub(" ", "_", location_name)), "_monitors.csv")
  write.csv(df, file_name, row.names = FALSE)
  message("✅ Saved to: ", file_name)
  
  return(df)
}

eo_txt <- '{"fields" : ["sensor_index","name","latitude","longitude"], "data" : [ [265689,"CBE24",37.731792,-122.19073], [266811,"CBE38",37.72728,-122.18157], [266840,"CBE46",37.739067,-122.1748], [267293,"CBE28",37.72791,-122.18439], [267541,"CBE08",37.726463,-122.18206], [267699,"CBE10",37.742733,-122.17396], [41399,"Stonehurst",37.7404,-122.17176], [77621,"2300 Davis St",37.71618,-122.18827], [87307,"Ashoka 1",37.738663,-122.163445], [112478,"Stoakes Ave",37.73171,-122.16329], [176699,"Gate 510",37.717243,-122.17996], [254813,"East Oakland Rose Monitor",37.73012,-122.18621] ]} '
east_oakland_monitors <- make_tables('east_oakland', eo_txt)

richmond_txt <- '{"fields" : ["sensor_index","name","latitude","longitude"], "data" : [ [61233,"Montana St",37.922974,-122.383064], [65413,"Kittyweyr",37.925434,-122.38814], [118991,"At Street",37.928528,-122.39458], [119785,"Near bay",37.92835,-122.394745], [152254,"154 collins GWR",37.93226,-122.37145], [190417,"GWR Marine st",37.930088,-122.391945], [196181,"GWR Golden Gate ave",37.927853,-122.390144], [202539,"Sanderling",37.908524,-122.38181], [219543,"Castro St GWR",37.928017,-122.388855], [219573,"GWR Chanslor Estate",37.932674,-122.36682], [219741,"343 River GWR",37.934875,-122.37269], [219775,"GWR Fire station 62",37.948196,-122.36503], [219773,"GWR Bridge Art space",37.928345,-122.36937], [243393,"Seacliff Pt Richmond",37.911755,-122.376144], [254831,"Point Richmond Rose Monitor",37.928593,-122.38631] ]} '
richmond_monitors <- make_tables('richmond',richmond_txt)

pittsburg_txt <- '{"fields" : ["sensor_index","name","latitude","longitude"], "data" : [ [278161,"Pittsburg High Rose Monitor",38.022194,-121.884964], [120703,"PittsburgOldTown",38.034157,-121.88032], [171605,"Annettes sensor",38.002666,-121.89663], [254776,"Pittsburg Rose Monitor",38.024426,-121.91051] ] }'
pittsburg_monitors <- make_tables('pittsburg', pittsburg_txt )

monitors <- bind_rows(east_oakland_monitors, richmond_monitors, pittsburg_monitors)
monitors <- monitors |>
  mutate(
    latitude  = parse_number(as.character(latitude),
                             locale = locale(decimal_mark = ".", grouping_mark = ",")),
    longitude = parse_number(as.character(longitude),
                             locale = locale(decimal_mark = ".", grouping_mark = ","))
  )


# Emissions Data ----------------------------------------------------------

emissions_data <- read.csv("~/Weekly_AQ/apps/2020_BAAQMD_Permitted_Stationary_Sources_20251104.csv")

#Lets filter out some of these emitters and get higher values
# ---- I want to verify what these units actually mean but in the meantime:
#Cancer.Risk > 10 
#Chronic Hazard > 0.01
#PM.2.5 > 0.1


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
  )


#pop up
popup_html <- ~paste0(
  "<b>", Facility.Name, "</b><br/>",
  Address, ", ", City, " ", Zip, "<br/>",
  "Industry: ", NAICS.Industry, "<br/>",
  "Cancer Risk: ", Cancer.Risk
)

#center map of mean of facilities
ctr_lat <- mean(emissions_data_filtered$Latitude,  na.rm = TRUE)
ctr_lon <- mean(emissions_data_filtered$Longitude, na.rm = TRUE)

#leaflet code
leaflet(emissions_data_filtered) |> 
  addProviderTiles(providers$CartoDB.Positron) |> 
  setView(lng = ctr_lon, lat = ctr_lat, zoom = 10) |> 
  addCircleMarkers(
    lng = ~Longitude, lat = ~Latitude,
    radius = ~ifelse(is.na(Cancer.Risk),4, pmin(sqrt(Cancer.Risk), 20)),
    stroke = FALSE,
    fillColor = "#E63946", fillOpacity = 0.8,
    popup = popup_html,
    label = ~Facility.Name,
    clusterOptions = markerClusterOptions(
      disableClusteringAtZoom = 11,
      spiderfyOnMaxZoom = TRUE,
      maxClusterRadius = 40)
  )

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Monitor Map"),
  tags$head(tags$style(HTML(".leaflet-container { height: 80vh !important; }"))),
  leafletOutput("monitor_map", height = "80vh")
)


# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  # Center
  ctr_lat <- mean(monitors$latitude,  na.rm = TRUE)
  ctr_lon <- mean(monitors$longitude, na.rm = TRUE)
  
  popup_html <- ~paste0(
    "<b>", name, "</b><br/>",
    "ID: ", sensor_index, "<br/>",
    "Lat/Lon: ", round(latitude,5), ", ", round(longitude,5)
  )
  
  output$monitor_map <- renderLeaflet({
    leaflet(monitors) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = ctr_lon, lat = ctr_lat, zoom = 10) |>
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 7, stroke = TRUE, weight = 1, color = "#222",
        fillColor = "#20b2aa", fillOpacity = 0.9,
        label = ~name, popup = popup_html
      ) |>
      addScaleBar(position = "bottomleft") |>
      addFullscreenControl()
  })
}

library(shiny)
shinyApp(ui, server)


