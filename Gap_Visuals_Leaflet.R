

# Libraries ---------------------------------------------------------------

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(thematic)
library(readr)
library(lubridate)
library(leaflet)


# Read in data ------------------------------------------------------------

#gap_df <- read.csv('~/Weekly_AQ/daily_data_2018_to_2024.csv')


# Group into Monthly ------------------------------------------------------

# monitor_timeline <- gap_df |> 
#   mutate(
#     Year = year(Date),
#     Month = month(Date)
#   ) |> 
#   group_by(Site_ID, Site_Name, Latitude, Longitude, Year, Month) |> 
#   summarize(.groups = "drop")

monitor_by_year <- monitor_timeline |> 
  group_by(Site_ID, Site_Name, Latitude, Longitude, Year) |> 
  summarize(.groups = "drop")



# Make map ----------------------------------------------------------------

leaflet(filter(monitor_by_year, Year == 2024)) |> 
  addTiles() |> 
  addCircleMarkers(~Longitude, ~Latitude, popup =~Site_Name)







