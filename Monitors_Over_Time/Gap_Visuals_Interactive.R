#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Monitors Over Time"),

    #slider
    sliderInput("year", "Select year:", min = 2018, max = 2025, value = 2018, sep = ""),
    leafletOutput("map")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$map <- renderLeaflet({
    leaflet() %>% addTiles()
  })
  
  observe({
    filtered <- monitor_by_year %>% filter(Year == input$year)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = filtered,
        lng = ~Longitude, lat = ~Latitude,
        popup = ~paste0("<b>", Site_Name, "</b><br>Installed: ", Year),
        radius = 8, fillOpacity = 0.7, color = "#5A1644"
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

