library(shiny)
library(tidyverse)
library(readxl)
library(leaflet)
# Define UI for application that draws a histogram
ui <- navbarPage("Lugares de interés", id="main",
                 tabPanel("Map", leafletOutput("bbmap", height=1000)),
                 tabPanel("Métodología", DT::dataTableOutput("data")),
                 tabPanel("Read Me",includeMarkdown("readme.md")))

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Import Data and clean it
  
  bb_data <- readxl::read_excel("lugares.xlsx")
  bb_data <- data.frame(bb_data)
  bb_data$lat <-  as.numeric(bb_data$lat)
  bb_data$long <-  as.numeric(bb_data$long) 
  
  # new column for the popup label
  
  # create a color paletter for category type in the data file
  
  pal <- colorFactor(pal = c("#1b9e77", "#d95f02", "purple", "yellow"), domain = c("Diligencia 2012-2020",
                                                                                   "Lugar de hallazgo",
                                                                                   "Recinto de detención",
                                                                                   "Sitio de interés"))
  
  # create the leaflet map  
  output$bbmap <- renderLeaflet({
    leaflet(bb_data) %>% 
      addCircles(lng = ~long, lat = ~lat) %>% 
      addTiles() %>%
      addCircleMarkers(data = bb_data, lat =  ~lat, lng =~long, 
                       radius = 5, popup = ~as.character(bb_data$nombre_lugar), 
                       color = ~pal(bb_data$tipo_lugar),
                       stroke = FALSE, fillOpacity = 0.8)%>%
      addLegend(pal=pal, values=bb_data$tipo_lugar,opacity=1)%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
