#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)
library(readxl)
library(plotly)
trayectorias <- read_excel("trayectorias.xlsx") %>%
  drop_na(identificacion) %>%
  drop_na(lat) %>%
  mutate(anno_desaparicion = lubridate::year(fecha_desaparicion_muerte))

# Define UI for application that draws a histogram
ui <- navbarPage("Plan Nacional de Búsqueda", id="main",
                 tabPanel("Trayectorias", 
                          sidebarPanel(width = 3,
                            checkboxGroupInput("tipo_caso", "Tipo de caso", 
                                               choices = list(
                                                 "Desaparecido/a con información de muerte" = "ASESINADO/A SIN ENTREGA DE CUERPO",
                                                 "Desaparecido/a" = "DESAPARECIDO/A"),
                                               selected = c("ASESINADO/A SIN ENTREGA DE CUERPO", "DESAPARECIDO/A")),
                            sliderInput('anno_desaparicion', 'Año de desaparición', min = 1973, max = 1990, value = c(1973, 1990)),
                            plotlyOutput("histplot")
                          ),
                          mainPanel(width = 9,
                          leafletOutput("bamap", height = 900))))

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Historiograma de fechas de desaparición
  output$histplot <- renderPlotly({
    pal <- colorFactor(pal = c("purple", "yellow"), domain = c("IDENTIFICADO/A", "NO IDENTIFICADO/A"))
    grafico <- trayectorias %>% filter(tipo_caso %in% c(input$tipo_caso),
                                    anno_desaparicion >= input$anno_desaparicion[1] & anno_desaparicion <= input$anno_desaparicion[2])
    figura <- grafico %>%
      ggplot(
        aes(x = as.Date(fecha_desaparicion_muerte),
            fill = tipo_caso)
      ) +
      geom_histogram(color = "black") +
      labs(x = "Fecha de desaparición/muerte",
           y = "Número de víctimas",
           title = "Historiograma de desapariciones",
           subtitle = "Según fecha de detención o secuestro") +
      scale_fill_brewer(type = "qual") +
      theme_classic() +
      theme(legend.position = "top",
            legend.title = element_blank())
    
    plotly::ggplotly(figura) %>% layout(legend = list(orientation = "h", x = 0, y = -0.2, title = list(text = '')))
      
  })
  
  # mapa trayectorias  
  output$bamap <- renderLeaflet({
    pal <- colorFactor(pal = c("purple", "yellow"), domain = c("IDENTIFICADO/A", "NO IDENTIFICADO/A"))
    mapa <- trayectorias %>% filter(tipo_caso %in% c(input$tipo_caso),
                                    anno_desaparicion > input$anno_desaparicion[1] & anno_desaparicion < input$anno_desaparicion[2])
    leaflet(mapa) %>% 
      addTiles() %>%
      addCircleMarkers(
        lng=~long,
        lat=~lat,
        radius = 5,
        clusterOptions = markerClusterOptions(),
        label = ~paste0(victima, " - ", tipo_caso),
        color = ~pal(identificacion),
        stroke = FALSE, fillOpacity = 0.8,
        popup = ~paste0(
          "<B>", 
          tipo_caso,"<br/>",
          "<br/>",
          "Nombre: ", victima, "<br/>",
          "Sexo: ", sexo, "<br/>",
          "Edad: ", edad, " años", "<br/>",
          "Nacionalidad: ", nacionalidad, "<br/>",
          "Tipo de ocupación: ", actividad, "<br/>",
          "Filiación política: ", militancia, "<br/>",
          "Cargo: ", cargo, "<br/>",
          "Fecha de detención o muerte: ", fecha_desaparicion_muerte, "<br/>",
          "<br/>",
          "Tipo de lugar: ", tipo_lugar, "<br/>",
          "Dirección: ", lugar, "<br/>",
          "<br/>",
          "Represores: ", agentes_responsables,"<br/>")
      ) %>%
      addLegend(pal=pal, values=trayectorias$identificacion,opacity=1)%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
