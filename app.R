# Youth Resilience in PR Shiny App

# Import libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(geojsonR)

# PR map
pr <- rgdal::readOGR("https://raw.githubusercontent.com/commonwealth-of-puerto-rico/crime-spotter/master/public/data/municipalities.geojson")
mapdf <- read_csv("mapdf1.csv")


# Build app
ui <- fluidPage(theme = shinytheme("darkly"),
                
                # Application title
                titlePanel("SunnyR: A Mental Health Dashboard for Youth in PR",
                           windowTitle = "Youth Resilience in PR"),
                
                # Tab 1
                tabsetPanel(type = "pills", id = "tabselected",
                            tabPanel("Background Information", value = 1
                            ),
                            
                            # Tab 2
                            tabPanel("Survey", value = 2
                            ),
                            
                            # Tab 3
                            tabPanel("Visualizations", value = 3
                            ),
                            
                            # Tab 4
                            tabPanel("Resources", value = 4
                            ),
                            
                            # Tab 5
                            tabPanel("Classroom Activities", value = 5
                            ),
                            
                            # Tab 6
                            tabPanel("Data Sources", value = 6
                            ),
                ),
                
                mainPanel(
                  conditionalPanel(condition = "input.tabselected==1",
                                   htmlOutput("youthinfo")
                  ),
                  # conditionalPanel(condition = "input.tabselected==2"
                  # ),
                  # conditionalPanel(condition = "input.tabselected==3"
                  # ),
                  conditionalPanel(condition = "input.tabselected==4",
                                   htmlOutput("prtitle"),
                                   leafletOutput("prmap"),
                                   htmlOutput("pronline")
                  ),
                  # conditionalPanel(condition = "input.tabselected==5"
                  # ),
                ),
)

# Define server logic
server <- function(input, output) {
  output$youthinfo <- renderUI({
    includeHTML("backgroundinfo.html")
  })
  
  # Resources tab
  output$prtitle <- renderText({
    paste("<h3><b>In-Person Resource Locations in PR</b></h3>")
  })
  output$pronline <- renderUI({
    includeHTML("onlineresources.html")
  })
  output$prmap <- renderLeaflet({
    leaflet(pr) %>%
      addTiles() %>%
      addPolygons(fillOpacity = .01,
                  label = ~NAME,
                  opacity = 1,
                  weight = 1.5,
                  highlightOptions = highlightOptions(color = "#FF0000",
                                                      weight = 5)) %>%
      addCircleMarkers(
        data = mapdf,
        radius = 7,
        lng = ~Lng,
        lat = ~Lat,
        popup = ~Link
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)