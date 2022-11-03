# Youth Resilience in PR Shiny App

# Import libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(leaflet)
library(geojsonR)

# PR map
pr <- rgdal::readOGR("https://raw.githubusercontent.com/commonwealth-of-puerto-rico/crime-spotter/master/public/data/municipalities.geojson")

# Build app
ui <- fluidPage(theme = shinytheme("darkly"),
                
                # Application title
                titlePanel("Enhancing Children's Resilience to Adversity in Puerto Rico",
                           windowTitle = "Youth Resilience in PR"),
                
                # Tab 1
                tabsetPanel(type = "pills", id = "tabselected",
                            tabPanel("Background Information", value = 1
                            ),
                            
                            # Tab 2
                            tabPanel("Tools", value = 2
                            ),
                            
                            # Tab 3
                            tabPanel("Resources", value = 3
                            ),
                            
                            # Tab 4
                            tabPanel("Classroom Activities", value = 4
                            ),
                            
                            # Tab 5
                            tabPanel("Data Sources", value = 5
                            ),
                ),
                
                mainPanel(
                    conditionalPanel(condition = "input.tabselected==1",
                                     htmlOutput("youthinfo")
                    ),
                    # conditionalPanel(condition = "input.tabselected==2"
                    # ),
                    conditionalPanel(condition = "input.tabselected==3",
                                     leafletOutput("prmap"),
                                     textOutput("prtitle")
                    ),
                    # conditionalPanel(condition = "input.tabselected==4"
                    # ),
                ),
)

# Define server logic
server <- function(input, output) {
    output$youthinfo <- renderUI ({
        HTML(paste(
            "", "<b><img src=https://www.youthdynamics.org/wp-content/uploads/2022/07/Drop-the-Labels-Kids-Shine-When-We-Focus-on-Strengths.png></b>", "",
            "", "<b>What is the problem?</b>","",
            "<i>Sample text:</i> Covid-19 and other disasters have highlighted critical
            gaps in mental health care for children and youth in the US and PR. 
            Advancing equity and promoting resilience in mental health for this
            age demographic can help to strengthen the impact of disasters in
            the community.",
            sep = "<br/>"
        ))
    })
    output$prtitle <- renderText({
        "Map of Puerto Rico: In-Person Resource Locations"
    })
    output$prmap <- renderLeaflet({
        leaflet(pr) %>%
            addTiles() %>%
            addPolygons(fillOpacity = .01,
                        label = ~NAME,
                        opacity = 1,
                        weight = 1.5,
                        highlightOptions = highlightOptions(color = "#FF0000",
                                                            weight = 5))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)