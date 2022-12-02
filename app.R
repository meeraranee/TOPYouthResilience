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
                  conditionalPanel(condition = "input.tabselected==2",
                                   htmlOutput("explanation"),
                                   selectInput("UserInput",  # This is the name of the variable the User Input will be saved to
                                               "1.	In the last two weeks, have you witnessed the following child’s behavior?", #This is what will be displayed for the user
                                               choices = c("","Fearful", "Difficulty sustaining attention", "Restless","None of these")), # the preset choices
                                   h4(textOutput("Result")) # h4 is text size again; here will be the Output send to 
                  ),
                  # conditionalPanel(condition = "input.tabselected==3"
                  # ),
                  conditionalPanel(condition = "input.tabselected==4",
                                   htmlOutput("prtitle"),
                                   leafletOutput("prmap"),
                                   htmlOutput("pronline")
                  ),
                  conditionalPanel(condition = "input.tabselected==5",
                                   htmlOutput("mindful")
                  ),
                  conditionalPanel(condition = "input.tabselected==6",
                                   htmlOutput("datasources")
                  )),
)

# Define server logic
server <- function(input, output) {
  output$youthinfo <- renderUI({
    includeHTML("backgroundinfo.html")
  })
  
  # Survey Tab
  output$explanation <- renderText({
    paste("<h4>This tool will help you identify some potential conditions the
           child may be experiencing. Select the answer that best describes your
           observations</h4>", "",
          "<h5>*Please note the results identified in this survey are not a diagnosis
          and a professional consultation is encouraged.*</h5>", "")#h2 comes form html style and simply is determining basically the text size")
  })
  
  Identifier =function(q.c){  # Your function (could be also set outside of shiny body)
    if (q.c == "Fearful"){
      QuizResult="Traumatice Experience"
    } else if (q.c == "Difficulty sustaining attention") {
      QuizResult="ADHD"
    } else if (q.c == "Restless"){
      QuizResult="Overlap"
    } else if(q.c == "None of these"){
      QuizResult="More information needed"
    }else{
      QuizResult=""
    }
    return(QuizResult)
  }
  output$Result <- renderText({  #render is observing any change of the input
    Identifier(input$UserInput) # is taken users, handing over to the function
    # the function's result will be finally send to the Output
  })
  
  # Classroom Activities Tab
  output$mindful <- renderText({
    includeHTML("mindful.html")
  })
  
  # Resources Tab
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
  
  # Data Sources Tab
  output$datasources <- renderText({
    includeHTML("datasources.html")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)