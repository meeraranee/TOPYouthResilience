# Youth Resilience in PR Shiny App

# Import libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(geojsonR)

# PR map
pr <- rgdal::readOGR("https://raw.githubusercontent.com/commonwealth-of-puerto-rico/crime-spotter/master/public/data/municipalities.geojson")
tt1 <- paste(sep = "<br/>",
             paste0("<b><a href='http://csmpr.org/services/clinica-arecibo'>Corporación de Servicios Médicos, Hatillo</a></b>"),
             paste0("Road PR-2 Km 86.6 Medical Tower Building"),
             paste0("Hatillo, PR")
)
# df <- data.frame(
#     "Name, Lat, Long
#     Corporación de Servicios Médicos, Hatillo, -66.81581900259775, 18.48760684076348"
# ))
# df <- tribble(
#    ~Name, ~Lat, ~Long,
# "Corporación de Servicios Médicos, Hatillo", 18.48760684076348, -66.81581900259775"
# # )
mapdf <- read_csv("mapdf.csv")
head(mapdf)



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
  output$youthinfo <- renderUI ({
    HTML(paste(
      "", "<br><b><img src=https://www.youthdynamics.org/wp-content/uploads/2022/07/Drop-the-Labels-Kids-Shine-When-We-Focus-on-Strengths.png></b></br>", "",
      "<br><h3><b>About the App</b></h3>",
      "Youths in Puerto Rico face a range of challenges, such as dealing with Adverse Childhood
      Experiences (ACEs) or negative outcomes of climate disasters. Because of this, some students may struggle with 
      difficult realities. Educators spend lots of time with their students and are often the first
      adults to notice concerning behaviors. This app is designed to help educators in Puerto Rico help
      students ages 12-17 face adversity and promote resilience by analyzing potential behavioral
      disorders and finding helpful resources. Mindful activities may also be encouraged in the classroom
      to promote a healthier learning environment and a safe space. If appropriate, educators may also
      provide information discovered with this app to parents/guardians of targeted youth. (Separate into
      separate paragraph and go into detail -- no tracking/saving on app. if you come back to app,
            no history is maintained, will have to re-enter info if coming back to app) No input is being
            recorded or shared with any other source.</br>",
      
      "<br><h3><b>How To Navigate the Tabs</b></h3>",
      "Educators in Puerto Rico can use this app to find helpful information for their students by using the following tabs:</br>",
      "✦ <b><i>Background Information:</i></b> Read about the purpose and use of the app </br>",
      "✦ <b><i>Tools:</i></b> Use behavioral indictor tools to identify potential behavioral disorders </br>",
      "✦ <b><i>Resources:</i></b> Find in-person facilities in Puerto Rico using a map and find online resources</br>",
      "✦ <b><i>Classroom Activities:</i></b> Find mindful activities to do in the classroom to help potential behavioral disorders</br>","",
      "✦ <b><i>Data Sources:</i></b> Find the open-source datasets used to create the app </br>", 
      
      "<br><h3><b>About the Data</b></h3>",
      "The data used in the creation of this app are all derived from federal open-source datasets, with the exception of
      geolocation data used to make the map of Puerto Rico. All sources are listed under the <i><b>Data Sources</b></i> tab.</br>",
      
      "<br><br><font size=1>American University</font></br>",
      "<font size=1>Tech Team: Lindsay Beyak, Jonathan Hague, Meera Patel</font></br>",
      "<font size=1>Team Leaders: Dr. Maria Barouti, Dr. Richard Ressler</font>",
      sep = ""
    ))
  })
  
  # Resources tab
  output$prtitle <- renderText({
    paste("<h3><b>In-Person Resource Locations in PR</b></h3>")
  })
  output$pronline <- renderText({
    paste("<h3><b>Online Resources</b></h3>")
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
      # addCircleMarkers(
      #     lng = -66.81581900259775,
      #     lat = 18.48760684076348,
      #     popup = ~tt1
      # ) %>%
      # addPopups(-66.81581900259775, 18.48760684076348, tt1,
      #           options = popupOptions())
      addCircleMarkers(
        data = mapdf,
        lng = ~Lng,
        lat = ~Lat,
        popup = ~Link,
        color = "#993366")
    # ) %>%
    # addPopups(
    #     data = mapdf,
    #     ~Lng, ~Lat, ~Link,
    #           options = popupOptions())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)