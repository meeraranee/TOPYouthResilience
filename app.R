# Youth Resilience in PR Shiny App

# Import libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(geojsonR)
library(haven)
library(RColorBrewer)
library(plotrix)
library(highcharter) 
library(hrbrthemes)

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

# Business - Viz : hc, p, 
nsdata_adhd_puf_u <- read_sas("./EDA/nsdata_adhd_puf_u.sas7bdat")
Concered <- table(nsdata_adhd_puf_u['ADHD_A1_4'])
Concered <- as.data.frame(Concered)
Concered$Var1 <- recode_factor(Concered$Var1, 
                               "1" = "Family",
                               "2" = "School",
                               "3" = "Healthcare", 
                               "4" = "Else",
                               "6" = "DontKnow",
                               "7" = "Refused")

df <- data.frame(
  x = c(0, 1, 2, 3, 4, 5),
  y = c(1869, 917, 64, 93, 22, 1),
  name = as.factor(c("Family", "School", "Healthcare",
                     "Else", "DontKnow", "RefusedAnswer"))
)

hc <- df %>%
  hchart(
    "pie", hcaes(x = name, y = y),
    name = "Responded") %>%
  hc_title(
    text = "The First Person Concerned With The Child’s Behavior, Attention, or Performance Before ADHD Diagnosis.",
    margin = 20,
    align = "left",
    style = list(color = "black", useHTML = TRUE))


Age_First_Concerned <- as.data.frame(nsdata_adhd_puf_u$ADHD_A1_4_AGE_STD) #2,966 input
Age_First_Concerned %>% 
  rename(Age_Concerned = 'nsdata_adhd_puf_u$ADHD_A1_4_AGE_STD') %>%
  filter(Age_Concerned != 96) %>% 
  filter(Age_Concerned != 97) -> Age_First_Concerned #2,942 input

p <- Age_First_Concerned %>%
  filter(Age_Concerned > 0) %>%
  ggplot( aes(x=Age_Concerned)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  xlab("Child's Age") +
  ggtitle("Child's Age When The First Person Concerned With Their Behavior Was First Concerned") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15), 
        text=element_text(color="black"), 
        axis.text=element_text(color="black")) 


df2 <- data.frame(
  x1 = c(1, 2, 3, 4, 5, 6),
  y1 = c(1752, 549, 189, 5, 383, 88),
  name = as.factor(c("Has ADHD & Taking ADHD Med", "Has ADHD & Not Taking ADHD Med", 
                     "Has ADHD & Never Taken ADHD Med", "Has ADHD & Med Unknown",
                     "Does Not Currently Have ADHD", "Current ADHD Status Unknown"))
)

hc2 <- df2 %>%
  hchart(
    "pie", hcaes(x = name, y = y1),
    name = "Responded") %>%
  hc_title(
    text = " ADHD Condition and Medication Status",
    margin = 20,
    align = "left",
    style = list(color = "black", useHTML = TRUE))
hc2


df3 <- data.frame(
  x2 = c(1, 2, 3, 4, 5, 6),
  y2 = c(633, 640, 853, 514, 247, 79),
  name = as.factor(c("Problematic", "Somewhat Problematic", 
                     "Average", "Above Average",
                     "Excellent", "Don't Know or Refused"))
)

hc3 <- df3 %>%
  hchart(
    "pie", hcaes(x = name, y = y2),
    name = "Responded") %>%
  hc_title(
    text = " ADHD and School Performance",
    margin = 20,
    align = "left",
    style = list(color = "black", useHTML = TRUE))
hc3

# Build app
ui <- fluidPage(theme = shinytheme("cerulean"),
                
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
                            tabPanel("Visualization", value = 3,
                                     fluidPage(
                                               tabsetPanel(
                                                tabPanel(title = "Why, What, and Who?",
                                                            img(id= "homeing", src="https://inspirecommunityoutreach.ca/wp-content/uploads/2022/02/inspire-adhd.jpg", style = "width: 70%; height= 70% ; padding: 0;"),
                                                            column(10,
                                                                   class="homeing",
                                                                   h2("What do the numbers say?"),
                                                                   br(),
                                                                   h4(" In this section we are looking at data from the CDC's National Survey of the Diagnosis and Treatment of ADHD and Tourette Syndrome (NS-DATA) to derive some insigntful visuals to help you better understand ADHD in children in the US."),
                                                                   br(),
                                                                   h4("Presented charts reflect responses from the survey data about children aged 2 to 15 years old in 2011-2012 who had ever been diagnosed with attention-deficit/hyperactivity disorder (ADHD)."),
                                                                   br(),
                                                                   h4("The survey is based on a national sampling of around 3,000 respondent parent of caregiver of a child with ADHD."),
                                                                   br()
                                                              )
                                                            ),
                                                tabPanel("Diagnosis",
                                                          fluidPage(
                                                              mainPanel(
                                                                highchartOutput("plotgraph1", height="500px")
                                                                #splitLayout(cellWidths = c("50%", "50%"), highchartOutput("plotgraph1", height="500px"), plotOutput("plotgraph2"))
                                                                #splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2"))
                                                                        #column(8, plotOutput("plotgraph1")),
                                                                        #column(8, plotOutput("plotgraph2"))
                                                                      )
                                                            #plotOutput("plotgraph1"), plotOutput("plotgraph2")
                                                                #splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2"))
                                                            )
                                                        ),
                                                tabPanel("Age",
                                                         fluidPage(
                                                           mainPanel(
                                                             plotOutput("plotgraph2")
                                                            )
                                                           )
                                                        #  sidebarLayout(
                                                        #    sidebarPanel(
                                                        #      selectInput("state_in", "State?", choices = unique(nsdata_adhd_puf_u$ADHD_A1_4), selected = "1", multiple = TRUE),
                                                        #      checkboxInput("facetcounty", "Facet by County?"),
                                                        #      varSelectInput("EDAx", "Variable", data = nsdata_adhd_puf_u, selected = "ADHD_A1_4"),
                                                        #      numericInput("nullVal", "Please Enter a Positive Integer", value = 0)
                                                        #    ),
                                                        #    mainPanel(
                                                        #      plotOutput("distPlot"),
                                                        #      verbatimTextOutput("t_test"),
                                                        #      verbatimTextOutput("noNull"))
                                                        #    )
                                                        ),
                                                tabPanel("Medication Status",
                                                         fluidPage(
                                                           mainPanel(
                                                             highchartOutput("plotgraph3")
                                                           )
                                                         )
                                                          #sidebarLayout(
                                                          #  sidebarPanel(
                                                          #    selectInput("state_in", "State?", choices = unique(nsdata_adhd_puf_u$ADHD_A1_4), selected = "1", multiple = TRUE),
                                                          #    checkboxInput("facetcounty", "Facet by County?"),
                                                          #    varSelectInput("EDAx", "Variable", data = nsdata_adhd_puf_u, selected = "ADHD_A1_4"),
                                                          #    numericInput("nullVal", "Please Enter a Positive Integer", value = 0)
                                                          #  ),
                                                          #  mainPanel(
                                                          #    plotOutput("distPlot"),
                                                          #    verbatimTextOutput("t_test"),
                                                          #    verbatimTextOutput("noNull"))
                                                          #)
                                                        ),
                                                tabPanel("Impact",
                                                         fluidPage(
                                                           mainPanel(
                                                             highchartOutput("plotgraph4")
                                                           )
                                                         )
                                                          #sidebarLayout(
                                                          #  sidebarPanel(
                                                          #    selectInput("state_in", "State?", choices = unique(nsdata_adhd_puf_u$ADHD_A1_4), selected = "1", multiple = TRUE),
                                                          #    checkboxInput("facetcounty", "Facet by County?"),
                                                          #    varSelectInput("EDAx", "Variable", data = nsdata_adhd_puf_u, selected = "ADHD_A1_4"),
                                                          #    numericInput("nullVal", "Please Enter a Positive Integer", value = 0)
                                                          #  ),
                                                          #  mainPanel(
                                                          #    plotOutput("distPlot"),
                                                          #    verbatimTextOutput("t_test"),
                                                          #    verbatimTextOutput("noNull"))
                                                          #)
                                                        #)
                                                    #)
                                               )
                                     )
                                     )
                                     ),
                            
                            # Tab 4
                            tabPanel("Resources", value = 4
                            ),
                            
                            # Tab 5
                            tabPanel("Classroom Activities", value = 5
                            ),
                            
                            # Tab 6
                            tabPanel("Data Sources", value = 6
                            )
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
                  )
                  # conditionalPanel(condition = "input.tabselected==5"
                  # ),
                )
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
      "✦ <b><i>Survey:</i></b> Use behavioral indictor survey to identify potential behavioral disorders </br>",
      "✦ <b><i>Visualizations:</i></b> Use visualization tool for potential behavioral disorders </br>",
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
  
  #Viz tab
  output$plotgraph1 <- renderHighchart({
    hc <- df %>%
      hchart(
        "pie", hcaes(x = name, y = y),
        name = "Responded") %>%
      hc_title(
        text = "The First Person Concerned With The Child’s Behavior, Attention, or Performance Before ADHD Diagnosis.",
        margin = 20,
        align = "left",
        style = list(color = "black", useHTML = TRUE))
    
    hchart(diamonds$carat)
    hc
  })
  
  output$plotgraph2 <- renderPlot({p})
  
  output$plotgraph3 <- renderHighchart({
    df2 <- data.frame(
      x1 = c(1, 2, 3, 4, 5, 6),
      y1 = c(1752, 549, 189, 5, 383, 88),
      name = as.factor(c("Has ADHD & Taking ADHD Med", "Has ADHD & Not Taking ADHD Med", 
                         "Has ADHD & Never Taken ADHD Med", "Has ADHD & Med Unknown",
                         "Does Not Currently Have ADHD", "Current ADHD Status Unknown"))
    )
    
    hc2 <- df2 %>%
      hchart(
        "pie", hcaes(x = name, y = y1),
        name = "Responded") %>%
      hc_title(
        text = "ADHD Condition and Medication Status",
        margin = 20,
        align = "left",
        style = list(color = "black", useHTML = TRUE))
    hc2
  })
  
  output$plotgraph4 <- renderHighchart({
    df3 <- data.frame(
      x2 = c(1, 2, 3, 4, 5, 6),
      y2 = c(633, 640, 853, 514, 247, 79),
      name = as.factor(c("Problematic", "Somewhat Problematic", 
                         "Average", "Above Average",
                         "Excellent", "Don't Know or Refused"))
    )
    
    hc3 <- df3 %>%
      hchart(
        "pie", hcaes(x = name, y = y2),
        name = "Responded") %>%
      hc_title(
        text = " ADHD and School Performance",
        margin = 20,
        align = "left",
        style = list(color = "black", useHTML = TRUE))
    hc3
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