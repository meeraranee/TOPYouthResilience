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
library(rsconnect)

# PR map
pr <- rgdal::readOGR("https://raw.githubusercontent.com/commonwealth-of-puerto-rico/crime-spotter/master/public/data/municipalities.geojson")
mapdf <- read_csv("Data/mapdf1.csv")
# Business - Viz : hc, p, 
nsdata_adhd_puf_u <- read_sas("Data/nsdata_adhd_puf_u.sas7bdat")
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
    text = "The First Person Concerned With The Child’s Behavior, Attention, or Performance Before ADHD Diagnosis:",
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
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 10, color="black", fill="lightblue") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Child's Age") +
  ylab("% of Total Population Surveyed") +
  ggtitle("Children's Age When Their Behavior Was First Concerning:") +
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
ui <- fluidPage(theme = shinytheme("yeti"),
                
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
                            tabPanel("Visualizations", value = 3,
                                     tabsetPanel(
                                       tabPanel("ADHD",
                                          tabsetPanel(
                                                tabPanel(title = "An Understanding",
                                                            img(id= "homeing", src="https://inspirecommunityoutreach.ca/wp-content/uploads/2022/02/inspire-adhd.jpg", style = "width: 70%; height= 70% ; padding: 0;"),
                                                            column(10,
                                                                   class="homeing",
                                                                   h2("What do the numbers say?"),
                                                                   br(),
                                                                   h4(" In this section we are looking at data from the CDC's National Survey of the Diagnosis and Treatment of ADHD and Tourette Syndrome (NS-DATA) to derive some insightful visuals to help you better understand ADHD in children in the US."),
                                                                   br(),
                                                                   HTML("<p>If you would like to check the survey data, please go to <a href='https://www.cdc.gov/nchs/slaits/ns_data.htm'> this CDC link</a>!</p>"),
                                                                   br(),
                                                                   h4("Presented charts reflect responses from the survey data about children aged 2 to 15 years old in 2011-2012 who had ever been diagnosed with attention-deficit/hyperactivity disorder (ADHD)."),
                                                                   br(),
                                                                   h4("The survey is based on a national sampling of around 3,000 respondent parent of caregiver of a child with ADHD."),
                                                                   br()
                                                              )),
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
                                                        ),
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
                                                tabPanel("Medication Status",
                                                         fluidPage(
                                                           mainPanel(
                                                             highchartOutput("plotgraph3")
                                                           )
                                                         )
                                                      ),
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
                                                tabPanel("Impact",
                                                         fluidPage(
                                                           mainPanel(
                                                             highchartOutput("plotgraph4")
                                                           )
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
                                     ),
                                     tabPanel("Trauma",
                                       tabsetPanel(
                                         tabPanel(title = "An Understanding",
                                                  img(id= "homeing", src="http://nestcac.org/wp-content/uploads/2019/01/xSad-Child-Dealing-with-Trauma.jpg.pagespeed.ic.kJTAVHxYea.jpg", style = "width: 70%; height= 70% ; padding: 0;"),
                                                  column(10,
                                                         class="homeing",
                                                         h2("What is child trauma?"),
                                                         br(),
                                                         h4("According to The National Child Traumatic Stress Network (NCTSN), trauma occurs when a child experiences an intense event that threatens or causes harm to his or her emotional and physical well-being. Child abuse is the leading cause of child trauma."),
                                                         br(),
                                                         HTML("<p>If you would like to check NCTSN work, please <a href='https://www.nctsn.org/what-is-child-trauma/trauma-types'> click here</a>!</p>"),
                                                         br(),
                                                         h4("3 million reports are made to the US Child Protection Services (USCPS) each year involving 5.5 million children. There is proof of abuse in about 30% of those cases each year."),
                                                         br(),
                                                         HTML("<p>Presented chart under Abuse tab reflects how often different types of child abuse occur in USCPS with proof of abuse documented cases. The chart under Victimization presents the outcome of The National Survey of Children's Exposure to Violence that reports on 1 year and lifetime prevalence of childhood victimization in a nationally representative sample of 4549 minors from 2014. The report can be accessed <a href='https://www.ojp.gov/pdffiles1/ojjdp/grants/248444.pdf'> on this link</a>!</p>"),
                                                         br()
                                                  )),
                                         tabPanel("Abuse",
                                                  fluidPage(
                                                    mainPanel(
                                                      highchartOutput("plotgraph5", height="500px")
                                                      )
                                                  )
                                                ),
                                         tabPanel("Victimization",
                                                  fluidPage(
                                                    mainPanel(
                                                      highchartOutput("plotgraph6", height="500px")
                                                    )
                                                  )
                                         )
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
                  conditionalPanel(condition = "input.tabselected==2",
                                   htmlOutput("explanation"),
                                   selectInput("UserInput",  # This is the name of the variable the User Input will be saved to
                                               "1.	In the last two weeks, have you witnessed the following child’s behavior?", #This is what will be displayed for the user
                                               choices = c("","Fearful", "Difficulty sustaining attention", "Restless","None of these")), # the preset choices
                                   h4(textOutput("Result")),
                                   htmlOutput("questions")# h4 is text size again; here will be the Output send to 
                  ),
                  # conditionalPanel(condition = "input.tabselected==3"
                  # ),
                  conditionalPanel(condition = "input.tabselected==4",
                                   htmlOutput("prtitle"),
                                   leafletOutput("prmap"),
                                   htmlOutput("prcaveat"),
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
    includeHTML("Text/backgroundinfo.html")
  })
  
  # Survey Tab
  output$explanation <- renderText({
    paste("<h5>Below is a sample survey that clinicians can commonly use to 
    assist in identifying behaviors associated with ADHD or PTSD. There may also
    be cases where children are experiencing an overlap of both conditions. The 
    survey is designed to provide insight into which treatment is most appropriate
    for their diagnosis.</h5></br>", "",
          
          "<h5>Understanding these behaviours can help professionals address them in a 
          way that would benefit the child and enhance their learning. Further 
          resources are available in the links below to gain a deeper understanding
          of each of these conditions.</h5></br>", "",
          "<h6>*Please note the results identified in this survey are not a diagnosis
          and a professional consultation is encouraged.*</h6>", "")#h2 comes form html style and simply is determining basically the text size")
  })
  
  output$questions <- renderText({
    paste("<h5><a href='https://www.health.vic.gov.au/practice-and-service-quality/trauma-and-abuse-asking-questions'>Trauma-Informed Care Questions</a></br></h5>",
          "<h5><a href='https://www.healthline.com/health/adhd#symptoms'>Understanding ADHD</a></br></h5></br>")
  })
  
  Identifier =function(q.c){  # Your function (could be also set outside of shiny body)
    if (q.c == "Fearful"){
      QuizResult="Traumatic Experience"
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
    includeHTML("Text/mindful.html")
  })
  
  #Viz tab
  output$plotgraph1 <- renderHighchart({
    hc <- df %>%
      hchart(
        "pie", hcaes(x = name, y = y),
        name = "Responded") %>%
      hc_title(
        text = "The First Person to Notice the Child's ADHD Before Official Diagnosis:",
        margin = 20,
        align = "left",
        style = list(color = "black", useHTML = TRUE))
    
    hchart(diamonds$carat)
    hc
  })
  
  output$plotgraph2 <- renderPlot({p})
  
  output$plotgraph3 <- renderHighchart({
    df2 <- data.frame(
      x1 = c(1, 2, 3, 4),
      y1 = c(1752, 549, 189, 5),
      name = as.factor(c("Taking ADHD Med", "Not Taking ADHD Med", 
                         "Never Taken ADHD Med", "Med Unknown"))
    )
    
    hc2 <- df2 %>%
      hchart(
        "pie", hcaes(x = name, y = y1),
        name = "Responded") %>%
      hc_title(
        text = "Medication Status of Children Currently with ADHD:",
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
                         "Excellent", "Refused to Answer"))
    )
    
    hc3 <- df3 %>%
      hchart(
        "pie", hcaes(x = name, y = y2),
        name = "Responded") %>%
      hc_title(
        text = "Academic Performance of Children Diagnosed with ADHD:",
        margin = 20,
        align = "left",
        style = list(color = "black", useHTML = TRUE))
    hc3
  })
  
  output$plotgraph5 <- renderHighchart({
    df5 <- data.frame(
      x2 = c(1, 2, 3, 4),
      y2 = c(65, 18, 10, 7),
      name = as.factor(c("Neglect", "Physical Abuse", 
                         "Sexual Abuse", "Psychological (mental) Abuse"))
    )
    
    hc5 <- df5 %>%
      hchart(
        "pie", hcaes(x = name, y = y2),
        name = "Percentage") %>%
      hc_title(
        text = "Type of Child Abuse in USCPS Reports With Documented Proof:",
        margin = 20,
        align = "left",
        style = list(color = "black", useHTML = TRUE))
    hc5
  })
  
  output$plotgraph6 <- renderHighchart({
    df6 <- data.frame(
      x2 = c(1, 2),
      y2 = c(60.6, 39.4),
      name = as.factor(c("Yes", "No"))
    )
    
    hc6 <- df6 %>%
      hchart(
        "pie", hcaes(x = name, y = y2),
        name = "Percentage") %>%
      hc_title(
        text = "Asked if experienced or witnessed victimization in the past year, a nationally representative sample of minors answered:",
        margin = 20,
        align = "left",
        style = list(color = "black", useHTML = TRUE))
    hc6
  })
  
  # Resources tab
  output$prtitle <- renderText({
    paste("<h3><b>In-Person Resource Locations in PR</b></h3>")
  })
  output$pronline <- renderUI({
    includeHTML("Text/onlineresources.html")
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
  output$prcaveat <- renderText({
    paste("*These facilities also offer Telehealth help.*")
  })
  
  # Data Sources Tab
  output$datasources <- renderText({
    includeHTML("Text/datasources.html")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)