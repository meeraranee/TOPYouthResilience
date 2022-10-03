# Youth Resilience in PR Shiny App

# Import libraries
library(shiny)
library(shinythemes)
library(dplyr)

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
            tabPanel("Classroom Activities", value = 2
                     ),
        
    
    # Tab 3
            tabPanel("Resources", value = 3
                     ),
    
    # Tab 4
            tabPanel("Data Sources", value = 4
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
}

# Run the application 
shinyApp(ui = ui, server = server)