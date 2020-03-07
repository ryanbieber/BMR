#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Activity", "Choose an activity level:",
                  list("Sedentary (Work out 0 times/week)", "Light (Work out 1-3 times/week)", "Moderate (Work out 3-5 times/week)","Very (Work out 6-7 times/week)", "Extremely (Work out 7+ times/week)")
      ,width = 1000),
      numericInput("weight", "Weight:", 180, min = 0, max = 1000),
      verbatimTextOutput("value")
      ,
      numericInput("bf", "Bodyfat %:", 15, min = 0, max = 100),
      verbatimTextOutput("value")
      ,
      numericInput("goal", "Goal Weight:", 175, min = 0, max = 1000),
      verbatimTextOutput("value")
    ), mainPanel(
      
    )    )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(ggplot2)
  activity <- tolower(strsplit(input$Activity, " ")[[1]][1])
  weight <- input$weight
  bf <- input$bf

  
  ## activity level
  activity_level <- c("sedentary", "light", "moderate", "very", "extremely")
  activity_value <- c(1.2, 1.375, 1.55, 1.725, 1.9)
  how_active <- activity_value[match(activity, activity_level)]
  
  ##weight calculations
  LeanMassLBS <- weight-(weight*bf/100)
  currentBMR <- (370 + (9.79759519 * LeanMassLBS))*how_active
  
  sedentaryBMR <- (370 + (9.79759519 * LeanMassLBS))*1.2
  lightBMR <- (370 + (9.79759519 * LeanMassLBS))*1.375
  moderateBMR <- (370 + (9.79759519 * LeanMassLBS))*1.55
  veryBMR <- (370 + (9.79759519 * LeanMassLBS))*1.725
  extremelyBMR <- (370 + (9.79759519 * LeanMassLBS))*1.9
  
  
  output$BMR <- renderPlot({
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

