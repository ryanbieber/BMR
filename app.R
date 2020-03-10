  # Define UI
  ui <- fluidPage(
    
    # Application title
    titlePanel("How long will it take you to lose weight in a safe manner?", windowTitle = "Weight Loss Calculator"),
    
    sidebarLayout(
      
      # Sidebar with a slider input
      sidebarPanel(
        selectInput(inputId = "Activity", "Choose an activity level:",
                    list("Sedentary (Work out 0 times/week)", "Light (Work out 1-3 times/week)", "Moderate (Work out 3-5 times/week)","Very (Work out 6-7 times/week)", "Extremely (Work out 7+ times/week)")
                    ,width = 1000)
        ,
        numericInput(inputId = "weight", "Weight:", 180, min = 0, max = 1000)
        ,
        numericInput(inputId = "bf", "Bodyfat %:", 15, min = 0, max = 100)
        ,
        numericInput(inputId = "goal", "Goal Weight:", 175, min = 0, max = 1000)
        ,
        numericInput(inputId = "cal", "Caloric deficit/surplus:", -350, min = -2000, max =2000)
        ,
        actionButton("goButton", "Pleast tell me!")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("weightloss"),
        plotOutput("BMR"),
        textOutput("time")
      )
    )
  )
  
  # Server logic
  server <- function(input, output) {
    observeEvent(
      eventExpr = input[["goButton"]],
      handlerExpr = {
    output$weightloss <- renderPlot({
      library(ggplot2)
      library(ggrepel)
      activity <- tolower(strsplit(input$Activity, " ")[[1]][1])
      weight <- input$weight
      bf <- input$bf
      deficit <- input$cal
      goal <- input$goal
      diff_weight <- weight-goal
      calories <- diff_weight*3500
      days <- calories/deficit
      
      ## activity level
      activity_level <- c("sedentary", "light", "moderate", "very", "extremely")
      activity_value <- c(1.2, 1.375, 1.55, 1.725, 1.9)
      how_active <- activity_value[match(activity, activity_level)]
      
      ##weight calculations
      LeanMassLBS <- weight-(weight*bf/100)
      goalLeanMassLBS <- LeanMassLBS - .25*(weight-goal)
      goalbf <- round(((goal-goalLeanMassLBS)/goal)*100, 1)
      weightlossday <- seq(weight*3500, goal*3500,deficit)/3500
      bodyfatloss <- seq(bf, goalbf, length.out = length(weightlossday))
      days <- seq.Date(Sys.Date(), by = 1, length.out = length(weightlossday))
      
      combined <- cbind.data.frame(days, weightlossday, bodyfatloss)
      combined_first_last <- rbind(combined[1,], combined[nrow(combined),])
      weightloss <- ggplot(combined, aes(days, weightlossday)) + 
        geom_line(size = 1)+
        geom_point(size = 2.5)+
        geom_label_repel(
          aes(x = days, y = weightlossday, label = round(weightlossday,0)),
          data=combined_first_last,
          size = 5,
          box.padding = 0.1, point.padding = 0.1,
          segment.color = 'grey50')
      # 
      weightloss <- weightloss + ggtitle("Weight loss/gain per day based on inputs", paste("Based on",deficit,"calorie deficit/surplus per-day", sep = " "))+
        labs(x = paste(days[1], "until", days[length(days)]), y = "Weight in lbs")
      print(weightloss)
    })
    output$BMR <- renderPlot({
      library(ggplot2)
      library(dplyr)
      library(ggrepel)
      activity <- tolower(strsplit(input$Activity, " ")[[1]][1])
      weight <- input$weight
      bf <- input$bf
      deficit <- input$cal
      goal <- input$goal
      diff_weight <- weight-goal
      calories <- diff_weight*3500
      days <- calories/deficit
      activity_level <- c("sedentary", "light", "moderate", "very", "extremely")
      activity_value <- c(1.2, 1.375, 1.55, 1.725, 1.9)
      
      ##weight calculations
      LeanMassLBS <- weight-(weight*bf/100)
      goalLeanMassLBS <- LeanMassLBS - .25*(weight-goal)
      goalbf <- round(((goal-goalLeanMassLBS)/goal)*100, 1)
      weightlossday <- seq(weight*3500, goal*3500,deficit)/3500
      bodyfatloss <- seq(bf, goalbf, length.out = length(weightlossday))
      days <- seq.Date(Sys.Date(), by = 1, length.out = length(weightlossday))
      
      how_active <- activity_value[match(activity, activity_level)]
      currentBMR <- (370 + (9.79759519 * LeanMassLBS))*how_active
      goalBMR <- (370 + (9.79759519 * goalLeanMassLBS))*how_active
      
      bmr_seq <- seq(LeanMassLBS, goalLeanMassLBS, length.out = length(days))
      sedentaryBMR <- (370 + (9.79759519 * bmr_seq))*1.2
      lightBMR <- (370 + (9.79759519 * bmr_seq))*1.375
      moderateBMR <- (370 + (9.79759519 * bmr_seq))*1.55
      veryBMR <- (370 + (9.79759519 * bmr_seq))*1.725
      extremelyBMR <- (370 + (9.79759519 * bmr_seq))*1.9
      
      
      combined <- cbind.data.frame(days, sedentaryBMR, lightBMR, moderateBMR, veryBMR, extremelyBMR)
      
      bmr_group <- reshape2::melt(combined, id.vars="days")
      
      bmr_group <- bmr_group %>%
        filter(variable==paste0(activity,"BMR"))
      bmr_first_last <- rbind(bmr_group[1,], bmr_group[nrow(bmr_group),])
      bmr <- ggplot(bmr_group, aes(days, value)) + 
        geom_line(size = 1)+
        geom_point(size = 2.5) +
        geom_label_repel(
          aes(x = days, y = value, label = round(value,0)),
          data=bmr_first_last,
          size = 5,
          box.padding = 0.1, point.padding = 0.1,
          segment.color = 'grey50')
      
      bmr <- bmr + ggtitle("Caloric expenditure based on activity level")+
        labs(x = paste(days[1], "until", days[length(days)]), y = "BMR in Calories")
      print(bmr)
      })
    if (input$cal<0){
      output$time <- renderText(paste("Losing", input$weight-input$goal , "lbs will take you approximetly", (( input$goal-input$weight)*3500)/input$cal, "days at your current caloric deficit.", sep = " "))
      
    } else {
      output$time <- renderText(paste("Gaining", input$weight-input$goal , "lbs will take you approximetly", ((input$goal-input$weight)*3500)/input$cal, "days at your current caloric deficit.", sep = " "))
      
       }
      }
    )
  }
  # Complete app with UI and server components
  shinyApp(ui, server)
