
library(ggplot2)
activity <- tolower(strsplit(input$Activity, " ")[[1]][1])
weight <- 180
bf <- 15
goal <- 170
activity <- "light"
deficit <- 1000
diff_weight <- weight-goal
calories <- diff_weight*3500
days <- calories/deficit

## activity level
activity_level <- c("sedentary", "light", "moderate", "very", "extremely")
activity_value <- c(1.2, 1.375, 1.55, 1.725, 1.9)
how_active <- activity_value[match(activity, activity_level)]

##weight calculations
LeanMassLBS <- weight-(weight*bf/100)

## using the 25% rule for lean mass for simplicty
goalLeanMassLBS <- LeanMassLBS - .25*(weight-goal)
currentBMR <- (370 + (9.79759519 * LeanMassLBS))*how_active
goalBMR <- (370 + (9.79759519 * goalLeanMassLBS))*how_active
goalbf <- round(((goal-goalLeanMassLBS)/goal)*100, 1)




# sedentaryBMR <- (370 + (9.79759519 * goalLeanMassLBS))*1.2
# lightBMR <- (370 + (9.79759519 * goalLeanMassLBS))*1.375
# moderateBMR <- (370 + (9.79759519 * goalLeanMassLBS))*1.55
# veryBMR <- (370 + (9.79759519 * goalLeanMassLBS))*1.725
# extremelyBMR <- (370 + (9.79759519 * goalLeanMassLBS))*1.9



