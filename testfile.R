library(ggplot2)
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

weightlossday <- seq(weight*3500, goal*3500,-1*deficit)/3500
bodyfatloss <- seq(bf, goalbf, length.out = length(weightlossday))
days <- seq.Date(Sys.Date(), by = 1, length.out = length(weightlossday))

bmr_seq <- seq(LeanMassLBS, goalLeanMassLBS, length.out = length(days))

sedentaryBMR <- (370 + (9.79759519 * bmr_seq))*1.2
lightBMR <- (370 + (9.79759519 * bmr_seq))*1.375
moderateBMR <- (370 + (9.79759519 * bmr_seq))*1.55
veryBMR <- (370 + (9.79759519 * bmr_seq))*1.725
extremelyBMR <- (370 + (9.79759519 * bmr_seq))*1.9


combined <- cbind.data.frame(days, sedentaryBMR, lightBMR, moderateBMR, veryBMR, extremelyBMR)

bmr_group <- reshape2::melt(combined, id.vars="days")

bmr <- ggplot(bmr_group, aes(days, value, col=variable )) + 
  geom_line()
bmr + ggtitle("BMR(Basal Metabolic Rate) based on activity level")+
  labs(x = paste(days[1], "until", days[length(days)]), y = "BMR in Calories")

print(bmr)

weightloss + ggtitle("Weight loss per day based on inputs", paste("Based on",deficit,"calorie deficit per-day", sep = " "))+
  labs(x = paste(days[1], "until", days[length(days)]), y = "Weight in lbs")


