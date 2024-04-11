# SET WORKING DIRECTORY ----
if (rstudioapi::isAvailable())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# LOAD LIBRARIES ----
library(data.table)
library(plotly)
library(caret)

# IMPORT DATA ----
## Data.frame ----
pp <- read.csv("./data/PowerPlant2.csv")
## Data.table ----
pp <- fread("./data/PowerPlant2.csv")
# AT = Atmospheric Temperature
# V = Exhaust Vacuum
# AP = Ambient Pressure
# RH = Relative Humidity
# PE = Produced Energy


# DATA TABLE PRIMER ----
# datatable[ where, select, group by]
# dataframe[i,j]
pp[1,] # first row
pp[10:15,] # rows from 10 to 15
pp[AT > 10,] # where AT > 10 (where condition)
pp[, AT] # select column
pp[, list(AT)] # select column as data.table
pp[, .(AT)] # select column as data.table
pp[, 2] # select column as data.table
pp[AT > 10, .(AT,V,PE)] # select AT, V, and PE from pp where AT > 10
pp[, mean(AT)] # mean of a column
pp[, mean(AT), EnergyMode] # mean of a column grouped by another column
pp[AT > 10, mean(AT), EnergyMode] # select mean(AT) where AT > 10 group by EnergyMode
pp[, .(AT,V)][order(AT)] # Chaining dt[][][][]
pp[, .(.N), EnergyMode] # Special keywords .N, .I, .SD, .GRP,
mycol <- "AT"
pp[, ..mycol] # resolve (parse) variable from outside the data.table context
# := "walruss operator"
pp[, mynewvar := AT + 100] # create a new numeric variable (in-place operation)
pp[, mynewcatvar := as.factor(ifelse(AT < 20, "A", "B")) ] # create a new factor variable (in-place operation)
pp[, EnergyMode := ordered(EnergyMode, levels = c("L", "M", "H"))] # convert a character to an ordered factor (in-place operation)
pp[, AT_previous := shift(AT,1)] # create a new numeric variable as shifted version of another variable
pp[, mynewvar := NULL]
pp[, mynewcatvar := NULL]
pp[, AT_previous := NULL]

my_sum <- function(a,b) {
  return(a + b)
}

my_sum <- \(a,b) return(a + b) # identical to line 49:51

my_sum(3,5)
# EXPLORING THE DATASET (EDA) ----

pp
dim(pp)
str(pp)
summary(pp)
plot_ly(data = pp, y = ~AT, x = ~EnergyMode, type = "box")

plot_ly(pp, x = ~AT, y = ~V, type = "scatter", mode = "markers")
plot_ly(pp, x = ~AT, y = ~AP, type = "scatter", mode = "markers")
plot_ly(pp, x = ~AT, y = ~RH, z = ~V, color = ~EnergyMode, type = "scatter3d", mode = "markers")
plot_ly(pp, x = ~AT, y = ~RH, z = ~V, color = ~EnergyMode, type = "scatter3d", mode = "markers", marker = list(size = 3))
plot_ly(pp, x = ~AT, y = ~RH, z = ~V, color = ~PE, type = "scatter3d", mode = "markers", marker = list(size = 3))

distr <- pp[, .N, EnergyMode]
plot_ly(distr, x = ~EnergyMode, y = ~N, type = "bar")


# REGRESSION TASK ----
pp[, EnergyMode := NULL] # We don't need this for our regression task
pp <- pp[1:800] # Let's subset only the first 800 observation for the exercise

## Linear Regression ----
lin_regr <- lm(PE ~ . , data = pp)
View(lin_regr)
training.predictions <- lin_regr$fitted.values
training.actuals <- pp[,PE]
MAE(training.predictions, training.actuals) # MAE on training data for linear regression
RMSE(training.predictions, training.actuals) # RMSE on training data for linear regression

## Polynomial Regression ----
poly_regr <- lm(PE ~ poly(AT,V,AP,RH, degree=3), data = pp)
training.predictions <- poly_regr$fitted.values
training.actuals <- pp[,PE]
MAE(training.predictions, training.actuals) # MAE on training data for polynomial regression d=3
RMSE(training.predictions, training.actuals) # RMSE on training data for polynomial regression d=3

# Iterate over multiple degrees values (1 to 10)
poly_regr <- list()
MAEs <- vector()
#MAEs <- list()
for (d in 1:10) {
  poly_regr[[d]] <- lm(PE ~ poly(AT, V, AP, RH, degree = d), data = pp)
  training.predictions <- poly_regr[[d]]$fitted.values
  training.actuals <- pp[,PE]
  MAEs[d] <- MAE(training.predictions, training.actuals)
  #MAEs[[d]] <- MAE(training.predictions, training.actuals)
}
plot_ly(x = 1:10, y = MAEs, type = "scatter", mode = "line")


# Data Splitting ----
ppIndex <- createDataPartition(pp$PE, p = .7, list = F)
training <- pp[ppIndex]
test <- pp[-ppIndex]
# my_pred <- predict(model, newdata=test_without_output)
# MAE(my_pred, actual_values)

poly_regr <- list()
MAEs_training <- vector()
MAEs_test <- vector()
for (d in 1:6) {
  poly_regr[[d]] <- lm(PE ~ poly(AT,V,AP,RH, degree = d), data = training )
  training.predictions <- poly_regr[[d]]$fitted.values
  training.actuals <- training[,PE]
  MAEs_training[d] <- MAE(training.predictions, training.actuals)

  test.predictions <- predict(poly_regr[[d]], test[, !"PE"])
  test.actuals <- test[, PE]
  MAEs_test[d] <- MAE(test.predictions, test.actuals)
}
plot_ly(x = 1:length(MAEs_training),y = MAEs_training, type = "scatter", mode = "line") %>%
  add_lines(x = 1:length(MAEs_test), y = MAEs_test)
