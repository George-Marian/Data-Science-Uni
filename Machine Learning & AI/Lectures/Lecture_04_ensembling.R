# Set WD to the folder of the currently opened r file
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Load libraries ---------------------------------------------------------------
# Create our function "using()" 
using <- \(pkg) {
  # if a package is not installed, install it
  if (!rlang::is_installed(pkg)) {
    install.packages(pkg)
  }
  # load the package
  library(pkg, character.only = TRUE)
}

using("data.table") # The data manipulation king
using("caret") # The ML swiss-knife - http://topepo.github.io/caret/available-models.html
using("plotly") # Beatiful interactive plots
using("ranger") # the fastest and better random forest implementation
using("adabag") # Adaboost
using("xgboost") # Extreme Gradient boosting
using("imbalance") # Oversampling
using("ROSE") # Synthetic generation of new data to rebalance
using("VIM") # imputation of missing values
using("mltools") # one-hot-encoding
# Import Data ------------------------------------------------------------------

data <- fread("D:/IMC FH/MLAIBD4ILV - Machine Learning, Artificial Intelligence and Big Data Analytics/data/healthcare-dataset-stroke-data.csv")
# data[, gender := as.factor(gender)]
summary(data)
str(data)
table(data$gender)
View(lapply(data, table))

# We need to change some variable types
# We could write a colClasses = named_vector changing only a subset of columns.
# Do not do this if you are developing code that must go on production
# In general it is a bad idea to rely on automatic type inference for prod code.

# he setted factor, numeric, int, manually!!!! 

data <- fread("D:/IMC FH/MLAIBD4ILV - Machine Learning, Artificial Intelligence and Big Data Analytics/data/healthcare-dataset-stroke-data.csv",
              colClasses = c("integer", "factor", "numeric",
                             rep("factor", 5),
                             rep("numeric", 2),
                             rep("factor", 2)),
              na.strings = c("N/A","Unknown"))
data[, stroke := as.factor(ifelse(stroke == "1", "Yes", "No"))]
data[, ever_married := as.factor(ifelse(ever_married == "Yes", 1, 0))]

#Changing the level names could have been done also like this:
#setattr(data$stroke, "levels", c("No","Yes"))
#setattr(data$ever_married, "levels", c("0","1"))

data
summary(data)
str(data)

# EDA --------------------------------------------------------------------------
my_numerical_columns <- colnames(data[, .SD, .SDcols = is.numeric])
my_categorical_columns <- colnames(data[, .SD, .SDcols = is.factor])

scatterplots <- lapply(my_numerical_columns,
                       function(col) {
                         plot_ly(data   = data,
                                 x      = ~1:nrow(data),
                                 y      = ~get(col),
                                 color  = ~stroke,
                                 colors = c("Green","Blue"),
                                 type   = "scattergl",
                                 mode   = "markers",
                                 showlegend = FALSE,
                                 marker = list(size = 3)) |>
                           layout(yaxis = list(title = col))
                       })
subplot(scatterplots, titleY = T, nrows = 2)

boxplots <- lapply(my_numerical_columns,
                       function(col) {
                         plot_ly(data   = data,
                                 x      = ~stroke,
                                 y      = ~get(col),
                                 color  = ~stroke,
                                 colors = c("Green","Blue"),
                                 type   = "box",
                                 showlegend = FALSE) |>
                           layout(yaxis = list(title = col))
                       })
subplot(boxplots, titleY = T, nrows = 2)

densityplots <- lapply(my_numerical_columns,
                  function(col) {
                    show_legend <- ifelse(col == "id", TRUE, FALSE)
                    kdensity_yes <- density(data[stroke=="Yes",get(col)], na.rm = T)
                    kdensity_no <- density(data[stroke=="No",get(col)], na.rm = T)
                    plot_ly(type = "scatter", mode = "lines", fill = "tozeroy", showlegend = show_legend) |>
                      add_trace(x=kdensity_yes$x, y=kdensity_yes$y, name = "Stroke", color = I("orange")) |>
                      add_trace(x=kdensity_no$x, y=kdensity_no$y, name = "No Stroke", color = I("darkgreen")) |>
                      layout(yaxis = list(title = col))
                  })
subplot(densityplots, titleY = T, nrows = 2)

# We should look also at the categorical variables
barplots <- lapply(my_categorical_columns[my_categorical_columns!="stroke"], 
                   function(col) {
                     show_legend <- ifelse(col == "gender", TRUE, FALSE)
                     counters <- data[, .(stroke = nrow(.SD[stroke == "Yes"]) / .N,
                                          no_stroke = nrow(.SD[stroke == "No"]) / .N),
                                      col]
                     plot_ly(data   = counters, type   = "bar", showlegend = show_legend) |>
                       add_trace(x = ~get(col), y = ~no_stroke, name = "No Stroke", color = I("orange")) |>
                       add_trace(x = ~get(col), y = ~stroke, name = "Stroke", color = I("darkgreen")) |>
                       layout(yaxis = list(title = col), barmode = "stack") 
                   })
subplot(barplots, titleY = T, nrows = 2, margin = 0.05)


# Preprocessing ----------------------------------------------------------------
# It's ordered. Let's shuffle it just in case
set.seed(123)
data<-data[sample(1:nrow(data))]

## Dealing with NAs ----
sum(is.na(data))
colSums(is.na(data))
#data[,lapply(.SD, function(x) sum(is.na(x)))]

### Option 1: Omit ----
# Can observations (rows) with NAs be dropped? No, losing so much data is a pity.
nrow(na.omit(data))/nrow(data)
# Can features (columns) with NAs be dropped?
# I would not do it because smoking could be quite a predictor for stroke.
plot_ly(data, y = ~bmi, color = ~stroke, type = "box")
data[,.(perc_stroke=sum(stroke=="Yes")/.N*100),smoking_status]

### Option 2: Manual imputation ----
# For example
# A. Substitute with the median (Easiest, suggested only if the NAs are random):
#setnafill(data, type = "const", fill = median(data[,bmi], na.rm = T), cols = "bmi")
data[is.na(bmi), bmi := median(data[,bmi], na.rm = T)]

# B. Substitute with the median considering some grouping
data[, median_bmi := median(bmi, na.rm = T), .(hypertension, ever_married)]
data[is.na(bmi), bmi := median_bmi]
data[,median_bmi:=NULL]

### Option 3. Model-based imputation ----
# For example, kNN with VIM
data.imputed<-VIM::kNN(data, variable = c("bmi", "smoking_status"), imp_var = F)
VIM::aggr(data)
?aggr
VIM::matrixplot(data)

?matrixplot
# ATTENTION WE SHOULD DO THIS AFTER DATA SPLITTING
# i.e. We don't use data.imputed, we rather apply imputation on the training subset

# Train/Test -------------------------------------------------------------------
data[,id:=NULL]
set.seed(123)

idx <- createDataPartition(data[, stroke], p = .8, list = F, times = 1)
training <- data[idx]
test <- data[!idx]

training<-VIM::kNN(training, variable = c("bmi", "smoking_status"), imp_var = F)

test <- na.omit(test)

training_x <- training[,!"stroke"]
training_y <- training[,stroke]
test_x <- test[,!"stroke"]
test_y <- test[,stroke]
training[,.N,stroke]
test[,.N,stroke]
training[,.N/nrow(training),stroke]
test[,.N/nrow(test),stroke]

# Ranger -----------------------------------------------------------------------
## Raw data ----
# we can try without oversampling
# fit.ranger <- ranger(stroke ~ ., data = training) # default parameters
rf.orig <- ranger(x = training_x, y = training_y) # default parameters
# Results on training set
confusionMatrix(data = rf.orig$predictions, reference = training_y,
                                 positive = "Yes", mode = "prec_recall")
# results on test set
my_pred <- predict(object = rf.orig, data = test_x)
confusionMatrix(data = my_pred$predictions, reference = test_y, positive = "Yes", mode = "prec_recall")

## Rebalancing ----------------------------------------------------------------
# Oversampling or undersampling
#training.rose <- ovun.sample(stroke ~ ., data = training,
#                              method = "over", seed = 123, p = 0.4)$data
# Synthetic generation
training.rose <- ROSE(stroke ~ ., data = training,
                      seed = 123, p = 0.5)$data
setDT(training.rose)
training[,.N,stroke]
training.rose[,.N,stroke]
training.rose_x <- training.rose[,!"stroke"]
training.rose_y <- training.rose[,stroke]

# Training
rf.rebalanced <- ranger(x = training.rose_x, y= training.rose_y) # default parameters

# results on training data
confusionMatrix(data = rf.rebalanced$predictions, reference = training.rose_y,
                positive = "Yes", mode = "prec_recall")
# results on test data
my_pred_reb <- predict(rf.rebalanced, data = test_x)
confusionMatrix(data = my_pred_reb$predictions, reference = test_y,
                positive = "Yes", mode = "prec_recall")

## Feature Importance ----
# Let's create a random feature
# We expect that other features are much more important than the random one
training.rose[,random:=runif(nrow(training.rose), 1, 100)]
fit.ranger <- ranger(stroke ~ ., data = training.rose, importance = "permutation")
View(fit.ranger)
imp <- importance(fit.ranger)
imp <- data.table(Feature = names(imp), importance = imp)
plot_ly(data=imp, x=~Feature, y=~importance, type="bar") %>%
  layout(xaxis = list(categoryorder="total descending"))
# Confirmed. We can remove the random feature
training.rose[, random :=NULL]
# If we would have seen that some features are much less important than
# others we could have dropped them as well.


# adaboost ---------------------------------------------------------------------
ada <- adabag::boosting(stroke ~ ., data = training.rose, mfinal = 200)
my_pred_adaboost <- predict(ada, newdata = test_x)
confusionMatrix(as.factor(my_pred_adaboost$class), test_y, mode = "prec_recall", positive = "Yes")

# xgboost ----------------------------------------------------------------------
# transform to matrix
# xgboost requires numeric input!
# I'll transform:
# work_type,smoking_status -> one-hot-encoding
# gender,hypertension,heart_disease,ever_married,Residence_type -> numeric
# I use mltools, we could have used also caret::dummyVars or base::model.matrix

library(mltools)
xgbtraining <- one_hot(training.rose, cols = c("work_type","smoking_status"))
xgbtraining[, ':='(
  gender = as.integer(ifelse(gender == "Male", 1, 0)),
  hypertension = as.integer(as.character(hypertension)),
  heart_disease = as.integer(as.character(heart_disease)),
  ever_married = as.integer(as.character(ever_married)),
  Residence_type = as.integer(ifelse(gender == "Urban", 1, 0))
)]
# gender has become "isMale" and Residence has become a "isUrban".
# Let's replace their name
setnames(xgbtraining,c("gender","Residence_type"),c("isMale","isUrban"))
# xgboost requires the input to be a matrix of input features (not a data.fame)
xgbtraining_x <- as.matrix(xgbtraining[,!"stroke"])
# and the label must be a separated numeric vector (xgboost is a regression technique)
xgbtraining_y <- as.integer(ifelse(xgbtraining[,stroke]=="Yes",1,0))

# Finally the training
xgb <- xgboost::xgboost(data = xgbtraining_x,
                        label = xgbtraining_y,
                        nrounds=3000, objective = "binary:logistic")
# This is a super naive training! xgboost has plenty of parameters
# and a lot of tuning options. For the full list (and tutorials) look at:
# https://xgboost.readthedocs.io/en/latest/R-package/index.html


# Evaluate on test set
# Let's do the same transformations on the test set
xgbtest <- one_hot(test,cols = c("work_type","smoking_status"))
xgbtest[, ':='(
  gender = as.integer(ifelse(gender=="Male",1,0)),
  hypertension = as.integer(as.character(hypertension)),
  heart_disease = as.integer(as.character(heart_disease)),
  ever_married = as.integer(as.character(ever_married)),
  Residence_type = as.integer(ifelse(gender=="Urban",1,0))
)]
setnames(xgbtest,c("gender","Residence_type"),c("isMale","isUrban"))
xgbtest_x <- as.matrix(xgbtest[,!"stroke"])
xgbtest_y <- as.integer(ifelse(test[,stroke]=="Yes",1,0))

# Performance on the test set
mypred <- predict(xgb,newdata = xgbtest_x)
# Remember: xgboost makes regression.
# We assign a label of one if mypred>0.5
my_pred_xgboost <- as.integer(mypred>0.5)
confusionMatrix(as.factor(my_pred_xgboost),as.factor(xgbtest_y), mode="prec_recall", positive = "1")


# Random Forest Tuning ----
# Test with different number of trees
# We create a dataframe that contains different number of trees.
# We add OOB and F1 (for the moment =0) that we will fill later on
hyper_grid <- data.frame(
  n_trees = seq(25, 1000, by = 25), #c(seq(1,20,1),25,50,100,250,500,1000,1500,2000),
  OOB = 0,
  F1 = 0
)

for (i in 1:nrow(hyper_grid)){
  model <- ranger(x = training.rose_x, y = training.rose_y, num.trees = hyper_grid$n_trees[i])
  hyper_grid$OOB[i] <-  sqrt(model$prediction.error)
  conf <- confusionMatrix(data      = model$predictions,
                          reference = training.rose_y,
                          mode      = "prec_recall",
                          positive  = "Yes")
  hyper_grid$F1[i] <- conf$byClass["F1"]
}
plot_ly(data=hyper_grid, x=~n_trees, y=~OOB, type = "scatter", mode = "line")
plot_ly(data=hyper_grid, x=~n_trees, y=~F1, type = "scatter", mode = "line")
# In this run we see that the F1 score stabilizes at ca. 300-400 trees.
# We should use this amount as minimum!

# hyperparameter grid search
# Now we try with other parameters (mtry and node_size)
hyper_grid <- expand.grid(
  n_trees      = c(100,200,500),
  mtry         = 1:5,
  node_size    = 1:5,
  OOB_RMSE     = 0,
  F1           = 0)

for(i in 1:nrow(hyper_grid)) {
  # train model
  model <- ranger(
    x               = training.rose_x,
    y               = training.rose_y,
    num.trees       = hyper_grid$n_trees[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    seed            = 123
  )
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
  conf <- confusionMatrix(data      = model$predictions,
                          reference = training.rose_y,
                          mode      = "prec_recall",
                          positive  = "Yes")
  hyper_grid$F1[i] <- conf$byClass["F1"]
}
View(hyper_grid)

# ntree of 300 and mtry of 3 provide the best results
# node_size has no impact in performance
rf.tuned <- ranger(x = training.rose_x, y = training.rose_y, mtry = 3, num.trees = 200, min.node.size = 2)
# Performance on training set
confusionMatrix(data = rf.tuned$predictions, reference = training.rose_y, mode = "prec_recall", positive = "Yes")
# Performance on test set
my_pred_tuned <- predict(rf.tuned, test_x)
confusionMatrix(my_pred_tuned$predictions, test_y, positive = "Yes", mode = "prec_recall")
confusionMatrix(my_pred_reb$predictions, test_y, positive = "Yes", mode = "prec_recall")
confusionMatrix(my_pred$predictions, test_y, positive = "Yes", mode = "prec_recall")

# Not bad. We increase performance on test set by few percents


# let's compare performance of all models
data.frame(
  "RF_original" = confusionMatrix(my_pred$predictions, test_y, positive = "Yes", mode = "prec_recall")$byClass,
  "RF_Rebalanced" = confusionMatrix(my_pred_reb$predictions, test_y, positive = "Yes", mode = "prec_recall")$byClass,
  "RF_Reb/Tuned" = confusionMatrix(my_pred_tuned$predictions, test_y, positive = "Yes", mode = "prec_recall")$byClass,
  "Adaboost" = confusionMatrix(as.factor(my_pred_adaboost$class), test_y, mode = "prec_recall", positive = "Yes")$byClass,
#  "Adaboost_500" = confusionMatrix(my_pred_adaboost_500$class, test_y, mode = "prec_recall", positive = "Yes")$byClass,
  "XGBoost" = confusionMatrix(as.factor(my_pred_xgboost), as.factor(xgbtest_y), mode="prec_recall", positive = "1")$byClass
)

# EXCERCISE: REPEAT THIS EXERCISE WITH CARET WITH ITS HYPERPARAMETER TUNING FACILITIES

