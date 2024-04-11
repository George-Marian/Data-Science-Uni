# Banknotes data
# 1. variance of Wavelet Transformed image (continuous)
# 2. skewness of Wavelet Transformed image (continuous)
# 3. curtosis of Wavelet Transformed image (continuous)
# 4. entropy of image (continuous)
# 5. class (0 is genuine, 1 is forged)


# Set WD to the folder of the currently opened r file
if (rstudioapi::isAvailable())
  setwd(dirname(rstudioapi::getActiveDocumentContext()[["path"]]))

# Load libraries
library(data.table) # Data wrangling (Py_equivalent: pandas, polars, pydatatable)
library(plotly) # Beautiful plots
library(rpart) # Decision trees
library(rpart.plot)
library(caret) # ML tools http://topepo.github.io/caret/available-models.html
#library(mlr3) # ML tools (Py_equivalent: Scikit learn)

# Load the data ----
# dt <- fread("./data/Banknotes.txt")
# setnames(dt,
#          old = c("V1","V2","V3","V4","V5"),
#          new = c("Variance", "Skewness", "Curtosis", "Entropy", "Class"))
# dt[, Class := as.factor(Class)]
# str(dt)

# or combined all in one fread
dt <- fread("D:/IMC FH/MLAIBD4ILV - Machine Learning, Artificial Intelligence and Big Data Analytics/data/Banknotes.txt",
            col.names = c("Variance", "Skewness", "Curtosis", "Entropy", "Class"),
            colClasses = c("numeric","numeric","numeric","numeric","factor"))

summary(dt)
dt[, Class := as.factor(ifelse(Class == "0", "Genuine", "Forged"))]
str(dt)
# two ways of performing in-memory in-place operations:
# 1. set*
# 2. walruss operator (:=)


# Explore the data ----
colSums(is.na(dt))
# rowSums(is.na(dt))
# any(is.na(dt))

# Look at each variable and the class (Scatterplot)
a <- plot_ly(data = dt, x = 1:nrow(dt), y = ~Variance, color = ~Class,
        type = "scatter", mode = "markers", showlegend = F)
b <- plot_ly(data = dt, x = 1:nrow(dt), y = ~Skewness, color = ~Class,
        type = "scatter", mode = "markers", showlegend = F)
c <- plot_ly(data = dt, x = 1:nrow(dt), y = ~Curtosis, color = ~Class,
        type = "scatter", mode = "markers", showlegend = F)
d <- plot_ly(data = dt, x = 1:nrow(dt), y = ~Entropy, color = ~Class,
        type = "scatter", mode = "markers")
subplot(a,b,c,d, nrows = 2, titleY = T, margin = 0.05)

# better do this in a loop
plots <- list()
for (i in colnames(dt[, !"Class"])) {
  plots[[i]] <- plot_ly(data = dt, x = 1:nrow(dt), y = dt[,get(i)], color = dt[,Class],
                        type = "scatter", mode = "markers")
}
subplot(plots, nrows = 2, titleY = T, margin = 0.05)


#my_var |> myfunc()

# this is the same as
# layout(plotly_object, bla bla bla)
# this
# ploly_object |> layout(bla bla bla)

# let's fix the legend and add the axis title
plots <- list()
for (i in colnames(dt[, !"Class"])) {
  show_legend <- ifelse(i == "Variance", TRUE, FALSE)
  plots[[i]] <- plot_ly(data = dt, x = 1:nrow(dt), y = dt[,get(i)], color = dt[,Class],
                        type = "scatter", mode = "markers", showlegend = show_legend)  |>
    layout(yaxis = list(title = i))
}
subplot(plots, nrows = 2, titleY = T, margin = 0.05)

# %>% from tidy package
## |> in R, piped

# For those who already know a bit of R
# This could have been done also with lapply
#
# plots <- list()
# plots <- lapply(colnames(dt[, !"Class"]), function(i) {
#     show_legend <- ifelse(i == "Variance", TRUE, FALSE)
#     plot_ly(dt, x = 1:nrow(dt), y = ~get(i), color = ~Class,
#             type = "scatter", mode = "markers", showlegend = show_legend) |>
#     layout(yaxis = list(title = i))
#   }
# )
# subplot(plots, nrows = 2, titleY = T, margin = 0.05)

# Look at the boxplots
plots <- list()
for(i in colnames(dt[,!"Class"])){
  show_legend <- ifelse(i == "Variance", TRUE, FALSE)
  plots[[i]] <- plot_ly(data = dt, x = dt[,Class], y = dt[,get(i)], color = dt[,Class],
                        type = "box", showlegend = show_legend) |>
    layout(yaxis = list(title = i))
}
subplot(plots, nrows = 2, titleY = T, margin = 0.05)

# It's ordered, we should shuffle it first.
set.seed(12345)
dt <- dt[sample(1:nrow(dt))]

# Look if class is distinguishable from with the three most important variables
plot_ly(data = dt,
        x = ~Variance, y = ~Skewness, color = ~Class,
        type = "scatter", mode = "markers", marker = list(size = 4))
plot_ly(data = dt,
        x = ~Variance, y = ~Skewness, z = ~Curtosis, color = ~Class,
        type = "scatter3d", mode = "markers", marker = list(size = 4))

# Splom (aka scatter matrix, aka pair plot)
plot_ly(data = dt, type = 'splom', color = ~Class, marker = list(size = 4),
    dimensions = list(
      list(label='Variance', values=~Variance),
      list(label='Skewness', values=~Skewness),
      list(label='Curtosis', values=~Curtosis),
      list(label='Entropy', values=~Entropy)))

# Split training/test ----
set.seed(1)
#idx <- sample(1:nrow(dt), 0.8*nrow(dt))
idx <- createDataPartition(dt[,Class], p = 0.8, list = F, times = 1)
training <- dt[idx]
test <- dt[!idx]
#training <- dt[idx, !c("Entropy","Variance")]
#test <- dt[!idx, !c("Entropy","Variance")]

dt[, .(.N, share = .N/nrow(dt)), Class]
training[, .(n_obs = .N, share = .N / nrow(training)), Class]
test[, .(n_obs = .N, share = .N / nrow(test)), Class]

train_x <- training[,!"Class"]
train_y <- training[,Class]
test_x  <- test[,!"Class"]
test_y  <- test[,Class]


# Train a classification Tree ----
## OPTION 1: Stopping criterion ----
# Grow the tree until stopping criterion is met
# Default rpart parameters: minsplit = 20 and cp = 0.01
# IMPORTANT: rpart already includes 10-fold-validation internally
# and show the cross-validation error with the printcp() function

dtree_default <- rpart::rpart(Class ~ ., method = "class", data = training)
print(dtree_default)
printcp(dtree_default)
plotcp(dtree_default)
summary(dtree_default)
rpart.plot(dtree_default, type = 2, extra = 101, fallen.leaves = F, main = "Classification Tree for Banknotes", tweak=1.2)

## OPTION 2: Entire + pruning ----
# Grow the entire tree, until we see it overfitting and then "prune" it
# (the previous tree did not overfit until cp=0.01)
dtree_full <- rpart::rpart(Class ~ ., method = "class", data = training,
                           control = rpart.control(minsplit = 1, cp = 0))
print(dtree_full)
printcp(dtree_full)
plotcp(dtree_full)
summary(dtree_full)
rpart.plot(dtree_full, type = 2, extra = 101, fallen.leaves = F, tweak = 1.2, main = "Entire tree for Banknotes")

which.min(dtree_full$cptable[, "xerror"])

# And now we prune it at the optimal level of CP
best_cp_for_pruning <- dtree_full$cptable[which.min(dtree_full$cptable[, "xerror"]), "CP"]
dtree_pruned <- prune(dtree_full, cp = best_cp_for_pruning)

# This is our final tree
dtree_pruned
printcp(dtree_pruned)
rpart.plot(dtree_pruned, type = 2, extra = 101, fallen.leaves = F, tweak = 1.2, main = "Pruned tree for Banknotes")

# Predict ----
# Initial tree with default values
pred_y_dtree_default <- predict(dtree_default, newdata = test_x, type = "class")
pred_y_dtree_full <- predict(dtree_full, newdata = test_x, type = "class")
pred_y_dtree_pruned <- predict(dtree_pruned, newdata = test_x, type = "class")


table(pred_y_dtree_default,test_y)
confusionMatrix(pred_y_dtree_default, reference = test_y)
confusionMatrix(pred_y_dtree_default, reference = test_y, positive = "Forged")
confusionMatrix(pred_y_dtree_default, reference = test_y, positive = "Forged", mode = "prec_recall")
confusionMatrix(pred_y_dtree_full,test_y, positive = "Forged", mode = "prec_recall")
confusionMatrix(pred_y_dtree_pruned, test_y, positive = "Forged", mode = "prec_recall")

# Classification Tree with Caret----
# Important Caret aspects:
# trainControl
# tuneGrid
# tuneLength
# train(..., )
# Start
ctrl <- trainControl(method = "cv", number = 10)
# ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# ctrl <- trainControl(method="LOOCV")
dtree_caret <- train(Class ~ ., data = training, method = "rpart", trControl = ctrl, tuneLength = 10)
# available methods and parameters are at http://topepo.github.io/caret/available-models.html
pred_y_caret <- predict(dtree_caret, newdata = test_x)
table(pred_y_caret, test_y)
confusionMatrix(data = pred_y_caret, reference = test_y, positive = "Forged", mode = "prec_recall")
summary(dtree_caret)
View(dtree_caret)
rpart.plot(dtree_caret$finalModel)


