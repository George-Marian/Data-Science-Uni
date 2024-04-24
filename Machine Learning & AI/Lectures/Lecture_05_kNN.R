# SETUP ------------------------------------------------------------------------
# Set WD to the folder of the currently opened r file
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
# Load libraries
library(data.table)
library(plotly)

# K-Nearest Neighbors ----------------------------------------------------------
library(caret) # http://topepo.github.io/caret/available-models.html
library(class) # Very common kNN implementation

## Iris data -------------------------------------------------------------------
data <- iris
setDT(data)
#setnames(data, "Species", "Class")

summary(data)

# All measures are in cm. In principle we do not need to rescale
# to_rescale <- colnames(data[,.SD,.SDcol=is.numeric])
# data[, (to_rescale) := lapply(.SD,scale), .SDcol = to_rescale]

### Train/Test ----
data <- data[sample(1:nrow(data))]
set.seed(123)
idx <- createDataPartition(y = data[,Species], p = .7, list = F, times = 1)
training_x <- data[idx, !"Species"]
training_y <- data[idx, Species]
test_x     <- data[!idx, !"Species"]
test_y     <- data[!idx, Species]

### Class library ----
# We need to choose a value of K. For start randomly. Only for educational purposes
?knn
test_pred <- knn(train = training_x,
                 cl    = training_y,
                 test  = test_x,
                 k     = 45)
confusionMatrix(data = test_pred, reference = test_y)
# 0.9. Not bad. Can we do better?
# Let's try to tune K by using cross-validation (LOOCV)

# Let's see with CV
training_pred <- knn.cv(train = training_x,
                        cl    = training_y,
                        k     = 45)
confusionMatrix(data = training_pred, reference = training_y)
# Setosa is easy to classify
# Versicolor and Virginica are sometime so close that they cannot be distinguished

# let's try different values of K
training_pred <- list()
Kselection <- seq(1, 55, 2)
for (i in Kselection) {
  training_pred[[as.character(i)]] <- knn.cv(train = training_x, cl = training_y, k = i)
}
# or with *apply. More efficient, harder syntax
# knn <- sapply(as.character(Kselection),
#               function(x) knn.cv(train = training_x, cl = training_y, k = as.numeric(x)),
#               USE.NAMES = T,
#               simplify = F)

my_conf_matrix <- confusionMatrix(training_pred[["3"]], training_y)
View(my_conf_matrix)

accuracies <- sapply(training_pred,
                     \(x) confusionMatrix(data = x, reference = training_y)$overall["Accuracy"])
plot_ly(x = Kselection, y = accuracies, type = "scatter", mode = "line")

# Let's pick the K that gives us the best accuracy with LOOCV
# let's check the confusion matrix
confusionMatrix(data = training_pred[["13"]], reference = training_y)

# Let's test it on the test data
test_pred <- knn(train = training_x,
                 cl    = training_y,
                 test  = test_x,
                 k     = 13)
confusionMatrix(data = test_pred, reference = test_y)


### Different Distance measures----
# class::knn is the most common but it supports only euclidean
# some packages support multiple measures
# some packages allow to provide a distance matrix instead of data frame
# kNN is so easy that implementing it from scratch is not so hard (<100 lines)



## Seed data -------------------------------------------------------------------
data <- fread("./Lecture_06_Seed_Data.csv")
data[, Class := as.factor(Class)]
levels(data$Class)<-c("Kama", "Rosa", "Canadian")

# We should rescale
# Should we do it now? NO! First split in train an test

### Train/Test ----
set.seed(0)
idx <- createDataPartition(data[, Class], p = .8, list = F, times = 1)
training_x <- data[idx, !"Class"]
training_y <- data[idx, Class]
test_x <- data[!idx, !"Class"]
test_y <- data[!idx, Class]

training_x_rescaling <- scale(training_x)
centering <- attr(training_x_rescaling, "scaled:center")
scaling <- attr(training_x_rescaling, "scaled:scale")
training_x_rescaled <- as.data.table(training_x_rescaling)

test_x_rescaled <- as.data.table(scale(test_x, center = centering, scale = scaling))

### Class library ----
# We need to choose a value of K.
# Only one problem. The test set is quite small.
# Let's start with cross-validation (LOOCV)

# let's try different values of K (with LOOCV)
training_pred <- list()
Kselection <- seq(1, 85, 2)
for (i in Kselection) {
  training_pred[[as.character(i)]] <- knn.cv(train = training_x_rescaled,
                                             cl    = training_y,
                                             k     = i)
}
accuracies <- sapply(training_pred, function(x) confusionMatrix(data = x, reference = training_y)$overall["Accuracy"])
plot_ly(x = Kselection, y = accuracies, type = "scatter", mode = "line")

# Let's pick the K that gives us the best accuracy with LOOCV
# let's check the confusion matrix
confusionMatrix(data = training_pred[["19"]], reference = training_y)

test_pred <- knn(train = training_x_rescaled,
                 cl    = training_y,
                 test  = test_x_rescaled,
                 k     = 19)
confusionMatrix(data = test_pred, reference = test_y)

### CARET ----
#TODO


## Breast Cancer data ----------------------------------------------------------
data <- fread("./Lecture_06_BreastCancerCoimbra.csv")
data[, Class := factor(ifelse(Class == 1, "Healty", "Cancer"))]

### Train/Test ----
set.seed(111)
idx <- createDataPartition(data[,Class], p = .8, list = F, times = 1)
training_x <- data[idx, !"Class"]
training_y <- data[idx, Class]
test_x     <- data[!idx, !"Class"]
test_y     <- data[!idx, Class]

# to_rescale <- colnames(data[, .SD, .SDcol = is.numeric])
# ?scale
# training_x[, (to_rescale) := lapply(.SD,scale), .SDcol = to_rescale]
# test_x[, (to_rescale) := lapply(.SD,scale), .SDcol = to_rescale]

training_x_rescaling <- scale(training_x)
centering <- attr(training_x_rescaling, "scaled:center")
scaling <- attr(training_x_rescaling, "scaled:scale")
training_x_rescaled <- as.data.table(training_x_rescaling)
#training_x_rescaled <- training_x

test_x_rescaled <- as.data.table(scale(test_x, center = centering, scale = scaling))
#test_x_rescaled <- test_x


### Class library ----
# Now we know the trick. Let's start with finding the correct K
training_pred <- list()
Kselection <- seq(1, 85, 2)
for(i in Kselection){
  training_pred[[as.character(i)]] <- knn.cv(train = training_x_rescaled,
                                             cl    = training_y,
                                             k     = i)
}
accuracies <- sapply(training_pred, function(x) confusionMatrix(data = x, reference = training_y, positive = "Cancer")$overall["Accuracy"])
F1_score <- sapply(training_pred, function(x) confusionMatrix(data = x, reference = training_y, positive = "Cancer", mode = "prec_recall")$byClass[["F1"]])

plot_ly(x = Kselection, y = accuracies,
        type = "scatter", mode = "line", name = "Accuracy") |>
  add_lines(y = F1_score, name = "F1 Score")

# Let's pick the K that gives us the best accuracy with LOOCV
# let's check the confusion matrix on training data with the best K
confusionMatrix(data = training_pred[["9"]], reference = training_y, positive = "Cancer")

# Let's now check how it perform on the test data
test_pred <- knn(train = training_x_rescaled,
                 test  = test_x_rescaled,
                 cl    = training_y,
                 k     = 5)
confusionMatrix(data = test_pred, reference = test_y, mode = "prec_recall")

### CARET ----
### Caret ----
?caret::train
?caret::trainControl
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 3)
getModelInfo("knn")

fit.caret <- train(x = training_x_rescaled, y = training_y,
                   method = "knn", trControl = ctrl, tuneLength = 40)
fit.caret
plot(fit.caret)
confusionMatrix.train(fit.caret, norm = "none")
my_pred <- predict(fit.caret, newdata = test_x)
confusionMatrix(data = my_pred, reference = test_y)



# Distances (the proxy package) ------------------------------------------------
library(proxy)
proxy::pr_DB$get_entries()

vecs <- matrix(c(0,1,1,0,1,1,1,0,0,
                 0,0,1,1,1,1,1,0,1),
               byrow = T, nrow = 2)

dist(vecs, method = "jaccard", diag=T)
dist(vecs, method = "cosine", diag=T)
dist(vecs, method = "manhattan", diag=T)
dist(vecs, method = "euclidean", diag=T)

simil(vecs, method = "jaccard", diag=T)
simil(vecs, method = "cosine", diag=T)

# Example from the lecture
vecs <- matrix(c(1, 2,
                 3, 5,
                 2, 0,
                 4, 5),
               byrow = T, nrow = 4)
dist(vecs, method = "manhattan", diag=T)
dist(vecs, method = "euclidean", diag=T)
dist(vecs, method = "Chebyshev", diag=T)

# vecs <- matrix(c(0,1,1,0,1,1,1,0,0,
#                  0,0,1,1,1,1,1,0,1,
#                  0,0,1,1,1,0,1,0,1,
#                  0,0,1,1,0,1,1,0,1,
#                  0,0,1,1,1,1,0,0,1), byrow= T, nrow = 5)
