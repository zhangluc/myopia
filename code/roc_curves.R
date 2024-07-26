# Load necessary packages
library(e1071)
library(caret)
library(ggplot2)
library(kernlab)
library(pROC)

#load & clean data
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)
myopia <- myopia[myopia$AGE != 9, ]
myopia$ID <- NULL
myopia$STUDYYEAR <- NULL
myopia$MYOPIC <- as.factor(myopia$MYOPIC)

# Split the data into training (70%) and testing (30%) sets
set.seed(42)
trainIndex <- createDataPartition(myopia$MYOPIC, p = 0.7, list = FALSE)
train_data <- myopia[trainIndex, ]
test_data <- myopia[-trainIndex, ]

# Set up the parameter grid with correct column names
tune_grid <- expand.grid(
  C = 2^(-5:5),      # Range of values for C
  sigma = 2^(-15:3)  # Range of values for sigma (gamma)
)

# Set up train control
train_control <- trainControl(
  method = "cv",        # Cross-validation
  number = 5,           # Number of folds
  search = "grid",      # Grid search
)

# Train the model with parameter tuning
svm_tuned <- train(
  MYOPIC ~ SPHEQ + SPORTHR,
  data = train_data,
  method = "svmRadial",
  trControl = train_control,
  tuneGrid = tune_grid
)

# Extract the best model
best_svm_model_rbf <- svm_tuned$finalModel

# Function to plot the decision boundary with RBF kernel and support vectors
plot_svm_rbf_with_support_vectors <- function(model, myopia) {
  # Create a grid of values
  x_range <- seq(min(myopia$SPHEQ) - 1, max(myopia$SPHEQ) + 1, length.out = 100)
  y_range <- seq(min(myopia$SPORTHR) - 1, max(myopia$SPORTHR) + 1, length.out = 100)
  grid <- expand.grid(SPHEQ = x_range, SPORTHR = y_range)
  
  # Predict the class for each point in the grid
  grid$Pred <- predict(model, newdata = grid)
  
  # Extract support vectors
  support_vectors_indices <- model@SVindex
  support_vectors <- myopia[support_vectors_indices, ]
}

# Split the data into training (70%) and testing (30%) sets
set.seed(42)
trainIndex <- createDataPartition(myopia$MYOPIC, p = 0.7, list = FALSE)
train_data <- myopia[trainIndex, ]
test_data <- myopia[-trainIndex, ]

# Train the SVM model with probability enabled
svm_model <- svm(MYOPIC ~ SPHEQ + SPORTHR, 
                 data = train_data, 
                 type = "C-classification", 
                 kernel = "radial", 
                 probability = TRUE)

# Predict probabilities for test data
test_pred <- predict(svm_model, newdata = test_data, probability = TRUE)

# Extract probabilities
probabilities <- attr(test_pred, "probabilities")

#initial exploration: random forest
library(ggplot2)
library(tidyverse)
library(cowplot) #improve ggplot's default settings
library(randomForest)
library(pROC)

#load & clean data
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)
myopia <- myopia[myopia$AGE != 9, ]
myopia$ID <- NULL
myopia$STUDYYEAR <- NULL

#tree
model <- randomForest(MYOPIC ~ ., data = myopia, ntree = 2000, proximity = TRUE)

#error rates at diff stages of RF
oob.error.myopia <- data.frame(
  trees = rep(1:nrow(model$err.rate), times = 3),
  type = as.factor(rep(c('OOB', '0', '1'), each = nrow(model$err.rate))),
  error = c(model$err.rate[,'OOB'],
            model$err.rate[,'0'],
            model$err.rate[,'1']))

oob.values <- vector(length = 7)
for(i in 1:7) {
  temp.model <- randomForest(MYOPIC ~ ., data = myopia, mtry = i, ntree = 2000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate), 1]
}
oob.values #mtry = 4/5 seems to be most optimal

distance.matrix <- dist(1-model$proximity) #conver RF proximity matrix into distance matrix
mds.stuff <- cmdscale(distance.matrix, eig = TRUE, x.ret = TRUE) #find lower-dimensional rep of data
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1) #computes % of var

#ROC curve
#use the model's OOB predictions
oob_predictions <- predict(model, type = "response", predict.all = TRUE)  # probabilities for the positive class (MYOPIC = 1)

#ROC Curves Combined

library(xgboost)
library(tidyverse)
library(caret)
library(data.table)
library(pROC)
library(ggplot2)
library(dplyr)

#load & clean data
myopia <- read.csv("myopia_og.csv", head = TRUE, sep = ";")

myopia <- myopia %>% 
  mutate(MYOPIC = as.factor(MYOPIC))

myopia <- myopia %>% 
  filter(AGE != 9)

myopia$ID <- NULL
myopia$STUDYYEAR <- NULL


#XG BOOST
# Setting the seed for reproducibility
set.seed(0)

# Split into training and testing sets
my_trainIndex <- createDataPartition(myopia$MYOPIC, p = 0.8, list = FALSE)
trainData <- myopia[my_trainIndex, ]
testData <- myopia[-my_trainIndex, ]

# Convert data.table to data.frame to avoid data.table-specific issues
trainData <- as.data.frame(trainData)
testData <- as.data.frame(testData)

# Detect character columns and convert them to factors, then to numeric
char_cols <- sapply(trainData, is.character)
for (col in names(char_cols[char_cols])) {
  trainData[[col]] <- as.numeric(as.factor(trainData[[col]]))
  testData[[col]] <- as.numeric(as.factor(testData[[col]]))
}

# Ensure all columns are numeric. We use sapply to convert any residual non-numeric columns
trainData <- as.data.frame(sapply(trainData, as.numeric))
testData <- as.data.frame(sapply(testData, as.numeric))

# Separate features and target
trainLabel <- trainData$MYOPIC
testLabel <- testData$MYOPIC
trainData <- trainData[, setdiff(names(trainData), "MYOPIC")]
testData <- testData[, setdiff(names(testData), "MYOPIC")]

# Convert features to numeric matrices
train_feats <- as.matrix(trainData)
test_feats <- as.matrix(testData)

# Convert to XGBoost DMatrix format
dtrain <- xgb.DMatrix(data = train_feats, label = as.numeric(trainLabel) - 1)
dtest <- xgb.DMatrix(data = test_feats, label = as.numeric(testLabel) - 1)

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 6,  # Depth of the tree
  eta = 0.3,      # Learning rate
  nthread = 2
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,  # Number of boosting rounds
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10  # Early stopping if no improvement
)

# Predict on test data
pred <- predict(xgb_model, newdata = dtest)

#ROC CURVE 
roc_rf <- roc(myopia$MYOPIC, oob_predictions)
roc_xgb <- roc(testLabel, pred)
roc_svm <- roc(test_data$MYOPIC, probabilities[, 2])

# Color-blind-friendly colors
color_rf <- "#0072B2"   # Dark Blue
color_xgb <- "#E69F00"  # Orange
color_svm <- "#009E73"  # Dark Green

plot(roc_rf, col = color_rf, main = "ROC Curves for Random Forest, XGBoost, and SVM")
lines(roc_xgb, col = color_xgb)
lines(roc_svm, col = color_svm)

# Add a legend
legend("bottomright", legend = c("Random Forest", "XGBoost", "SVM"),
       col = c(color_rf, color_xgb, color_svm), lty = 1)

# Print AUC (change location of AUC)
text(0.5, 0.75, paste("AUC =", round(auc(roc_rf), 2)), col = color_rf, cex = 1)
text(1.1, 0.8, paste("AUC =", round(auc(roc_xgb), 2)), col = color_xgb, cex = 1)
text(0.7, 0.4, paste("AUC =", round(auc(roc_svm), 2)), col = color_svm, cex = 1)
