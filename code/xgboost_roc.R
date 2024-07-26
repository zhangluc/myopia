#xgboost ROC curve

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
roc_boost <- roc(testLabel, pred)

plot(roc_boost, main = "ROC Curve for XGBoost Model", col = "green", lwd = 2)

# Add AUC (Area Under the Curve) to the plot
text(0.75, 0.2, paste("AUC =", round(auc(roc_boost), 2)), col = "green", cex = 1)
