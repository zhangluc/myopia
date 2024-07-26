# Load necessary libraries
library(xgboost)
library(caret)

# Load data
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)

#convert target variable to numeric (have to do this for model to work)
#myopia$MYOPIC <- as.numeric(myopia$MYOPIC)

#split data into training & test sets
set.seed(123)
trainIndex <- createDataPartition(myopia$MYOPIC, p = .8, 
                                  list = FALSE, 
                                  times = 1)
myopia.train <- myopia[trainIndex,]
myopia.test  <- myopia[-trainIndex,]

# convert data to matrix format (optimized for memory efficiency/computational speed)
train_matrix <- xgb.DMatrix(data = data.matrix(myopia.train[, -5]), label = myopia.train$MYOPIC)
test_matrix <- xgb.DMatrix(data = data.matrix(myopia.test[, -5]), label = myopia.test$MYOPIC)

#define model parameters
params <- list(
  objective = "binary:logistic", #logistic regression for binary classification
  eval_metric = "logloss",
  booster = "gbtree" #gradient boosting
)

#train the model with all features
set.seed(42)
model_all <- xgb.train(params = params, 
                       data = train_matrix, 
                       nrounds = 100, 
                       watchlist = list(train = train_matrix, eval = test_matrix),
                       verbose = 1)

#evaluate performance
y_pred_all_prob <- predict(model_all, test_matrix)
y_pred_all <- ifelse(y_pred_all_prob > 0.5, 1, 0)

#calculate accuracy
accuracy_all <- sum(y_pred_all == as.numeric(y_test) - 1) / length(y_test)
print(paste("Accuracy with all features:", round(accuracy_all * 100, 2), "%"))

#confusion matrix
conf_matrix_all <- confusionMatrix(factor(y_pred_all, levels = c(0, 1)), factor(as.numeric(y_test) - 1, levels = c(0, 1)))
print(conf_matrix_all)
