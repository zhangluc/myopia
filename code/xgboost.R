#gradient boosting model with XGBoost
library(xgboost)
library(tree)
library(caret) #calculate MSE
library(Matrix)
library(ggplot2)

myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)

#convert target variable to numeric (have to do this for model to work)
myopia$MYOPIC <- as.numeric(myopia$MYOPIC)

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

#set parameters for xgboost
params <- list(
  booster = "gbtree",
  objective = "binary:logistic",  # for binary classification
  eta = 0.3,  # learning rate
  max_depth = 6,
  eval_metric = "error"
)

#train the model
nrounds <- 100
xgb_model <- xgb.train(
  params = params,
  data = train_matrix,
  nrounds = nrounds,
  watchlist = list(train = train_matrix, test = test_matrix),
  verbose = 1
)
