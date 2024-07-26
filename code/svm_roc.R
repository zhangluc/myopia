#svm ROC curve 

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

# Compute ROC curve
roc_svm <- roc(test_data$MYOPIC, probabilities[, 2])

# Plot ROC curve
plot.roc(roc_svm, col = "blue", main = "ROC Curve for SVM Model")

# Print AUC
text(0.5, 0.2, paste("AUC =", round(auc(roc_svm), 2)), col = "blue", cex = 1)
