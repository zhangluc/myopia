# Load necessary packages
install.packages("kernlab")
install.packages("pROC")
library(e1071)
library(caret)
library(ggplot2)
library(kernlab)
library(pROC)

# Load your dataset
data <- read.csv("myopia_og.csv", head = TRUE, sep = ";")
data$SPHEQ <- as.numeric(data$SPHEQ)
data$SPORTHR <- as.numeric(data$SPORTHR)
data$MYOPIC <- as.factor(data$MYOPIC)

# Split the data into training (70%) and testing (30%) sets
set.seed(42)
trainIndex <- createDataPartition(data$MYOPIC, p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

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
plot_svm_rbf_with_support_vectors <- function(model, data) {
  # Create a grid of values
  x_range <- seq(min(data$SPHEQ) - 1, max(data$SPHEQ) + 1, length.out = 100)
  y_range <- seq(min(data$SPORTHR) - 1, max(data$SPORTHR) + 1, length.out = 100)
  grid <- expand.grid(SPHEQ = x_range, SPORTHR = y_range)
  
  # Predict the class for each point in the grid
  grid$Pred <- predict(model, newdata = grid)
  
  # Extract support vectors
  support_vectors_indices <- model@SVindex
  support_vectors <- data[support_vectors_indices, ]
}

# Load your dataset
data <- read.csv("myopia_og.csv", header = TRUE, sep = ";")
data$SPHEQ <- as.numeric(data$SPHEQ)
data$SPORTHR <- as.numeric(data$SPORTHR)
data$MYOPIC <- as.factor(data$MYOPIC)

# Split the data into training (70%) and testing (30%) sets
set.seed(42)
trainIndex <- createDataPartition(data$MYOPIC, p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

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
roc_curve <- roc(test_data$MYOPIC, probabilities[, 2])

# Plot ROC curve
plot.roc(roc_curve, col = "blue", main = "ROC Curve for SVM Model")

# Print AUC
auc_value <- auc(roc_curve)
print(auc_value)