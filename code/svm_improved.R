# Load necessary packages
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

# Print the best parameters and model
print(svm_tuned)

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
  
  # Check if support_vectors is empty
  if (nrow(support_vectors) == 0) {
    stop("No support vectors found.")
  }
  
  # Plot the decision boundary and support vectors
  ggplot() +
    geom_tile(data = grid, aes(x = SPHEQ, y = SPORTHR, fill = Pred), alpha = 0.3) +
    geom_point(data = data, aes(x = SPHEQ, y = SPORTHR, color = MYOPIC), size = 2) +
    geom_point(data = support_vectors, aes(x = SPHEQ, y = SPORTHR), 
               color = 'black', shape = 17, size = 2, stroke = 1.5) +
    scale_fill_manual(values = c('lightblue', 'lightcoral'), guide = "none") +
    labs(title = "SVM with RBF Kernel Decision Boundary and Support Vectors",
         x = "SPHEQ",
         y = "SPORTHR") +
    theme_minimal()
}

# Plot using the training data
plot_svm_rbf_with_support_vectors(best_svm_model_rbf, train_data)

# Load necessary packages
library(kernlab)
library(pROC)

# Train the SVM model with ksvm
best_svm_model_rbf <- ksvm(MYOPIC ~ SPHEQ + SPORTHR, data = train_data, kernel = "rbfdot", C = best_C, sigma = best_sigma, prob.model = TRUE)

# Predict probabilities on the test data
test_probabilities <- predict(best_svm_model_rbf, newdata = test_data, type = "probabilities")[,2]
