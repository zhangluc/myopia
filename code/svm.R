# load necessary packages
library(e1071)
library(caret)
library(ggplot2)

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

# Train the SVM model with RBF kernel
svm_model_rbf <- svm(MYOPIC ~ SPHEQ + SPORTHR, data = train_data, type = 'C-classification', kernel = 'radial')

# Function to plot the decision boundary with RBF kernel and support vectors
plot_svm_rbf_with_support_vectors <- function(model, data) {
  # Create a grid of values
  x_range <- seq(min(data$SPHEQ) - 1, max(data$SPHEQ) + 1, length.out = 100)
  y_range <- seq(min(data$SPORTHR) - 1, max(data$SPORTHR) + 1, length.out = 100)
  grid <- expand.grid(SPHEQ = x_range, SPORTHR = y_range)
  
  # Predict the class for each point in the grid
  grid$Pred <- predict(model, grid)
  
  # Extract support vectors
  support_vectors <- data[model$index, ]
  
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
plot_svm_rbf_with_support_vectors(svm_model_rbf, train_data)