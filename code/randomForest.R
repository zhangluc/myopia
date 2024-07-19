install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("randomForest")
library(randomForest)

setwd("Desktop")
setwd("cosmosMyopiaProject")
data <- read.csv("myopia_og.csv", head = TRUE, sep = ";")
head(data)


# Fit the random forest model
# Assuming MYOPIC is the outcome variable and the rest are predictors
set.seed(42)  # Set a seed for reproducibility
rf_model <- randomForest(MYOPIC ~ ., data = data, ntree = 1000, importance = TRUE)

# Print the model summary
print(rf_model)

# View the importance of each variable
importance(rf_model)
varImpPlot(rf_model)



#Split the data into training and testing sets
set.seed(42)  # Set a seed for reproducibility
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Fit the random forest model on the training data
rf_model <- randomForest(MYOPIC ~ ., data = train_data, ntree = 1000, importance = TRUE)

# Make predictions on the test data
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model's performance
confusion_matrix <- table(predictions, test_data$MYOPIC)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

