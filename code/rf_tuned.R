#random forest tuned
# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(caret)
library(cowplot)
library(randomForest)
library(pROC)
library(smotefamily)

# Read & clean data
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)
myopia$MYOPIC <- as.factor(myopia$MYOPIC)
myopia <- myopia[myopia$AGE != 9, ]
myopia$ID <- NULL
myopia$STUDYYEAR <- NULL

# Apply SMOTE
smote_output <- SMOTE(X = myopia[, -which(names(myopia) == "MYOPIC")],
                      target = myopia$MYOPIC, 
                      K = 5, 
                      dup_size = 1)

# Combine the resulting data
myopia_balanced <- smote_output$data
names(myopia_balanced)[names(myopia_balanced) == "class"] <- "MYOPIC"

# Check the class distribution
print(table(myopia_balanced$MYOPIC))

# Define the control for cross-validation
control <- trainControl(method = "cv", number = 5, search = "grid")

# Define the grid of hyperparameters to search
tunegrid <- expand.grid(.mtry = c(1:7))

# Perform the grid search with the balanced data
set.seed(42)
rf_gridsearch <- train(MYOPIC ~ ., data = myopia_balanced, 
                       method = "rf", 
                       metric = "Accuracy", 
                       tuneGrid = tunegrid, 
                       trControl = control)

# Print the best model
print(rf_gridsearch)

# Train the model using the best parameters
best_params <- rf_gridsearch$bestTune
best_ntree <- ifelse(!is.null(best_params$.ntree), best_params$.ntree, 2000)
best_nodesize <- ifelse(!is.null(best_params$.nodesize), best_params$.nodesize, 1)
best_maxnodes <- ifelse(!is.null(best_params$.maxnodes), best_params$.maxnodes, NULL)

set.seed(42)
myopia_balanced$MYOPIC <- as.factor(myopia_balanced$MYOPIC)
model <- randomForest(MYOPIC ~ ., data = myopia_balanced, ntree = 2000, proximity = TRUE)
model

# Evaluate the model
oob_predictions <- predict(model, type = "prob")[, 2] # Probabilities for the positive class

# Create the ROC curve
roc_curve <- roc(myopia_balanced$MYOPIC, as.numeric(oob_predictions))
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for Random Forest Model")
text(0.5, 0.2, paste("AUC =", round(auc(roc_curve), 2)), col = "blue", cex = 1)
