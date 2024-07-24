#SVM- classifying any new element into one of the two classes
#Line of best separation
#Support Vectors: two points, one from each class that are close


# Install and load necessary packages
install.packages("e1071")
library(e1071)
install.packages("caret")
library(caret)

# Load your dataset
data <- read.csv("myopia_og.csv", head = TRUE, sep = ";")
head(data)
# Prepare the data
data$SPHEQ <- as.numeric(data$SPHEQ)
data$SPORTHR <- as.numeric(data$SPORTHR)
data$MYOPIC <- as.factor(data$MYOPIC)

# Split the data into training (70%) and testing (30%) sets
set.seed(42)
trainIndex <- createDataPartition(data$MYOPIC, p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Train the SVM model
svm_model <- svm(MYOPIC ~ SPHEQ + SPORTHR, data = train_data, type = 'C-classification', kernel = 'linear')

# Make predictions on the test data
predictions <- predict(svm_model, test_data)

# Evaluate the model's performance
confusion_matrix <- table(Predicted = predictions, Actual = test_data$MYOPIC)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))


#TRY DIFFERENT KERNELS

