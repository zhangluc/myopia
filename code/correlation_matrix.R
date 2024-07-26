#initial exploration: CORRELATION MATRIX

# Load necessary libraries
library(ggplot2)
library(reshape2)

# Load & Clean data
data <- read.csv('myopia_og.csv', sep=";", header=TRUE)

data$MYOPIC <- as.numeric(data$MYOPIC)

data <- subset(data, AGE != 9)

# Select only numeric columns for correlation analysis
numeric_data <- data[sapply(data, is.numeric)]

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Melt the correlation matrix to a long format for ggplot2
correlation_melted <- melt(correlation_matrix)

# Plot the heatmap
ggplot(data = correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  ggtitle("Correlation Matrix Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))
