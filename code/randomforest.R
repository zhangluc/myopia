#initial exploration: random forest
library(ggplot2)
library(tidyverse)
library(cowplot) #improve ggplot's default settings
library(randomForest)

#read & clean data
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)
myopia <- myopia[myopia$AGE != 9, ]
myopia$ID <- NULL
myopia$STUDYYEAR <- NULL

#for missing vals
#set.seed(42)
#myopia.imputed <- rfImpute(myopic ~ ., data = myopia, iter = 6)
#myopic = var we want to predict
#iter = how many forests should be built to estimate missing vals (usually 4-6)

model <- randomForest(MYOPIC ~ ., data = myopia, proximity = TRUE)
model
#524 not myopic correctly classified as "not myopic"
#13 not myopic incorrectly classified as "myopic"
#57 myopic incorrectly classified as "not myopic"
#24 myopic correctly classified as "myopic"

#error rates at diff stages of RF
#1st row: error rates after making 1st tree
oob.error.myopia <- data.frame(
  trees = rep(1:nrow(model$err.rate), times = 3),
  type = as.factor(rep(c('OOB', '0', '1'), each = nrow(model$err.rate))),
  error = c(model$err.rate[,'OOB'],
  model$err.rate[,'0'],
  model$err.rate[,'1']))

#graph of error rates (0, 1, OOB)
ggplot(data = oob.error.myopia, 
       aes(x = trees, y = error)) +
  geom_line(aes(color = type,group = type))

#improving tree
model <- randomForest(MYOPIC ~ ., data = myopia, ntree = 2000, proximity = TRUE)
model

oob.values <- vector(length = 7)
for(i in 1:7) {
  temp.model <- randomForest(MYOPIC ~ ., data = myopia, mtry = i, ntree = 2000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate), 1]
}
oob.values #mtry = 4/5 seems to be most optimal

distance.matrix <- dist(1-model$proximity) #conver RF proximity matrix into distance matrix
mds.stuff <- cmdscale(distance.matrix, eig = TRUE, x.ret = TRUE) #find lower-dimensional rep of data
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1) #computes % of var

#format data for ggplot
mds.values <- mds.stuff$points
mds.data <- data.frame(sample = rownames(mds.values),
                       X = mds.values[,1],
                       Y = mds.values[,2],
                       status = myopia$MYOPIC)

#graph on ggplot
ggplot(data = mds.data, aes(x=X, y=Y, label=sample)) +
  geom_text(aes(color = status)) +
  theme_bw() +
  xlab(paste('MDS1 - ', mds.var.per[1], '%', sep = '')) +
  ylab(paste('MDS2 - ', mds.var.per[2], '%', sep = '')) +
  ggtitle("MDS plot using (1- Random Forest Proximities)")

#improved MDS plot
ggplot(data = mds.data, aes(x = X, y = Y, color = status, label = sample)) +
  geom_point(size = 3, alpha = 0.7) +  # Use points instead of text for clarity
  scale_color_manual(values = c('0' = 'blue', '1' = 'red')) +
  theme_minimal() +
  xlab(paste('MDS1 - ', mds.var.per[1], '% variance', sep = '')) +
  ylab(paste('MDS2 - ', mds.var.per[2], '% variance', sep = '')) +
  ggtitle("MDS Plot using (1 - Random Forest Proximities)") +
  theme(plot.title = element_text(hjust = 0.5))

#calculate feature importance
importance_scores <- importance(model)
importance_scores

par(cex = 0.7) #change font size
varImpPlot(model, main = "Feature Importance: Mean Decrease in Gini")
par(cex = 1)


#convert to data frame for ggplot
importance_df <- data.frame(Feature = rownames(importance_scores), 
                            Importance = importance_scores[, 1])

#plot feature importance
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  xlab('Features') +
  ylab('Importance') +
  ggtitle('Feature Importance in Random Forest Model') +
  theme_bw()

#ROC curve
install.packages("pROC")
library(pROC)

#use the model's OOB predictions
oob_predictions <- predict(model, type = "response", predict.all = TRUE)  # probabilities for the positive class (MYOPIC = 1)

#create the ROC curve
roc_curve <- roc(myopia$MYOPIC, oob_predictions)
auc_value <- auc(roc_curve)
print(auc_value)

#plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for Random Forest Model")

text(0.5, 0.2, paste("AUC =", round(auc(roc_curve), 2)), col = "blue", cex = 1)
