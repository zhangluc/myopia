#random forest ROC curve 

#initial exploration: random forest
library(ggplot2)
library(tidyverse)
library(cowplot) #improve ggplot's default settings
library(randomForest)
library(pROC)

#load & clean data
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)
myopia <- myopia[myopia$AGE != 9, ]
myopia$ID <- NULL
myopia$STUDYYEAR <- NULL

#tree
model <- randomForest(MYOPIC ~ ., data = myopia, ntree = 2000, proximity = TRUE)

#error rates at diff stages of RF
oob.error.myopia <- data.frame(
  trees = rep(1:nrow(model$err.rate), times = 3),
  type = as.factor(rep(c('OOB', '0', '1'), each = nrow(model$err.rate))),
  error = c(model$err.rate[,'OOB'],
            model$err.rate[,'0'],
            model$err.rate[,'1']))

oob.values <- vector(length = 7)
for(i in 1:7) {
  temp.model <- randomForest(MYOPIC ~ ., data = myopia, mtry = i, ntree = 2000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate), 1]
}
oob.values #mtry = 4/5 seems to be most optimal

distance.matrix <- dist(1-model$proximity) #conver RF proximity matrix into distance matrix
mds.stuff <- cmdscale(distance.matrix, eig = TRUE, x.ret = TRUE) #find lower-dimensional rep of data
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1) #computes % of var

#ROC curve
#use the model's OOB predictions
oob_predictions <- predict(model, type = "response", predict.all = TRUE)  # probabilities for the positive class (MYOPIC = 1)

#create the ROC curve
roc_rf <- roc(myopia$MYOPIC, oob_predictions)

#plot the ROC curve
plot(roc_rf, col = "red", lwd = 2, main = "ROC Curve for Random Forest Model")

text(0.5, 0.2, paste("AUC =", round(auc(roc_rf), 2)), col = "red", cex = 1)