library(tidyverse)
library(ggplot2)
library(gtsummary)
library(rsample)

myopiaData <- read.csv("data/myopia_og.csv", head = TRUE, sep = ";")

ggplot(myopiaData,aes(x = MYOPIC, y = SPHEQ))  +
  geom_boxplot()

spheqResult <- t.test(myopiaData$SPHEQ ~ myopiaData$MYOPIC)
print(spheqResult)

set.seed(0)
spheq_split <- initial_split(myopiaData, prop = 0.7) 
train_spheq <- training(spheq_split)
test_spheq <- testing(spheq_split)

spheq_model <- glm(MYOPIC~ SPHEQ, family=binomial, data=train_spheq)
summary(spheq_model)

spheq_model %>%
  tbl_regression(estimate_fun = function(x) style_number(x, digits = 3), exponentiate = TRUE)

spheq_prob <- spheq_model %>% 
  predict(test_spheq,type="response")

predicted.classes <- ifelse(spheq_prob > 0.5, "1", "0")
mean(predicted.classes == test_spheq$MYOPIC)

#Boxplot for AL VS MYOPIC
ggplot(myopiaData, aes(x = MYOPIC, y = AL))  +
  geom_boxplot()

alResult <- t.test(myopiaData$AL ~ myopiaData$MYOPIC)
print(alResult)
