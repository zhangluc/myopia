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

select(data, SPHEQ, SPORTHR) %>% 
  head()


ggplot(data = data,
       aes(x = as.factor(MYOPIC),
           fill = SPORTHR >= 11.95307)) + 
  geom_bar(position = "fill")

ggplot(data = data,
       aes(x = as.factor(MYOPIC),
           fill = SPHEQ >= 0.8010097)) + 
  geom_bar(position = "fill")


# Basic line plot with points
ggplot(data=data, aes(x=SPORTHR, y=SPHEQ, group=1)) +
  geom_point()

hist(data$SPHEQ)
hist(data$SPORTHR)
count(data, MYOPIC)
mean(data$SPHEQ)
mean(data$SPORTHR)
count(data, MYOPIC, SPHEQ <= 0.8010097)
count(data, MYOPIC, SPORTHR <= 11.95307)
head(data$SPHEQ)
#negative spheq is myopia, 
#0 is normal
#combine SPHEQ and sporthr

#means hours of people with vs without myopia
linear_model <-lm(MYOPIC ~ SPORTHR*SPHEQ, family=binomial, data=data)
summary(linear_model)