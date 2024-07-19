install.packages('tidyverse')
library(tidyverse)
library(ggplot2)

setwd('data')
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)
myopia
head(myopia)

hours <- select(myopia, contains("hr"), contains("myopic"))

hours <- hours %>% 
  mutate(MYOPIC = as.factor(MYOPIC))

myopic <- hours$MYOPIC
sporthr <- hours$SPORTHR
readhr <- hours$READHR
comphr <- hours$COMPHR
studyhr <- hours$STUDYHR
tvhr <- hours$TVHR
diopterhr <- hours$DIOPTERHR #hours of glasses


#readhr vs sport hr
ggplot(hours,
       aes(x = readhr,
           y = sporthr,
           color = myopic)) +
  geom_point()

ggplot(hours,
       aes(x = sporthr,
           y = tvhr,
           color = myopic)) +
  geom_point()

#box plot
pivot_longer(hours,
  contains('HR'),
  names_to = "category",
  values_to = "hours"
) %>%
  ggplot(aes(x = category, y = hours, color = MYOPIC)) +
  geom_boxplot(position = "dodge")


#box plot
pivot_longer(hours,
             "COMPHR",
             names_to = "category",
             values_to = "hours"
) %>%
  ggplot(aes(x = category, y = hours, color = MYOPIC)) +
  geom_boxplot(position = "dodge")
