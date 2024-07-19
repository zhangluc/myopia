install.packages('tidyverse')
library(tidyverse)
library(ggplot2)
library(dplyr)

setwd('data')
myopia <- read.csv('myopia_og.csv', sep=";", header=TRUE)
myopia
head(myopia)

hours <- select(myopia, contains("hr"), contains("myopic"))

hours <- hours %>% 
  mutate(MYOPIC = as.factor(MYOPIC))

# Summary statistics
summary_stats <- hours %>%
  group_by(MYOPIC) %>%
  summarise(across(c(SPORTHR, TVHR, COMPHR, READHR, STUDYHR, DIOPTERHR), list(mean = mean, sd = sd, median = median, IQR = IQR), .names = "{col}_{fn}"))

print(summary_stats)

#two-sample t-test for each variable
t_test_results <- list(
  sporthr = t.test(sporthr ~ myopic, data = hours), #12.26 vs 9.94
  tvhr = t.test(tvhr ~ myopic, data = hours), #8.96 vs 8.89
  comphr = t.test(comphr ~ myopic, data = hours), #2.07 vs 2.31
  readhr = t.test(readhr ~ myopic, data = hours), #2.71 vs 3.37
  studyhr = t.test(studyhr ~ myopic, data = hours), #1.52 vs 1.31
  diopter = t.test(diopterhr ~ myopic, data = hours) #25.79 vs 27.54
)

t_test_results

#Mann-Whitney U Test (Wilcoxon Test): rank sum 
wilcox_test_results <- list(
  sporthr = wilcox.test(sporthr ~ myopic, data = hours), #statistically significant, p-val = 0.004804
  tvhr = wilcox.test(tvhr ~ myopic, data = hours), #p-val = 0.8204
  comphr = wilcox.test(comphr ~ myopic, data = hours), #p-val = 0.8927
  readhr = wilcox.test(readhr ~ myopic, data = hours), #p-val = 0.102
  studyhr = wilcox.test(studyhr ~ myopic, data = hours), #p-val = 0.9676
  diopterhr = wilcox.test(diopterhr ~ myopic, data = hours) #p-val = 0.3169
)

wilcox_test_results

#readhr vs sporthr
ggplot(hours,
       aes(x = readhr,
           y = sporthr,
           color = myopic)) +
  geom_point()

#sporthr vs tvhr
ggplot(hours,
       aes(x = sporthr,
           y = tvhr,
           color = myopic)) +
  geom_point()

#logistic regression: sports vs tv hours (same but looks diff)
model <- glm(myopic ~ SPORTHR + TVHR, data = hours, family = binomial)
summary(model)

ggplot(hours, aes(x = SPORTHR, y = TVHR, color = myopic)) +
  geom_point(size = 2) +
  labs(x = "Hours Spent on Sports", y = "Hours Spent on TV", title = "Logistic Regression Decision Boundary") +
  theme_minimal()

#box plot: ALL 6 HOURS
pivot_longer(hours,
  contains('HR'),
  names_to = "category",
  values_to = "hours"
) %>%
  ggplot(aes(x = category, y = hours, color = MYOPIC)) +
  geom_boxplot(position = "dodge")

#box plot: COMPHR
pivot_longer(hours,
             "COMPHR",
             names_to = "category",
             values_to = "hours"
) %>%
  ggplot(aes(x = category, y = hours, color = MYOPIC)) +
  geom_boxplot(position = "dodge")

#box plot: READHR
pivot_longer(hours,
             "READHR",
             names_to = "category",
             values_to = "hours"
) %>%
  ggplot(aes(x = category, y = hours, color = MYOPIC)) +
  geom_boxplot(position = "dodge")

#box plot: TVHR
pivot_longer(hours,
             "TVHR",
             names_to = "category",
             values_to = "hours"
) %>%
  ggplot(aes(x = category, y = hours, color = MYOPIC)) +
  geom_boxplot(position = "dodge")

#box plot: SPORTHR
pivot_longer(hours,
             "SPORTHR",
             names_to = "category",
             values_to = "hours"
) %>%
  ggplot(aes(x = category, y = hours, color = MYOPIC)) +
  geom_boxplot(position = "dodge")

#box plot: STUDYHR
pivot_longer(hours,
             "STUDYHR",
             names_to = "category",
             values_to = "hours"
) %>%
  ggplot(aes(x = category, y = hours, color = MYOPIC)) +
  geom_boxplot(position = "dodge")

#box plot: DIOPTERHR
pivot_longer(hours,
             "DIOPTERHR",
             names_to = "category",
             values_to = "hours"
) %>%
  ggplot(aes(x = category, y = hours, color = MYOPIC)) +
  geom_boxplot(position = "dodge")

#summarize info
pivot_longer(hours,
             contains('HR'),
             names_to = "category",
             values_to = "hours"
) %>% 
  group_by(MYOPIC, category) %>% 
  summarize(mean = mean(hours))
