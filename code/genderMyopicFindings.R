install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
setwd("Desktop")
setwd("cosmosMyopiaProject")
data <- read.csv("myopia_og.csv", head = TRUE, sep = ";")
glimpse(data)
nrow(data)
ncol(data)

head(data)
colnames(data)

data <- data %>%
  mutate(GENDER = case_when(GENDER == 1 ~ "male",
                            GENDER == 0 ~ "female")) 

data <- data %>%
  mutate(MYOPIC = case_when(MYOPIC == 1 ~ "yes",
                            MYOPIC == 0 ~ "no")) 

data <- data %>% 
  mutate(GENDER = as.factor(GENDER),
         MYOPIC = as.factor(MYOPIC), 
         AGE = as.factor(AGE)) 


select(data, GENDER, MYOPIC) %>% 
  head()

ggplot(data = data,
       aes(x = GENDER,
           fill = MYOPIC)) + 
  geom_bar(position = 'fill')

count(data, MYOPIC)
count(data, GENDER)
count(data, MYOPIC, GENDER)

test <- matrix(c(281, 256, 35, 46), nrow=2, ncol=2, byrow = TRUE)
test
chisq.test(test)

ncol(data)
nrow(data)
unique(data)

ggplot(data = data,
       aes(x = AGE,
           fill = MYOPIC)) + 
  geom_bar(position = 'fill')

count(data, AGE, MYOPIC)

logistic_model <-glm(MYOPIC ~ AGE*GENDER, family=binomial, data=data)
summary(logistic_model)
#Age and gender interaction are not significant

logistic_model <-glm(MYOPIC ~ GENDER + AGE, family=binomial, data=data)
summary(logistic_model)

#Tree model: building them the partition becomes so complex you only have one person per thing, but it doesnt generalize
#buiilding models to gernalize data for other model.response()
#Accuracy vs practocaloty
#optimazation vs generalization 
#deep learning
#build logistic regression to find out which varaibles are important then tree the random forest (rf) to see which varaible is imporatnat which variable was used by most trees
#cross validation
