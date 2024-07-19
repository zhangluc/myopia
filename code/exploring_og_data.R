library(tidyverse)
library(ggplot2)
library(gtsummary)
library(rsample)

# setwd('desktop')
# setwd(cosmos24)
# setwd('cosmos24')
# setwd('myopia project')
# setwd('data')

# Use rm(dataName) to remove unwanted datatables
myopiaData <- read.csv("myopia_og.csv", head = TRUE, sep = ";")
myopiaGraph <- read.csv("myopia_og.csv", head = TRUE, sep = ";")

# Changing myopic to a factor variable
myopiaGraph <- myopiaGraph %>% 
  mutate(MYOPIC = case_when(MYOPIC == 1 ~ "Yes", 
                            MYOPIC == 0 ~ "No"))
# If myopic = 1, child has myopia
# If myopic = 0, child does NOT have myopia

myopiaGraph <- myopiaGraph %>% 
  mutate(MYOPIC = as.factor(MYOPIC))

# EYE CHARACTERISTICS
#-------------------------------------------------------------------------------
# SPHEQ: Spherical equivalent of the participant's refractive error: 
# sphere power plus half of the cylinder power

# Mild Myopia: SPHEQ values between -0.25 and -3.00 diopters (D)
# Moderate Myopia: SPHEQ values between -3.00 and -6.00 diopters (D)
# High Myopia: SPHEQ values greater than -6.00 diopters (D)

# Decrease (More - ): Indicates more severe myopia.
# Increase ( + or Less - ): Indicates less severe myopia or hyperopia (farsightedness).

# represent a person's refractive error in a single value that combines the effects of 
# nearsightedness (myopia), farsightedness (hyperopia), and astigmatism into one number

# Boxplot for SPHEQ VS MYOPIC
ggplot(myopiaGraph,
       aes(x = MYOPIC,
           y = SPHEQ))  +
  geom_boxplot()

# t-test for spheq vs. myopia
spheqResult <- t.test(myopiaData$SPHEQ ~ myopiaData$MYOPIC)
print(spheqResult)

#Logistic model for spheq vs. myopia
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

# Calculates model's accuracy in prediction
predicted.classes <- ifelse(spheq_prob > 0.5, "1", "0")
mean(predicted.classes == test_spheq$MYOPIC)
# This has a 90% accuracy rate
#-------------------------------------------------------------------------------
# AL: Axial length of the eye: 
# measurement of the eye from the front surface of the cornea to the back surface of the retina

# Ages 6-10: An axial length greater than approximately 23.0 mm MIGHT indicate myopia.

# Increase: A longer axial length is associated with more severe myopia.
# Decrease: A shorter axial length is associated with less severe myopia or normal vision.

# Helps w/ diagnosing myopia 

# Boxplot for AL VS MYOPIC
ggplot(myopiaGraph,
       aes(x = MYOPIC,
           y = AL))  +
  geom_boxplot()

# t-test for al vs. myopia
alResult <- t.test(myopiaData$AL ~ myopiaData$MYOPIC)
print(alResult)

#Logistic model for al vs. myopia
set.seed(0)
al_split <- initial_split(myopiaData, prop = 0.7) 
train_al <- training(al_split)
test_al <- testing(al_split)

al_model <- glm(MYOPIC~ AL, family=binomial, data=train_al)
summary(al_model)

al_model %>%
  tbl_regression(estimate_fun = function(x) style_number(x, digits = 3), exponentiate = TRUE)

al_prob <- al_model %>% 
  predict(test_al,type="response")

# Calculates model's accuracy in prediction
predicted.classes <- ifelse(al_prob > 0.5, "1", "0")
mean(predicted.classes == test_al$MYOPIC)
# This has a 88% accuracy rate
#-------------------------------------------------------------------------------
# ACD: Anterior chamber depth: 
# distance between the back of the cornea (the clear front part of the eye) 
# and the front of the iris (the colored part of the eye)

# Increase: Not directly correlated with myopia severity but can be associated with overall eye growth.
# Decrease: May indicate a shallower anterior chamber, which is less directly related to myopia.

# helps assess the risk of certain eye conditions

# Boxplot for ACD VS MYOPIC
ggplot(myopiaGraph,
       aes(x = MYOPIC,
           y = ACD))  +
  geom_boxplot()

# t-test for acd vs. myopia
acdResult <- t.test(myopiaData$ACD ~ myopiaData$MYOPIC)
print(acdResult)

#Logistic model for acd vs. myopia
set.seed(0)
acd_split <- initial_split(myopiaData, prop = 0.7) 
train_acd <- training(acd_split)
test_acd <- testing(acd_split)

acd_model <- glm(MYOPIC~ ACD, family=binomial, data=train_acd)
summary(acd_model)

acd_model %>%
  tbl_regression(estimate_fun = function(x) style_number(x, digits = 3), exponentiate = TRUE)

acd_prob <- acd_model %>% 
  predict(test_acd,type="response")

# Calculates model's accuracy in prediction
predicted.classes <- ifelse(acd_prob > 0.5, "1", "0")
mean(predicted.classes == test_acd$MYOPIC)
# This has a 88% accuracy rate
#-------------------------------------------------------------------------------
# LT: Lens thickness. (of glasses)

# Increase: Thicker lenses can contribute to myopic shifts but the relationship is complex.
# Decrease: Thinner lenses are less likely to contribute to myopia.

# Boxplot for LT VS MYOPIC
ggplot(myopiaGraph,
       aes(x = MYOPIC,
           y = LT))  +
  geom_boxplot()

# t-test for lt vs. myopia
ltResult <- t.test(myopiaData$LT ~ myopiaData$MYOPIC)
print(ltResult)

#Logistic model for lt vs. myopia
set.seed(0)
lt_split <- initial_split(myopiaData, prop = 0.7) 
train_lt <- training(lt_split)
test_lt <- testing(lt_split)

lt_model <- glm(MYOPIC~ LT, family=binomial, data=train_lt)
summary(lt_model)

lt_model %>%
  tbl_regression(estimate_fun = function(x) style_number(x, digits = 3), exponentiate = TRUE)

lt_prob <- lt_model %>% 
  predict(test_lt,type="response")

# Calculates model's accuracy in prediction
predicted.classes <- ifelse(lt_prob > 0.5, "1", "0")
mean(predicted.classes == test_lt$MYOPIC)
# This has a 88% accuracy rate
#-------------------------------------------------------------------------------
# VCD: Vitreous chamber depth: 
# distance from the back of the lens to the retina inside the eye

# Increase: Greater vitreous chamber depth is strongly associated with more severe myopia.
# Decrease: Shallower vitreous chamber depth is associated with less severe myopia or normal vision.

# Boxplot for VCD VS MYOPIC
ggplot(myopiaGraph,
       aes(x = MYOPIC,
           y = VCD))  +
  geom_boxplot()

# t-test for vcd vs. myopia
vcdResult <- t.test(myopiaData$VCD ~ myopiaData$MYOPIC)
print(vcdResult)

#Logistic model for vcd vs. myopia
set.seed(0)
vcd_split <- initial_split(myopiaData, prop = 0.7) 
train_vcd <- training(vcd_split)
test_vcd <- testing(vcd_split)

vcd_model <- glm(MYOPIC~ VCD, family=binomial, data=train_vcd)
summary(vcd_model)

vcd_model %>%
  tbl_regression(estimate_fun = function(x) style_number(x, digits = 3), exponentiate = TRUE)

vcd_prob <- vcd_model %>% 
  predict(test_vcd,type="response")

# Calculates model's accuracy in prediction
predicted.classes <- ifelse(vcd_prob > 0.5, "1", "0")
mean(predicted.classes == test_vcd$MYOPIC)
# This has a 88% accuracy rate
#-------------------------------------------------------------------------------
