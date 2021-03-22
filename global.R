####################################
#### Messy Data - Global.R ###
####################################


library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(tibble)
library(purrr)
library(stringr)
library(forcats)
library(corrplot)
library(caTools)
library(ROCR)
library(plotROC)
library(gplots)
library(magrittr)
library(shiny)
library(tibble)
###### tidyverse
# Read CSV into R

df1 <- readr::read_csv(file="./inst/extdata/messy-data-tail.csv", col_names=TRUE, col_types = readr::cols())

# call general data-preprocessing
df1 <- general_data_preprocess(df1)

# create cs_es_table
cs_es_table <- create_cs_es_table(df1)

# create cs_es_table
es_ab_table <- create_es_ab_table(df1)


################ Task 3 - key factor that determines the tendency to move closer to the work place (keen_move) and other factors (what is the most relevant factor influencing the will to move home closer work)

# Regression - level 2
# independent variables - all can be made into factors
# - current_work
# - before_work,
# - city_size
# - gender,
# - age,
# - education
# - current salary levels
# - expected salary levels
# - how far commuting
# - ability to commute

# Task 3 - level 1 preprocessing
df_level1 <- task3_preprocessing_level1(df1)

# Task 3 - level 2 preprocessing
df_level2 <- task3_preprocessing_level2(df1)


############## preparing model parameters Task 3 level 2 #############################

# set up parameters for logistic regression with keen to move as dependent factor and independent level 2 factors
y <- "keen_move"
x <- "current_work + city_size + gender + age + education + hf_com + ab_com + curr_sal + exp_sal"
dataset <- df_level2

# calculate logistic regression with keen to move as dependent factor and independent level 2 factors
model2 <- glm_function(y, x, dataset)
summary(model2)

# logistic regression with training and test
# split df_level2 into training and test datasets
split <- sample.split(df_level2$keen_move, SplitRatio = 0.75)

level2_Train = subset(df_level2, split == TRUE)
level2_Test = subset(df_level2, split == FALSE)

# display the number of rows in each dataset
nrow(level2_Train)
nrow(level2_Test)

# convert selected columns to factor
level2_Train <-
  level2_Train %>%
  mutate(across(c(current_work,
                  before_work,
                  keen_move,
                  city_size,
                  gender,
                  age,
                  education,
                  com_level,
                  sal_level),
                factor))

# convert selected columns to factor
level2_Train <-
  level2_Train %>%
  mutate(across(c(hf_com,
                  ab_com,
                  curr_sal,
                  exp_sal),
                factor))

# set up parameters for:
# logistic regression with keen to move as dependent factor and independent level 2 factors and training set
y <- "keen_move"
x <- "current_work + city_size + gender + age + education + hf_com + ab_com + curr_sal + exp_sal"
dataset <- level2_Train

# calculate logistic regression with keen to move as dependent factor and independent level 2 factors and training set
Level2_Train_Log <- glm_function(y, x, dataset)
summary(Level2_Train_Log)

# Training set predictions from level 1 training set logistic regression
predictTrain2 <- predict(Level2_Train_Log, type="response")
summary(predictTrain2)

# apply predictions to level2_train dataset mean
tapply(predictTrain2, level2_Train$keen_move, mean)


# Confusion matrix for threshold of 0.5 for training set
Train_5_2 <- sesp_func(level2_Train$keen_move, predictTrain2, 0.5)

# Confusion matrix for threshold of 0.7 for training set
Train_7_2 <- sesp_func(level2_Train$keen_move, predictTrain2, 0.6)

# Confusion matrix for threshold of 0.2 for training set
Train_2_2 <- sesp_func(level2_Train$keen_move, predictTrain2, 0.2)

# calculate ROC (Receiver Operating Characteristics) predictions for training set
ROCRpred2 = prediction(predictTrain2, level2_Train$keen_move)

# Performance function
ROCRperf2 = performance(ROCRpred2, "tpr", "fpr")

#  apply Training set predictions from level 2 training set logistic regression to test dataset
 predictTest2 <- predict(Level2_Train_Log, type = "response", newdata = level2_Test)

# Predict the accuracy of the model with the test set
pr <- prediction(predictTest2, level2_Test$keen_move)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]


# Confusion matrix for threshold of 0.6 for training set
Test_6_2 <- sesp_func(level2_Test$keen_move, predictTest2, 0.6)

# convert list to dataframe to round numbers
Test_6_2_df <- data.frame(matrix(unlist(Test_6_2), nrow=length(Test_6_2), byrow=T))





