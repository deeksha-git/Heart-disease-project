# Name- Deeksha RV
# Country - India
# Project title - Prediction of Heart Failure using ML algorithms

# Downloaded dataset from kaggle
# https://www.kaggle.com/fedesoriano/heart-failure-prediction

#Download following packages
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(dplyr)) install.packages("dplyr")
if(!require(readxl)) install.packages("readxl")
if(!require(knitr)) install.packages("knitr")
if(!require(caret)) install.packages("caret")
if(!require(gam)) install.packages("gam")
if(!require(MASS)) install.packages("MASS")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(kableExtra)) install.packages("kableExtra")



library(tidyverse)
library(dplyr)
library(readxl)
library(knitr)
library(caret)
library(gam)
library(MASS)
library(RColorBrewer)
library(kableExtra)

# import .csv file from working directory of laptop
heart_attack <- read.csv("C:/Users/hp/Documents/Capstone Projects/CYO project/heart attack prediction dataset.csv")

# kindly download dataset from my github repo before running this R script

### Viewing the Dataset ###

# what does the dataset give us ?

View(heart_attack)

# Age - in years

# Sex - M for male, F for female

table(heart_attack$ChestPainType)
# types of chest pain :
# ASY - asymptomatic
# ATA - atypical angina
# NAP - Non anginal pain
# TA - typical angina

# RestingBP - systolic, in mm Hg, at time of admission in hospital

# cholesterol - serum cholestoral in mg/dl

# Fasting BS - fasting blood sugar > 120 mg/dl, (1 = true; 0 = false)

# restecg - resting electrocardiographic results
# Normal = normal
# ST =  having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)
# LVH = showing probable or definite left ventricular hypertrophy by Estes' criteria

# MaxHR - maximum heart rate achieved

# ExerciseAngina - exercise induced angina
# Y = yes, N = no

# Oldpeak - ST depression induced by exercise relative to rest

# STslope -  the slope of the peak exercise ST segment
# Up = upsloping
# Flat =  flat
# Down =  downsloping

# HeartDisease
# 0 for absense, 1 for presence

# no missing values in dataset. checked using is.na() for all attributes

# add info about original dataset in cyo report. saved bookmark in google

### Modify the dataset ###

# change Yes and No to 1 and 0, respectively for values of ExerciseAngina variable
heart_attack$ExerciseAngina <- ifelse(heart_attack$ExerciseAngina == "Y",1,0)
View(heart_attack)

# change M=1 and F=0, respectively for values of Sex variable
heart_attack$Sex <- ifelse(heart_attack$Sex == "M",1,0)
View(heart_attack)

## change ASY=1,ATA=2,NAP=3,TA=4 for ChestPainType variable
# first change to factor, then to numeric
heart_attack$ChestPainType <- as.factor(heart_attack$ChestPainType)
heart_attack$ChestPainType <- as.numeric(heart_attack$ChestPainType)

## change RestingECG values to numeric values (LVH=1, Normal=2, ST=3)
heart_attack$RestingECG <- as.numeric(as.factor(heart_attack$RestingECG))

## change ST_Slope values : Down=1, Flat=2, Up=3
heart_attack$ST_Slope <- as.numeric(as.factor(heart_attack$ST_Slope))

### Exploratory Data Analysis ###
# dimensions of dataset
dim(heart_attack)
# 918 rows, 12 columns

# how many predictors for heart disease ?
# 11

# proportion of data of people who have heart disease
mean(heart_attack$HeartDisease == 1)
# comes to 0.5533769. means 55.3% of people in dataset had heart disease 


#how many men and women?
sum(heart_attack$Sex==1)
sum(heart_attack$Sex==0)

#725 men and 193 women

# types of attributes after modifying dataset
sapply(heart_attack, class)
# dataset has numerics and integers only. Characters were changed to numerics

# snippet of data
first10 <- head(heart_attack,10)
knitr::kable(first10)

# summarise data
summary <- summary(heart_attack)

# distribution of people by age
Graph1 <- heart_attack %>% ggplot(aes(Age)) + geom_histogram(binwidth = 0.5, fill = "orange") + xlab("Distribution of Age") + ggtitle("Graph 1")

# types of chest pain in patients
Graph2 <- heart_attack %>% ggplot(aes(ChestPainType)) + geom_bar(fill = "magenta") + xlab("Chest Pain Type") + ggtitle("Graph 2")
# highest cases of ASY or Asymptomatic type chest pain is seen (1 = ASY as per modification)

# how does Fasting Blood Sugar vary with Age
Graph3 <- heart_attack %>% ggplot(aes(factor(FastingBS), Age, fill = factor(FastingBS))) + geom_boxplot(alpha = 0.3) + xlab("Fasting Blood Sugar") + ylab("Age in Years") + ggtitle("Graph 3") + scale_fill_discrete("Legend")
# inference - fasting blood sugar greater than 120 mg/dl is prevalent in ages 52-60 years, the avg being at 56 years

# spatial heatmap of all the features
# create matrix first. heatmap can only take matrix
matrix <- data.matrix(heart_attack)
heatmap(matrix, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column")


# find relationship of all attributes with HeartDisease
# calculate pearson's correlation coefficient for each variable with Heartdisease
pearson <- heart_attack %>% summarise(Parameter = c("Age","Sex","ChestPainType","Resting BP","Cholesterol","Fasting BS","RestingECG","Max HR","Exercise Angina","Old peak","ST Slope"), 
                                      "correlation with heart" = c(cor(Age, HeartDisease, method = "pearson"),
                                                                   cor(Sex,HeartDisease, method = "pearson"),
                                                                   cor(ChestPainType, HeartDisease, method = "pearson"),
                                                                   cor(RestingBP,HeartDisease, method = "pearson"),
                                                                   cor(Cholesterol,HeartDisease,method = "pearson"),
                                                                   cor(FastingBS,HeartDisease,method = "pearson"),
                                                                   cor(RestingECG, HeartDisease, method = "pearson"),
                                                                   cor(MaxHR,HeartDisease,method = "pearson"),
                                                                   cor(ExerciseAngina, HeartDisease, method = "pearson"),
                                                                   cor(Oldpeak,HeartDisease,method = "pearson"), 
                                                                   cor(ST_Slope, HeartDisease, method = "pearson")
                                                                   ))

View(pearson)

# calculate spearman's correlation coffecient for each numeric variable with Heartdisease
spearman <- heart_attack %>% 
  summarise(Parameter = c("Age","Sex","ChestPainType","Resting BP","Cholesterol","Fasting BS","Resting ECG","Max HR","Exercise Angina","Old peak","ST Slope"), 
            "correlation with heart disease" = c(cor(Age, HeartDisease, method = "spearman"),
                                                 cor(Sex, HeartDisease, method = "spearman"),
                                                 cor(ChestPainType, HeartDisease, method = "spearman"),
                                                 cor(RestingBP,HeartDisease, method = "spearman"),
                                                 cor(Cholesterol,HeartDisease,method = "spearman"),
                                                 cor(FastingBS,HeartDisease,method = "spearman"),
                                                 cor(RestingECG, HeartDisease, method = "spearman"),
                                                 cor(MaxHR,HeartDisease,method = "spearman"),
                                                 cor(ExerciseAngina,HeartDisease, method = "spearman"),
                                                 cor(Oldpeak,HeartDisease,method = "spearman"),
                                                 cor(ST_Slope, HeartDisease, method = "spearman")
                                                 ))
View(spearman)



## Building the algorithm 

# create vector y, having all values of HeartDisease
y <- heart_attack$HeartDisease

# create subset of heart attack dataset by removing HeartDisease variable
heart_attack_2 <- heart_attack %>% subset(select = -c(HeartDisease))

# scale the new dataset
scaled_heart_attack <- scale(heart_attack_2)
View(scaled_heart_attack)

# create test and train sets
test_index <- createDataPartition(y, times = 1, p = 0.25, list = FALSE)

# test set of heart_attack. HA stands for Heart Attack. 
test_HA <- scaled_heart_attack[test_index,]

# test set of y (HeartDisease)
test_y <- y[test_index]

# train set of heart_attack. HA stands for Heart Attack. 
train_HA <- scaled_heart_attack[-test_index,]

# train set of y (HeartDisease)
train_y <- y[-test_index]

### Models ###

# convert y, test_y and train_y to factors
y <- as.factor(y)
train_y <- as.factor(train_y)
test_y <- as.factor(test_y)


# fit a lda model
train_lda <- train(train_HA, train_y, method = "lda", family = "binomial")
lda_preds <- predict(train_lda, test_HA)
mean(lda_preds == test_y)

# accuracy is 0.8565217 or 85.6%

# fit a qda model 
train_qda <- train(train_HA, train_y, method = "qda", family = "binomial")
qda_preds <- predict(train_qda, test_HA)
mean(qda_preds == test_y)

# accuracy is 0.8304348 or 83.04% 

# fit a logistic regression model 
train_glm <- train(train_HA, train_y, method = "glm", family = "binomial")
glm_preds <- predict(train_glm, test_HA)
mean(glm_preds == test_y)

# accuracy is 0.8652174 or 86.5%

# fit a loess model 
train_loess <- train(train_HA, train_y, method = "gamLoess")
loess_preds <- predict(train_loess, test_HA)
mean(loess_preds == test_y)

# accuracy is 0.8695652 or 87%


# fit a knn model 
set.seed(7, sample.kind = "Rounding") 
tuning <- data.frame(k = seq(21, 41, 2))
train_knn <- train(train_HA, train_y,
                   method = "knn", 
                   tuneGrid = tuning)

# find best k value
train_knn$bestTune

knn_preds <- predict(train_knn, test_HA)
mean(knn_preds == test_y)

# accuracy is 88.69% 

# fit a rf model
set.seed(7, sample.kind = "Rounding")
tune_rf <- data.frame(mtry = c(5,7,9,13))
train_rf <- train(train_HA, train_y,
                  method = "rf",
                  tuneGrid = tune_rf,
                  importance = TRUE)

# get best mtry value
train_rf$bestTune

rf_preds <- predict(train_rf, test_HA)
mean(rf_preds == test_y)

# accuracy is 0.8826087 or 88.26%


# ensemble model 
# first ensemble model has all models
ensemble <- cbind(lda = lda_preds == 1, qda = qda_preds == 1, glm = glm_preds ==1, loess = loess_preds == 1, knn = knn_preds == 1, rf = rf_preds == 1)
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, 1, 0)
mean(ensemble_preds == test_y)

# accuracy is 86.95% or 87%

# second ensemble model uses only rf and knn models 
ensemble_2 <- cbind(rf = rf_preds ==1, knn = knn_preds ==1)
ensemble_2_preds <- ifelse(rowMeans(ensemble_2)> 0.5, 1, 0)
mean(ensemble_2_preds== test_y)

# accuracy is exactly 90%; final accuracy of my algorithm.

#*****************************************************************************#


















