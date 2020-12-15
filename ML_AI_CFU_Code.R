# ----------------------------------------------------------------------------------
# *DAIRY AGROECOSYSTEMS*
# *Machine Learning (Artificial Intelligence) Models*
# ---------------------------------------------------------------------------------
# *Author: Dharma*
# *Starting Date: Dec 14, 2020
# *Reference: https://www.machinelearningplus.com/machine-learning/caret-package/
# -----------------------------------------------------------------------------------

library(readxl)
library (tidyverse)
install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))
library (caret)

# Import master dataset & convert to data frame
masterdata <- read_excel("masterdata.xlsx")
masterdata <- as.data.frame (masterdata)


# Creating a concise dataset restricted to sample averages
masterdata <- masterdata [, c(1:7,11,15,19,23,27,31,35:37,42)]
## Removed the 'sample code' variable in order to avoid creating many dummy variables later on
masterdata <- masterdata [, -2]


# Splitting the master dataset into training and test dataset for outcome=CFU
## Step 1: Get row numbers for the training data
TrainRowNumbers <- createDataPartition(masterdata$CFU, p=0.8, list=FALSE)
## Step 2: Create the training dataset
TrainData <- masterdata[TrainRowNumbers,]
## Step 3: Create the test dataset
TestData <- masterdata[-TrainRowNumbers,]
## Store X & Y from training dataset for future use
x = TrainData[, 2:16]
y = TrainData$CFU


# Descriptive Statistics
library(skimr)
skimmed <- skim(TrainData)
skimmed
## [Note: e+03 means 10^3 where e stands for EXP = exponential.
## It is a way to read numerical data. It is not the same as the exponential constant (e = 2.71)]


# Preprocessing (incuding missing data imputation) of the training dataset  
## Create the knn imputation model on the training data
PreProcess_missing <- preProcess(TrainData, method='knnImpute') 
PreProcess_missing
## Use the imputation model to predict the values of missing data points
library (RANN)
TrainData <- predict (PreProcess_missing, newdata = TrainData)
anyNA (TrainData)
## [This preprocessing step, while imputing the missing data, also transforms
## numerical varaibles by centering (subtracting by mean) and scaling (dividing by std deviation)]


# Creating dummy variables for categorical variables (One-Hot encoding)
## (One-hot means only one of the dummy variables are 1, and all the others are 0)
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
Dummies_model <- dummyVars(CFU ~ ., data=TrainData)
# Create the dummy variables using predict. The Y variable (CFU) will not be present in trainData_mat.
TrainData_mat <- predict(Dummies_model, newdata = TrainData)
# Convert to dataframe
TrainData <- data.frame(TrainData_mat)
# See the structure of the new dataset
str(TrainData)


# Data transformation
## (Preprocessing the data by using the range transformation

preProcess_range_model <- preProcess(trainData, method='range')
trainData <- predict(preProcess_range_model, newdata = trainData)

# Append the Y variable
trainData$Purchase <- y

apply(trainData[, 1:10], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})




