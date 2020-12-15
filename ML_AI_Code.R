# ----------------------------------------------------------------------------------
# *DAIRY AGROECOSYSTEMS*
# *Machine Learning (Artificial Intelligence) Models*
# ---------------------------------------------------------------------------------
# *Author: Dharma*
# *Start Date: Dec 14, 2020
# *Reference: https://www.machinelearningplus.com/machine-learning/caret-package/
# -----------------------------------------------------------------------------------

library(readxl)
library (tidyverse)
install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))
library (caret)

# Import master dataset & convert to data frame
masterdata <- read_excel("masterdata.xlsx")
masterdata <- as.data.frame (masterdata)

# Splitting the master dataset into training and test dataset for outcome=CFU
## Step 1: Get row numbers for the training data
TrainRowNumbers_CFU <- createDataPartition(masterdata$CFU, p=0.8, list=FALSE)
## Step 2: Create the training dataset
TrainData_CFU <- masterdata[TrainRowNumbers_CFU,]
## Step 3: Create the test dataset
TestData_CFU <- masterdata[-TrainRowNumbers_CFU]
## Store X & Y from training dataset for future use
x = TrainData_CFU[, 2:42]
y = TrainData_CFU$CFU





