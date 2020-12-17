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
## (Removed the 'sample code' variable in order to avoid creating multiple dummy variables)
## (Removed the 'no' variable to avoid serial numbers for each row taken as a continuous variable)
## (Removed the date variabel as the dates were converted into seasons; i.e, time series into categorial)
masterdata <- masterdata [, -c(1,2,7)]


# Splitting the master dataset into training and test dataset for outcome=CFU
## Step 1: Get row numbers for the training data
TrainRowNumbers <- createDataPartition(masterdata$CFU, p=0.8, list=FALSE)
## Step 2: Create the training dataset
TrainData <- masterdata[TrainRowNumbers,]
## Step 3: Create the test dataset
TestData <- masterdata[-TrainRowNumbers,]
## Store X & Y from training dataset for future use
x = TrainData[, 2:14]
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
## (Preprocessing the data by using the range transformation)
preProcess_range_model <- preProcess(TrainData, method='range')
TrainData <- predict(preProcess_range_model, newdata = TrainData)
# Append the Y variable
TrainData$CFU <- y
# Check if all the predictors (except response variable, CFU) range from 0 to 1
apply(TrainData[, 1:20], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})


# Visualizing the importance of different variables 
featurePlot(x = TrainData[, 11:19],
            y = TrainData$CFU,
            plot = "scatter",
            type = c("p", "smooth"))
## (This is merely for visualization purposes (and not variable selection purposes))


# Feature (predictor) selection using recursive feature elimination
options(warn=-1) 
subsets <- c(1:5, 10, 15, 18, 19)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
lmProfile <- rfe(x=TrainData[, 1:19], y=TrainData$CFU,
                 sizes = subsets,
                 rfeControl = ctrl)
lmProfile
## (However, this is only for this particular model = rfFuncs = random forest
## (Thus, not wise to select the variables based on this output only because it is restricted to one model)


# Training the model 
## (List of names of the various models present in caret )
names (getModelInfo())
## (Getting the information of individual models/algorithms)
modelLookup("earth")
## (Training the model using the 'earth' algorithm
## (which is a Multivariate Adaptive Regression Splines (MARS) model)
model_mars = train (CFU ~., data=TrainData, method='earth')
fitted <- predict(model_mars)
model_mars
## (Plotting the model shows how the various iterations of hyperparameter search performed)
varimp_mars <- varImp (model_mars)
plot(model_mars, main="Model Accuracies with MARS")

# Computing variable importance in the MARS model
varimp_mars <- varImp(model_mars, useModel = FALSE, nonpara = FALSE, scale = TRUE)
plot(varimp_mars, main="Variable Importance with MARS")


# Preparing the test dataset 
## Step 1: Impute missing values 
TestData2 <- predict(PreProcess_missing, TestData)  
# Step 2: Create one-hot encodings (dummy variables)
TestData3 <- predict(Dummies_model, TestData2)
# Step 3: Transform the features to range between 0 and 1
TestData4 <- predict(preProcess_range_model, TestData3)


# Predicting on the test dataset
predicted <- predict(model_mars, TestData4)
predicted


# Confusion Matrix


# Optimizing the model by tuning the model hyperparameters


# Training with other models
## Training the model using Random Forest
modelLookup('rf')
model_rf = train(CFU ~ ., data=TrainData, method='rf')
model_rf
## Training the model using svmRadial
modelLookup('svmRadial')
model_svmRadial = train(CFU ~ ., data=TrainData, method='svmRadial')
model_svmRadial
## Compare model performances using resample()
models_compare <- resamples(list(RF=model_rf, MARS=model_mars, SVM=model_svmRadial))
# Summary of the models performances
summary(models_compare)
## Draw box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)


# Comparing various models without having to train them individually
library(caretEnsemble)
## Stacking Algorithms - Run multiple algorithms/models in one call.
trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)

algorithmList <- c('rf', 'earth', 'svmRadial', 'glm', 
                   'glmnet', 'knn', 'rpart', 'treebag', 'gbm')
models <- caretList(CFU ~ ., data=TrainData, trControl=trainControl, methodList=algorithmList) 
results <- resamples(models)
summary(results)
## Box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)


# Combing the predictions of multiple models to form a final prediction
## Create the trainControl
stackControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)
## Ensemble the predictions of `models` to form a new combined prediction based on glm
stack.glm <- caretStack(models, method="glm", metric="rmse", trControl=stackControl)
print(stack.glm)
# Predict on testData
stack_predicteds <- predict(stack.glm, newdata=TestData4)
stack_predicteds

# Computing variable importance in the Combined Prediction based glm model
varimp_combined <- varImp(stack_predicteds, useModel = FALSE, nonpara = FALSE, scale = TRUE)
plot(varimp_combined, main="Variable Importance with MARS")
