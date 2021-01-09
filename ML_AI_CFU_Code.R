# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# *DAIRY AGROECOSYSTEMS*
# *Machine Learning (Artificial Intelligence) Models*
# ----------------------------------------------------------------------------------
# *Author: Dharma*
# *Start Date: Dec 14, 2020
# *Reference: https://topepo.github.io/caret/index.html
# *Reference: https://www.machinelearningplus.com/machine-learning/caret-package/
# *Reference: https://www.jstatsoft.org/article/view/v028i05
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------


# -----------------------------------------------------------------
# 1: EXPLORATORY DATA ANALYSIS: DATASET, LIBRARIES, & VISUALIZATION
# -----------------------------------------------------------------

install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))
library(readxl)
library (caret)
library (tidyverse)

# Import master dataset & convert to data frame
masterdata <- read_excel("masterdata.xlsx")
masterdata <- as.data.frame (masterdata)


# Creating a concise dataset restricted to sample averages
masterdata <- masterdata [, c(1:7,11,15,19,23,27,31,35:37,42)]
## (Removed the 'farm' variable since the treatment variable is a duplicate of the 'farm' variable)
## (Removed the 'sample code' variable in order to avoid creating multiple dummy variables)
## (Removed the 'no' variable to avoid serial numbers for each row taken as a continuous variable)
## (Removed the date variabel as the dates were converted into seasons; i.e, time series into categorial)
masterdata <- masterdata [, -c(1:3,7)]


# Descriptive Statistics
library(skimr)
skim_descriptive <- skim(masterdata)
skim_descriptive 
  ## skim_descriptive %>% 
    ## write.csv('skim_desc.csv') 
## [The above dplyr pipeline command to write.csv needs more work since the histograms are not showing up.
## Thus, I took a screenshot instead]
## [Note: e+03 means 10^3 where e stands for EXP = exponential.
## It is a way to read numerical data. It is not the same as the exponential constant (e = 2.71)]


# Visualizing the importance or relationship of different variables to the outcome
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = masterdata[,4:12],
            y = masterdata$CFU,
            plot = "scatter",
            type = c("p", "smooth"))
pdf ("boxplot.pdf")
ggplot (masterdata, aes (x = Treatment, y = CFU)) + 
  geom_boxplot()
ggplot (masterdata, aes (x = Stage, y = CFU)) + 
  geom_boxplot()
ggplot (masterdata, aes (x = Season, y = CFU)) + 
  geom_boxplot()
dev.off()


# Visualizing predictor correlations
library (corrplot)
masterdata_num <- masterdata [, c(4:13)]
masterdata_num <- na.omit (masterdata_num)
anyNA (masterdata_num)
correlations = cor(masterdata_num)
corrplot(correlations, method="color")



# ---------------------------------------------------------------------
# 2: PRE-PROCESSING THE MASTER DATASET
# ---------------------------------------------------------------------


# Identifying zero & near-zero variance predictors
nzv <- nearZeroVar(masterdata, saveMetrics = TRUE)
nzv
## [There are no zero or near Zero Value variables to delete from the dataset]
## dim (masterdata)
  ## nzv <- nearZeroVar (masterdata)
  ## masterdata_filtered <- masterdata [, -nzv]
  ## dim (masterdata_filtered)


# Identifying correlated predictors
masterdata_cor <-  cor (masterdata_num)
## Finding variables that are correlated above 0.999
highCorr <- sum(abs(masterdata_cor[upper.tri(masterdata_cor)]) > .999)
highCorr
## [There are no variables that are correlated > 0.999, and thus, no variables need to be deleted]
## Removing highly correlated variables above 0.75 if the correlated variables had been found
  ## summary (masterdata_cor [upper.tri(masterdata_cor)])
  ## highlyCorMasterdata <- findCorrelation(masterdata_cor, cutoff = .75)
  ## masterdata_filtered <- masterdata [, -highlyCorMasterdata]
  ## masterdata_cor_2 <- cor (masterdata_filtered)
  ## summary (masterdata_cor_2 [upper.tri(masterdata_cor_2)]) 


# Finding linear dependancies 
## [Does not apply here]
  ## comboInfo <- findLinearCombos(masterdata)
  ## comboInfo




# ---------------------------------------------------------------------
# 3: PRE-PROCESSING THE TRAINING & TEST DATASETS AFTER DATA SPLITTING  
# ---------------------------------------------------------------------


# Splitting the master dataset into training and test dataset, based on outcome=CFU
set.seed (100)
## Step 1: Get row numbers for the training data
TrainRowNumbers <- createDataPartition(masterdata$CFU, p=0.8, list=FALSE)
## Step 2: Create the training dataset
TrainData <- masterdata[TrainRowNumbers,]
## Step 3: Create the test dataset
TestData <- masterdata[-TrainRowNumbers,]
## Store X & Y from training dataset for future use
x_train = TrainData[, 1:12]
y_train = TrainData$CFU
x_test = TestData[, 1:12]
y_test = TestData$CFU


# (1) Data Imputation Model: Creating a Data Imputation Model (knn imputation model) using the Training Dataset
## [This preprocessing step, while imputing the missing data, also transforms
## numerical varaibles by centering (subtracting by mean) and scaling (dividing by std deviation)]
MissingDataModel <- preProcess(TrainData, method='knnImpute') 
MissingDataModel


# (2) Dummy Variable Model: Creating a Dummy Variable Model using the Training Dataset
## [One-hot means only one of the dummy variables are 1, and all the others are 0.]
## [Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.]
## [Creating the dummy variables using this function removes the outcome variable]
DummiesModel <- dummyVars(CFU ~ ., data = TrainData)
DummiesModel


# (3) Data Transformation Model: Model to Transform the values of predictor variables to a range between 0 and 1 using Training Dataset
  ## TransformationModel <- preProcess(TrainData, method ='range')
  ## TransformationModel
## [This model has to be built after the Training/Test Datasets have been put through the Dummy Variable Model 
## since it changes the no. of columns]


# Pre-processing the Training Dataset
## (1) Imputation
library (RANN)
TrainData <- predict (MissingDataModel, newdata = TrainData)
anyNA (TrainData)
## (2) Dummy variable creation
TrainData_dummies <- predict(DummiesModel, newdata = TrainData)
TrainData <- data.frame(TrainData_dummies)
str(TrainData)
## (3) Transformation (Range)
TransformationModel <- preProcess(TrainData, method ='range')
TrainData <- predict(TransformationModel, newdata = TrainData)
### [Check if all the predictors (except response variable, CFU) range from 0 to 1]
apply(TrainData[, 1:17], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})
## Append the Y variable and convert to df
TrainData$CFU <- y_train
TrainData <- as.data.frame(TrainData)


# Pre-processing the Test Dataset
## (1) Imputation
TestData <- predict (MissingDataModel, newdata = TestData)
anyNA (TestData)
## (2) Dummy variable creation
TestData_dummies <- predict(DummiesModel, newdata = TestData)
TestData <- data.frame(TestData_dummies)
str(TestData)
## (3) Transformation (Range)
TestData <- predict(TransformationModel, newdata = TestData)
## Append the Y variable and convert to df
TestData$CFU <- y_test
TestData <- as.data.frame(TestData)


# Exploratory Feature Selection for the Training Dataset
## [Feature (predictor) selection using recursive feature elimination for algorithms, namely
## (1) Random Forest, (2) Linear Model, (3) Bagged Trees, and (4) Carets Train]
options(warn=-1) 
subsets <- c(1:17)
## (Creating the control objects)
ctrl_rf <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 5,
                      verbose = FALSE)
ctrl_lm <- rfeControl(functions = lmFuncs,
                      method = "repeatedcv",
                      repeats = 5,
                      verbose = FALSE)
ctrl_tb <- rfeControl(functions = treebagFuncs,
                      method = "repeatedcv",
                      repeats = 5,
                      verbose = FALSE)
ctrl_ct <- rfeControl(functions = caretFuncs,
                      method = "repeatedcv",
                      repeats = 5,
                      verbose = FALSE)
## (Creating the simulation model for Recursive Feature Elimination)
lmProfile_rf <- rfe(x=TrainData[, 1:17], y=TrainData$CFU,
                    sizes = subsets,
                    rfeControl = ctrl_rf)
lmProfile_lm <- rfe(x=TrainData[, 1:17], y=TrainData$CFU,
                    sizes = subsets,
                    rfeControl = ctrl_lm)
lmProfile_tb <- rfe(x=TrainData[, 1:17], y=TrainData$CFU,
                    sizes = subsets,
                    rfeControl = ctrl_tb)
lmProfile_ct <- rfe(x=TrainData[, 1:17], y=TrainData$CFU,
                    sizes = subsets,
                    rfeControl = ctrl_ct)
lmProfile_rf
lmProfile_lm
lmProfile_tb
lmProfile_ct
## (Visualizing the predictors for the selected model)
predictors (lmProfile_rf)
predictors (lmProfile_lm)
predictors (lmProfile_tb)
predictors (lmProfile_ct)
## (Creating a comparitive dataframe for the various models)
Table <- as.data.frame (matrix(0, ncol = 1, nrow = 17))
Table$V1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
              11, 12, 13, 14, 15, 16, 17)
colnames(Table) <- c('rank')
a <- data.frame (predictors (lmProfile_rf))
a$rank <- c(1:17)
b <- data.frame (predictors (lmProfile_lm))
b$rank <- c(1:14)
c <- data.frame (predictors (lmProfile_tb))
c$rank <- c(1:11)
d <- data.frame (predictors (lmProfile_ct))
d$rank <- c(1:15)
Table <- merge (Table, a, by = 'rank', all = TRUE)
Table <- merge (Table, b, by = 'rank', all = TRUE)
Table <- merge (Table, c, by = 'rank', all = TRUE)
Table <- merge (Table, d, by = 'rank', all = TRUE)
## (This provides us with a visualization of the important predictors according to the various algorithms)




# ------------------------------------------------------------------
# 4.MODEL TRAINING AND TUNING: USING THE TRAINING DATASET
# -------------------------------------------------------------------

# Training the model using the 'earth' algorithm (MARS model)
## (List of names of the various models present in caret )
###names (getModelInfo())
## (Getting the information of individual models/algorithms)
###modelLookup("earth")
## (Training the model using the 'earth' algorithm
## (which is a Multivariate Adaptive Regression Splines (MARS) model)
###model_mars = train (CFU ~., data=TrainData, method='earth')
###fitted <- predict(model_mars)
###model_mars
## (Plotting the model shows how the various iterations of hyperparameter search performed)
###varimp_mars <- varImp (model_mars)
###plot(model_mars, main="Model Accuracies with MARS")

# Computing variable importance in the MARS model
###varimp_mars <- varImp(model_mars, useModel = FALSE, nonpara = FALSE, scale = TRUE)
###plot(varimp_mars, main="Variable Importance with MARS")

# Training with other models
## Training the model using Random Forest
###modelLookup('rf')
###model_rf = train(CFU ~ ., data=TrainData, method='rf')
###model_rf
## Training the model using svmRadial
###modelLookup('svmRadial')
###model_svmRadial = train(CFU ~ ., data=TrainData, method='svmRadial')
###model_svmRadial
## Compare model performances using resample()
###models_compare <- resamples(list(RF=model_rf, MARS=model_mars, SVM=model_svmRadial))
# Summary of the models performances
###summary(models_compare)
## Draw box plots to compare models
###scales <- list(x=list(relation="free"), y=list(relation="free"))
###bwplot(models_compare, scales=scales)

# List of algorithms categorized by Dharma for future use (if req)
## (Algorithm List where type = regression only)
##algorithmList1 <- c('ANFIS', 'brnn', 'bridge', 'blassoAveraged', 'cubist',
##                    'DENFIS', 'enet', 'FIR.DM', 'GFS.FR.MOGUL', 'GFS.THRIFT',
##                    'GFS.LT.RS', 'HYFIS', 'icr', 'lars', 'lars2', 'lm',
##                    'leapBackward', 'leapForward', 'leapSeq', 'lmStepAIC',
##                    'M5Rules', 'M5', 'glm.nb', 'neuralnet', 'rqnc', 'nnls',
##                    'penalized', 'krlsPoly', 'pcr', 'ppr', 'qrf', 'qrnn',
##                    'rqlasso', 'krlsRadial', 'relaxo', 'rvmLinear', 'rvmPoly',
##                    'rvmRadial', 'ridge', 'foba', 'rlm', 'FS.HGD', 'spikeslab',
##                    'SBC', 'superpc', 'blasso', 'lasso', 'WM')
## (Algorithm List where type = regression & classification)
##algorithmList2 <- c('treebag', 'logicBag', 'bagEarth', 'bagEarthGCV', 'bag', 'bartMachine',
##                   'bayesglm', 'gamboost', 'glmboost', 'BstLm', 'bstSm', '	blackboost',
##                   'bstTree', 'rpart', 'rpart1SE', 'rpart2', 'cforest', 'ctree', 'ctree2',
##                   'randomGLM', 'xgbDART', 'xgbLinear', 'xgbTree', 'elm', 'gaussprLinear', 
##                  'gaussprPoly', 'gaussprRadial', 'gamLoess', 'bam', 'gam', 'gamSpline', 
##                  'glm', 'glmStepAIC', 'glmnet', 'glmnet_h2o', 'gbm_h2o', 'kknn', 'knn',
##                  'svmLinear3', '	logreg', 'avNNet', 'monmlp', 'mlp', 'mlpWeightDecay',
##                  'mlpWeightDecayML', 'mlpML', 'msaenet', 'mlpSGD', 'mlpKerasDropout',
##                   'mlpKerasDecay', 'earth', 'gcvEarth', 'mxnet', 'mxnetAdam', 'nnet',
##                  'pcaNNet', 'null', 'parRF', 'partDSA', 'kernelpls', 'pls', 'simpls',
##                  'widekernelpls', 'plsRglm', 'rbf', 'rbfDDA', 'ranger', 'Rborist', 
##                  'rf', 'extraTrees', 'rfRules', 'RRF', 'RRFglobal', 'xyf', 'spls', 
##                  'dnn', 'gbm', 'svmBoundrangeString', 'svmExpoString', 'svmLinear',
##                  'svmLinear2', 'svmPoly', 'svmRadial', 'svmRadialCost', 'svmRadialSigma',
##                  'svmSpectrumString', 'evtree', 'nodeHarvest')




# Comparing various ML models
library(caretEnsemble)
## Set the seed for reproducibility
set.seed(100)
## Creating an universal traincontrol function
trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)
## Stacking Algorithms - creating groups of algorithms to run together
Algo_GLM <- c('bayesglm', 'glm', 'glmStepAIC')
Algo_boosting <- c('xgbDART', 'xgbDART', 'xgbLinear', 'xgbTree')
Algo_SVM <- c('svmLinear', 'svmRadial')
Algo_neuralnetworks <- c('avNNet', 'nnet', 'brnn')
Algo_others_RfMarsPlsLr <- c('rf', 'earth', 'widekernelpls', 'enet' )
## Runing multiple algorithms/models in combined calls.
models_GLM <- caretList(CFU ~ ., data=TrainData, trControl=trainControl, methodList=Algo_GLM) 
models_boosting <- caretList(CFU ~ ., data=TrainData, trControl=trainControl, methodList=Algo_boosting)
models_SVM <- caretList(CFU ~ ., data=TrainData, trControl=trainControl, methodList=Algo_SVM) 
models_neuralnetworks <- caretList(CFU ~ ., data=TrainData, trControl=trainControl, methodList=Algo_neuralnetworks) 
models_others_RfMarsPlsLr <- caretList(CFU ~ ., data=TrainData, trControl=trainControl, methodList=Algo_others_RfMarsPlsLr) 
## Obtaining the results of the model sets
results_GLM <- resamples(models_GLM)
results_boosting <- resamples(models_boosting)
results_SVM <- resamples(models_SVM)
results_neuralnetworks <- resamples(models_neuralnetworks)
results_others_RfMarsPlsLr <- resamples(models_others_RfMarsPlsLr)
## Summarizing the results of the model sets
summary(results_GLM)
summary(results_boosting)
summary(results_SVM)
summary(results_neuralnetworks)
summary(results_others_RfMarsPlsLr)
## Box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
pdf("model_RMSE.pdf")
bwplot(results_GLM, scales=scales)
bwplot(results_boosting, scales=scales)
bwplot(results_SVM, scales=scales)
bwplot(results_neuralnetworks, scales=scales)
bwplot(results_others_RfMarsPlsLr, scales=scales)
dev.off()
## [Thus, based on the RMSE scores, the best models are svmRadial and rf]


# Predicting on the Test Data Set Using the selected models (req for classification problems)
##model_svmRadial = train(CFU ~ ., data=TrainData, method ='svmRadial')
##model_rf = train(CFU ~ ., data=TrainData, method ='rf')
##predicted_svmRadial <- predict(model_svmRadial, TestData)
##predicted_rf <- predict(model_rf, TestData)
##predicted_svmRadial
##predicted_rf
# Confusion matrix (req for classification problems)
##confusionMatrix(reference = TestData$CFU, data = predicted_rf, mode='everything', positive='MM')



# Optimizing the svmRadial model by tuning the model hyperparameters






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