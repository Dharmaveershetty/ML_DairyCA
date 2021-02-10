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

install.packages(c('caret', 'skimr'))
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
masterdata <- masterdata %>% rename (Conductivity = conduc, 
                                     Total_Solids = TS, 
                                     Volatile_Solids = VS, 
                                     Sodium = Na, 
                                     Potassium = K, 
                                     Calcium = Ca, 
                                     Nitrates = NO3)


# Descriptive & Univariate Statistics using the Skimr package
##library (skimr)
##skim_descriptive <- masterdata %>% skim () %>% rename (variable_type = skim_type, variable = skim_variable)
##library(xlsx)
##write.xlsx(skim_descriptive, "desc.xlsx") 

# Descriptive and Univariate Statistics categorized by Treatment, using the Arsenal package
##library(arsenal) 
##descriptive_group <- tableby(Treatment ~ ., data = masterdata, 
##                             test = T, total = T, 
##                             numeric.test = "anova", cat.test = "chisq",
##                             numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
##                             cat.stats = c("countpct", "Nmiss2"),
##                             stats.labels = list(
##                               meansd = "Mean (SD)",
##                               medianq1q3 = "Median (Q1, Q3)",
##                               range = "Min - Max",
##                               Nmiss2 = "Missing"))
##summary(descriptive_group, title = "Descriptive Statistics")
##write2word(descriptive_group, 
##           keep.md = FALSE,
##           "~/Desktop/descriptive.doc")


# Descriptive and Univariate Statistics categorized by Treatment and Stage, using the Arsenal package
# Summary Statistics & ANOVA
# Creating tableby controls variable (Summary stats & ANOVA)
library (arsenal)
tcontrols <- tableby.control(
  test = TRUE,
  total = TRUE,
  numeric.test = "anova",
  numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    range = "Min - Max",
    Nmiss2 = "Missing"))
# Creating tables
table1 <- tableby (Treatment ~., data = masterdata, control = tcontrols)
table2 <- tableby (Stage ~., data = masterdata, control = tcontrols)
table3 <- tableby (Season ~., data = masterdata, control = tcontrols)
table4 <- tableby (interaction (Treatment, Stage) ~., data = masterdata, control = tcontrols)
write2html(table1, "table1.html", 
           title = "TABLE 1: COMPARING FARM TREATMENTS: ANALYSIS OF VARIANCE (ANOVA) & DESCRIPTIVE STATS")
write2html(table2, "table2.html", 
           title = "TABLE 2: COMPARING STAGES: ANALYSIS OF VARIANCE (ANOVA) & DESCRIPTIVE STATS")
write2html(table3, "table3.html",
           title = "TABLE 3: COMPARING SEASONS: ANALYSIS OF VARIANCE (ANOVA) & DESCRIPTIVE STATS")
write2html(table4, "table4.html",
           title = "TABLE 4: COMPARING TREATMENTS & STAGES: ANALYSIS OF VARIANCE (ANOVA) & DESCRIPTIVE STATS")
table1 <- as.data.frame(table1)
table4 <- as.data.frame(table4)



# Descriptive statistics by group using the Skimr package
##descriptive_gp <- masterdata %>% group_by (Treatment, Season) %>% skim () 
##descriptive_gp
##write2html(descriptive_gp, 
           ##keep.md = FALSE,  
           ##"~/descriptive.doc")

# Descriptive statistics by group using the Arsenal package
##library(arsenal) 
##descriptive_group <- tableby(interaction (Treatment, Season) ~ ., data = masterdata, 
##                             test = T, total = T, 
##                             numeric.test = "anova", cat.test = "chisq",
##                             numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
##                             cat.stats = c("countpct", "Nmiss2"),
##                             stats.labels = list(
##                                  meansd = "Mean (SD)",
##                                  medianq1q3 = "Median (Q1, Q3)",
##                                  range = "Min - Max",
##                                  Nmiss2 = "Missing"))
##summary(descriptive_group, title = "Descriptive Statistics")
##write2word(descriptive_group, 
##           keep.md = FALSE,
##           "~/Desktop/descriptive.doc")


# Visualizing model variable correlations
library (corrplot)
masterdata_num <- masterdata [, c(4:13)]
masterdata_num <- na.omit (masterdata_num)
anyNA (masterdata_num)
masterdata_num <- masterdata_num %>% rename (EC = Conductivity, 
                                     TS = Total_Solids, 
                                     VS = Volatile_Solids, 
                                     Na = Sodium, 
                                     K = Potassium, 
                                     Ca = Calcium, 
                                     NO3 = Nitrates)
correlations = cor(masterdata_num)
col3 <- colorRampPalette(c("brightred", "white", "blue")) 
correlogram <- corrplot(correlations, type = "lower", tl.srt = 1, tl.pos = "d", tl.col = "black",
                        outline = TRUE, order = "hclust", col = col3(10))

# Visualizing the relationship of individual variables to the outcome
##Continuous Variables
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = masterdata[,4:12],
            y = masterdata$CFU,
            plot = "scatter",
            type = c("p", "smooth"), 
            labels = c("Predictors", "Bacterial Count (CFU)"))


##Categorical Variables
library (Rmisc)
bp1 <- ggplot (masterdata, aes (x = Treatment, y = CFU)) + 
  geom_boxplot()
bp2 <- ggplot (masterdata, aes (x = Season, y = CFU)) + 
  geom_boxplot()
bp3 <- ggplot (masterdata, aes (x = Stage, y = CFU)) + 
  geom_boxplot()
multiplot (bp1, bp2, bp3, layout = matrix (c(1,2,3,3), nrow=2, byrow=TRUE))





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
## Finding variables that are correlated above 0.75
highCorr <- sum(abs(masterdata_cor[upper.tri(masterdata_cor)]) > .75)
highCorr
## [There are no variables that are correlated > 0.999, and thus, no variables need to be deleted]
## Removing highly correlated variables above 0.75 if the correlated variables had been found
  ## summary (masterdata_cor [upper.tri(masterdata_cor)])
  ## highlyCorMasterdata <- findCorrelation(masterdata_cor, cutoff = .75)
  ## masterdata_filtered <- masterdata [, -highlyCorMasterdata]
  ## masterdata_cor_2 <- cor (masterdata_filtered)
  ## summary (masterdata_cor_2 [upper.tri(masterdata_cor_2)]) 


# Finding linear dependancies 
comboInfo <- findLinearCombos(masterdata_num)
comboInfo




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
## (Select algorithms from various algorithm classes)
##Algo_GLM <- c('bayesglm', 'glm', 'glmStepAIC')
##Algo_boosting <- c('xgbDART', 'xgbLinear', 'xgbTree')
##Algo_SVM <- c('svmRadial','svmLinear','svmLinear2','svmLinear3','svmPoly','svmRadialCost','svmRadialSigma')
##Algo_neuralnetworks <- c('avNNet', 'nnet', 'brnn')
##Algo_rf <- c('rf','cforest','parRF','qrf','ranger','Rborist','rfRules','RRF','RRFglobal')
##Algo_casewts <- c('treebag', 'bayesglm')
##Algo_bagging <- c('cforest')
##Algo_bayesian <- c('spikeslab')
##Algo_binarypredictors <- c()
##Algo_costsens <- c()
##Algo_discriminant <- c()
##Algo_distwtdiscrimination <- c()
##Algo_ensemble <- c()
##Algo_featureextctn <- c('icr', 'pcaNNet', 'simpls', 'pcr', 'ppr', 'superpc')
##Algo_featureselnwrap <- c('leapBackward', 'leapForward', 'leapSeq', 'stepQDA', 'foba')
##Algo_gaussian <- c('gaussprLinear', 'gaussprPoly')
##Algo_generalizedadditive <- c('gamboost', 'gamLoess', 'gam')
##Algo_generalizedlinear <- c('bayesglm', 'randomGLM', 'glm', 'msaenet', 'glm.nb')
##Algo_missingdata <- c()
##Algo_implicitfeatureseln <- c('bartMachine', 'BstLm', 'rpart', 'cforest', 'ctree2')
##Algo_kernel <- c('gaussprLinear', 'gaussprRadial')
##Algo_L1regularization <- c('blassoAveraged', 'glmnet_h2o')
##Algo_L2regularization <- c('bridge', 'mlpWeightDecay', 'mlpWeightDecayML')
##Algo_Linearclassifier <- c('glmboost', 'randomGLM')
##Algo_Linearregression <- c('BstLm', 'cubist', 'icr', 'lars2', 'lm')
##Algo_Logicregression <- c('logreg', 'logreg')
##Algo_MARS <- c('earth', 'gcvEarth')
##Algo_Mixturemodel <- c()
##Algo_ModelTree <- c('M5Rules', 'M5')
##Algo_Obliquetree <- c()
##Algo_PLS <- c('kernelpls', 'pls')
##Algo_PatientRIM <- c()
##Algo_Polynomial <- c('krlsPoly', 'rvmPoly')
##Algo_Prototypemodels <- c('kknn','knn')
##Algo_Quantileregression <- c('rqnc', 'qrf', 'qrnn', 'rqlasso')
##Algo_Radialbasisfn <- c('rbf', 'rbfDDA', 'rvmRadial', 'svmRadialCost')
##Algo_Regularization <- c('RRFglobal')
##Algo_RVM <- c('rvmLinear', 'rvmPoly')
##Algo_Ridgeregression <- c()
##Algo_Robustmethods <- c('svmRadialSigma')
##Algo_Robustmodels <- c('qrnn', 'rlm')
##Algo_Rulebased <- c('ANFIS', 'DENFIS', 'FIR.DM', 'GFS.FR.MOGUL', 'GFS.THRIFT')
##Algo_SOMaps <- c('xyf')
##Algo_Stringkernels <- c()
##Algo_SCP <- c('blackboost', 'rpart1SE')
##Algo_Textmining <- c()
##Algo_Treebasedmodel <- c('bstTree')
##Algo_2class <- c('ORFlog', 'nodeHarvest')

# Comparing various Machine Learning models in order to select a few models
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
Algorithms <- c('bayesglm', 'glm', 'glmStepAIC',
                'xgbDART', 'xgbLinear', 'xgbTree',
                'svmRadial','svmLinear','svmLinear2','svmLinear3','svmPoly','svmRadialCost','svmRadialSigma',
                'avNNet', 'nnet', 'brnn',
                'rf','cforest','parRF','qrf','ranger','Rborist','rfRules','RRF','RRFglobal',
                'earth', 'widekernelpls', 'enet')
## Runing multiple algorithms/models in a combined call.            
Models <- caretList(CFU ~ ., data=TrainData, trControl=trainControl, methodList=Algorithms) 
## Obtaining the results of the model sets
Results_models <- resamples(Models)
## Summarizing the results of the model sets
summary (Results_models)
## Box plots to compare models using the bw plot function
scales <- list(x=list(relation="free"), y=list(relation="free"))
bw_models <- bwplot(Results_models, scales=scales)
## Box plots to compare models using the ggplot2 plot function (which gives us more control but is tougher to perform)
library(tidyverse)
Results_models$values %>%                   #extract the values
  select(1, ends_with("RMSE")) %>%          #select the first column and all columns with a name ending with "RMSE"
  gather(model, RMSE, -1) %>%               #convert to long table
  mutate(model = sub("~RMSE", "", model)) %>%              #leave just the model names
  ggplot()+                                                #call ggplot
  geom_boxplot(aes(x = RMSE, y = model)) -> p1             #and plot the box plot
Results_models$values %>%                  
  select(1, ends_with("Rsquared")) %>%          
  gather(model, Rsquared, -1) %>%               
  mutate(model = sub("~Rsquared", "", model)) %>%              
  ggplot()+                                                
  geom_boxplot(aes(x = Rsquared, y = model)) -> p2
library (Rmisc)
multiplot(p1, p2, cols = 2)


## [Thus, based on the RMSE scores, the best models are svmRadialSigma and qrf]



# Running the second best Model = qrf, in more detail
set.seed(100)
## Looking up the parameters present in the model
modelLookup("qrf")
## Training the model
model_qrf = train (CFU ~., data=TrainData, method='qrf')
## Visualizing the model parameters chosen
model_qrf
plot (model_qrf)
## Hypertuning the parameters of the model using the previously used TrainControl function
set.seed(100)
model_qrf = train(CFU ~ ., data=TrainData, method='qrf', tuneLength = 5, metric='rmse', trControl = trainControl)
model_qrf
plot (model_qrf)
## Fitting the trained model to the Training Data to predict the outcome (only for visualization since it is done automatically in the model)
fitted_qrf <- predict (model_qrf)
fitted_qrf
## Computing the importance of each variable in the model
varimp_qrf <- varImp(model_qrf, useModel = FALSE, nonpara = FALSE, scale = TRUE)
plot(varimp_qrf, main="Variable Importance with qrf")



# Running the first best Model = svmRadialSigma, in more detail
set.seed(100)
## Looking up the parameters present in the model
modelLookup("svmRadialSigma")
## Training the model
model_svmRadialSigma = train (CFU ~., data=TrainData, method='svmRadialSigma')
## Visualizing the model parameters chosen
model_svmRadialSigma
plot (model_svmRadialSigma)
## Creating an universal traincontrol function
trainControl <- trainControl(method="cv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)
## Hypertuning the parameters of the model 
set.seed(100)
model_svmRadialSigma = train(CFU ~ ., data=TrainData, method='svmRadialSigma', tuneLength = 5, metric='rmse', trControl = trainControl)
model_svmRadialSigma
plot (model_svmRadialSigma)
## Fitting the trained model to the Training Data to predict the outcome (only for visualization since it is done automatically in the model)
fitted_svmRadialSigma <- predict (model_svmRadialSigma)
fitted_svmRadialSigma
## Computing the importance of each variable in the model
varimp_svmRadialSigma <- varImp(model_svmRadialSigma, useModel = FALSE, nonpara = FALSE, scale = TRUE)
plot(varimp_svmRadialSigma, main="Variable Importance with SVMRadialSigma")






# ------------------------------------------------------------------
# 5.MODEL TESTING: USING THE TEST DATASET
# -------------------------------------------------------------------


# qrf Model: Predicting the outcome in the Test dataset using the trained model
predicted_qrf <- predict(model_qrf, TestData)
predicted_qrf
# Measures of regression between the actual outcome and the predicted outcome in the Test Dataset
postResample(pred = predicted_qrf, obs = TestData$CFU)


# svmRadialSigma Model: Predicting the outcome in the Test dataset using the trained model
predicted_svmRadialSigma <- predict(model_svmRadialSigma, TestData)
predicted_svmRadialSigma
# Measures of regression between the actual outcome and the predicted outcome in the Test Dataset
postResample(pred = predicted_svmRadialSigma, obs = TestData$CFU)





# Combing the predictions of multiple models to form a final prediction
## Create the trainControl
stackControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)
## Ensemble the predictions of `models` to form a new combined prediction based on glm
stack.glm <- caretStack(Models, method="glm", metric="rmse", trControl=stackControl)
print(stack.glm)
# Predict on testData
stack_predicteds <- predict(stack.glm, newdata=TestData)
stack_predicteds
# Measures of regression between the actual outcome and the predicted outcome in the Test Dataset
postResample(pred = stack_predicteds, obs = TestData$CFU)

# Computing variable importance in the Combined Prediction based glm model
varimp_combined <- varImp(stack_predicteds, useModel = FALSE, nonpara = FALSE, scale = TRUE)
plot(varimp_combined, main="Variable Importance with MARS")