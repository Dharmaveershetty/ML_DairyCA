# *DAIRY AGROECOSYSTEMS: REGRESSION ANALYSIS* 

# Libraries
library(readxl)
library (tidyverse)
library (car)
library(MASS)

# Importing dataset
masterdata <- read_excel("masterdata.xlsx")
# Converting Date values to Date format
masterdata$Date <- as.Date(masterdata$Date, format = "%Y-%m-%d" )
# Renaming NO3
masterdata <- masterdata %>% rename (N3 = NO3)
# Creating a concise dataset restricted to sample averages
masterdata_av <- masterdata [, c(1:7,11,15,19,23,27,31,35:37,42)]
# Creating a final dataset where the samples/rows where there is NA is deleted
masterdata_final <- masterdata_av [-c(25:28, 39, 89:92),]  
# Rechecked to see if all the missing data rows have been deleted
masterdata_final[!complete.cases(masterdata_final),]


# STEP 1: VARIABLE SELECTION FOR THE LINEAR MODELS

# (A) Summary stats and ANOVA (Refer Descriptive_code.R file)
      # The important continuous variables that were significant in treatment/stage/season comparisons were 
      # CFU + K + TS + Na + N3
# (B) Stepwise Regression for the TS linear regression model 
      # Stepwise regression
      Fit_TS <- lm (TS ~ Treatment + Stage + Season + Salt + Ca + N3 + Na + K + pH + conduc + VS + CFU, 
                 data = masterdata_final)
      step <- stepAIC (Fit_TS, direction = "both")
      step$anova                                           # display results 
      # The selected predictor variables for the TS model are: 
      # TS ~ Treatment + Stage + Season
# (C) Stepwise Regression for the CFU linear regression model 
      # Stepwise regression
      Fit_CFU <- lm (CFU ~ Treatment + Stage + Season + Salt + Ca + N3 + Na + K + pH + conduc + VS + TS, 
                 data = masterdata_final)
      step <- stepAIC (Fit_CFU, direction = "both")
      step$anova                                           # display results 
      # The selected predictor variables for the TS model are: 
      # CFU ~ Treatment + Stage + Season + pH + VS


# STEP 2: FITTING THE MODEL
      
# Multiple Linear Regression
Fit_TS <- lm (TS ~ Treatment + Stage + Season, data = masterdata_final)
Fit_CFU <- lm (CFU ~ Treatment + Stage + Season + pH + VS, data = masterdata_final)
summary (Fit_TS)
summary (Fit_CFU)

# Other functions to describe the linear model
coefficients(Fit_TS)               # model coefficients
confint(Fit_TS, level=0.95)        # CIs for model parameters
fitted(Fit_TS)                     # predicted values
residuals(Fit_TS)                  # residuals
anova(Fit_TS)                      # anova table
vcov(Fit_TS)                       # covariance matrix for model parameters
influence(Fit_TS)                  # regression diagnostics 


# STEP 3: REGRESSION DIAGNOSTICS FOR OUTLIERS

# Assessing Outliers
outlierTest(Fit_TS)
qqPlot (Fit_TS, main = "TS: QQ Plot")
leveragePlots (Fit_TS, main = "TS: Leverage Plot")
# Outliers identified (that are influencial) = row no. 42

outlierTest(Fit_CFU)
qqPlot (Fit_CFU, main = "CFU: QQ Plot")
leveragePlots (Fit_CFU, main = "CFU: Leverage Plot")
# Outliers identified (that are influencial) = row no. 55 and 52

# Influencial Observations
# Added-Variable plots
avPlots(Fit_TS, main = "TS: Added-Variable Plot")
avPlots(Fit_CFU, main = "CFU: Added-Variable Plot")
# Cook's D plot
# Identify D values > 4/(n-k-1)
cutoff_TS <- 4/((nrow(masterdata_av)-length(Fit_TS$coefficients)-2))
cutoff_CFU <- 4/((nrow(masterdata_av)-length(Fit_CFU$coefficients)-2))
plot(Fit_TS, which=4, cook.levels=cutoff_TS, main = "TS: Cooks D-value Plot")
plot(Fit_CFU, which=4, cook.levels=cutoff_CFU, main = "CFU: Cooks D-value Plot")
# Influence Plot
influencePlot(Fit_TS, id.method="identify", 
              main="Influence Plot for TS", 
              sub="Circle size is proportial to Cook's Distance")
influencePlot(Fit_CFU, id.method="identify", 
              main="Influence Plot for CFU", 
              sub="Circle size is proportial to Cook's Distance")

# Non-normality
# Normality of Residuals
# QQ plot for Studentized residuals
qqPlot (Fit_TS, main="TS: QQ Plot")
qqPlot (Fit_CFU, main="CFU: QQ Plot")
# Distribution of Studentized residuals
sresid_TS <- studres(Fit_TS)
hist(sresid_TS, freq=FALSE,
     main="TS: Distribution of Studentized Residuals")
xfit_TS<-seq(min(sresid_TS),max(sresid_TS),length=40)
yfit_TS<-dnorm(xfit_TS)
lines(xfit_TS, yfit_TS) 
sresid_CFU <- studres(Fit_CFU)
hist(sresid_CFU, freq=FALSE,
     main="CFU: Distribution of Studentized Residuals")
xfit_CFU<-seq(min(sresid_CFU),max(sresid_CFU),length=40)
yfit_CFU<-dnorm(xfit_CFU)
lines(xfit_CFU, yfit_CFU) 

# Non-constant Error Variance
# Evaluate homoscedasticity
# Non-constant error variance test
ncvTest(Fit_TS)
ncvTest(Fit_CFU)
# Plot Studentized residuals vs. fitted values
spreadLevelPlot(Fit_TS)
spreadLevelPlot(Fit_CFU)

# Evaluate Multi-Collinearity
vif(Fit_TS)                   # variance inflation factors
sqrt(vif(Fit_TS)) > 2         
vif(Fit_CFU)                  # variance inflation factors
sqrt(vif(Fit_CFU)) > 2        

# Evaluate Nonlinearity
# Component + residual plot
crPlots(Fit_TS)
crPlots(Fit_CFU)
# Ceres plots
ceresPlots(Fit_TS)
ceresPlots (Fit_CFU)

# Non-independence of Errors
# Test for Autocorrelated Errors
durbinWatsonTest(Fit_TS)
durbinWatsonTest(Fit_CFU)



