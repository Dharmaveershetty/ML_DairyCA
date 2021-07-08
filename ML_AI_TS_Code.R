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
# *Reference Tutorial: https://www.r-project.org/conferences/useR-2013/Tutorials/kuhn/user_caret_2up.pdf
# *Reference (normality): https://statsandr.com/blog/do-my-data-follow-a-normal-distribution-a-note-on-the-most-widely-used-distribution-and-how-to-test-for-normality-in-r/#how-to-test-the-normality-assumption
# *Reference (transformation): https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/
# *Reference (outliers): https://statsandr.com/blog/outliers-detection-in-r/
# *Reference (model performance improvement): https://link.springer.com/chapter/10.1007/978-1-4842-2334-5_8
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
masterdata <- as.data.frame (masterdata1)


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
                                     Nitrates = NO3,
                                     E.coli = CFU) %>% mutate (Stage = recode (Stage, 
                                                                               A = "S1",
                                                                               B = "S2",
                                                                               C = "S3",
                                                                               D = "S4"))

# Creating masterdata_num that is restricted to continuous variables
masterdata_num <- masterdata [, c(4:13)]


# (A) Descriptive Statistics
# --------------------------

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



# (B) Visualizing Distribution, Skewness, and Outliers for all continuous variables
# ---------------------------------------------------------------------------------

# (B1) The Normality assumption can be tested by (a) histograms, (b) density plots, (c) QQ plots,  
       # and (d) normality tests like Shapiro-Wilk’s test and Kolmogorov-Smirnov tests.  
       # Histograms and Density plots also help visualize Skewness
library (ggpubr)

# Visualizing Histogram for E.coli
ggplot (data = masterdata, aes (x = E.coli)) + geom_histogram()
# Visualizing Density Plot for E.coli
ggplot (data = masterdata, aes (x = E.coli)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
## Not normally distributed; positively skewed


# Visualizing Density Plots for all the variables
# Changing (melting) the data from wide form to the long form
library (reshape2)
masterdata_long <- melt (masterdata_num)
# Visualizing Density Plots for all the variables
ggplot (data = masterdata_long, aes (x = value)) + stat_density() + facet_wrap (~variable, scales = "free")
ggplot (data = masterdata_long, aes (x = value)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed") + 
  facet_wrap (~variable, scales = "free")


# Visualizing QQ Plots for all variables
ggqqplot(masterdata$Salt, title = "SALT")
ggqqplot(masterdata$Calcium, title = "CALCIUM")
ggqqplot(masterdata$Nitrates, title = "NITRATES")
ggqqplot(masterdata$Sodium, title = "SODIUM")
ggqqplot(masterdata$Potassium, title = "POTASSIUM")
ggqqplot(masterdata$pH, title = "pH")
ggqqplot(masterdata$Conductivity, title = "EC")
ggqqplot(masterdata$Total_Solids, title = "TS")
ggqqplot(masterdata$Volatile_Solids, title = "VS")
ggqqplot(masterdata$E.coli, title = "E.coli")


# Normality Tests (shapiro test, Ho: Follows normal distribution, Ha: Does not follow normal distribution)
shapiro.test(masterdata$Salt)
shapiro.test(masterdata$Calcium)
shapiro.test(masterdata$Nitrates)
shapiro.test(masterdata$Sodium)
shapiro.test(masterdata$Potassium)
shapiro.test(masterdata$pH)
shapiro.test(masterdata$Conductivity)
shapiro.test(masterdata$Total_Solids)
shapiro.test(masterdata$Volatile_Solids)
shapiro.test(masterdata$E.coli)

# Skewness
library (moments)
skewness(masterdata$Salt, na.rm = TRUE)
skewness(masterdata$Calcium, na.rm = TRUE)
skewness(masterdata$Nitrates, na.rm = TRUE)
skewness(masterdata$Sodium, na.rm = TRUE)
skewness(masterdata$Potassium, na.rm = TRUE)
skewness(masterdata$pH, na.rm = TRUE)
skewness(masterdata$Conductivity, na.rm = TRUE)
skewness(masterdata$Total_Solids, na.rm = TRUE)
skewness(masterdata$Volatile_Solids, na.rm = TRUE)
skewness(masterdata$E.coli, na.rm = TRUE)


# (B2) The outliers can be visualized by using boxplots
boxplot(masterdata_num)


# Conclusion
# Normally Distributed: None of the variables are normally distributed. 
  # Ca, EC, and NO3 are the variables closest to being normally distributed 
# Skewness: Positively skewed: E.coli, salt, Na, TS, pH
# Outliers: Present in multiple variables



# (D) Variable Correlations
# --------------------------

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
col3 <- colorRampPalette(c("red", "white", "blue")) 
correlogram <- corrplot(correlations, type = "lower", 
                        tl.srt = 1, tl.pos = "d", tl.col = "black", tl.cex = 1.3, 
                        outline = TRUE, order = "hclust", col = col3(10))


# (E) Visualizing the relationship of individual variables to the outcome
# -----------------------------------------------------------------------

##Continuous Variables
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = masterdata[,4:12],
            y = masterdata$E.coli,
            plot = "scatter",
            type = c("p", "smooth"), 
            labels = c("Predictors", "Bacterial Count (CFU)"))


##Continuous Variables (for publication purposes)
library (ggplot2)
##Positive 
p1 <- ggplot(data=masterdata, mapping = aes(x = Potassium, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "blue")
p2 <- ggplot(data=masterdata, mapping = aes(x = Nitrates, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "blue")
p3 <- ggplot(data=masterdata, mapping = aes(x = Calcium, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "blue")
p4 <- ggplot(data=masterdata, mapping = aes(x = Sodium, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "blue")
##Negative 
p5 <- ggplot(data=masterdata, mapping = aes(x = Salt, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "red")
p6 <- ggplot(data=masterdata, mapping = aes(x = Total_Solids, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "red") + 
  scale_y_continuous(limits = c(0,10000))
p7 <- ggplot(data=masterdata, mapping = aes(x = pH, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "red")
p8 <- ggplot(data=masterdata, mapping = aes(x = Conductivity, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "red")
p9 <- ggplot(data=masterdata, mapping = aes(x = Volatile_Solids, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "red")

##Categorical Variables (for publication purposes)
##Box and Whiskers Plot
bp1 <- ggplot (masterdata, aes (x = Treatment, y = E.coli)) + 
  geom_boxplot()
bp2 <- ggplot (masterdata, aes (x = Season, y = E.coli)) + 
  geom_boxplot()
bp3 <- ggplot (masterdata, aes (x = Stage, y = E.coli)) + 
  geom_boxplot()
##Creating a multiple plot 
library (Rmisc)
multiplot (p1,p2,p3,p4,p5,p6,p7,p8,p9,bp1, bp2, bp3, cols = 3)
           

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
## [There are no variables that are correlated > 0.999 or >0.75, and thus, no variables need to be deleted]
## Removing highly correlated variables above 0.75 if the correlated variables had been found
  ## summary (masterdata_cor [upper.tri(masterdata_cor)])
  ## highlyCorMasterdata <- findCorrelation(masterdata_cor, cutoff = .75)
  ## masterdata_filtered <- masterdata [, -highlyCorMasterdata]
  ## masterdata_cor_2 <- cor (masterdata_filtered)
  ## summary (masterdata_cor_2 [upper.tri(masterdata_cor_2)]) 


# Finding linear dependancies 
comboInfo <- findLinearCombos(masterdata_num)
comboInfo



# Transforming variables: normalizing, removing outliers, linearity, etc
# --------------------------------------------------------------------------
library(bestNormalize)
library(ggpubr)
library (moments)

# E.coli
## Histogram
ggplot (data = masterdata, aes (x = E.coli)) + geom_histogram()
## Density Distribution
ggplot (data = masterdata, aes (x = E.coli)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
## QQ-Plot
ggqqplot(masterdata$E.coli, title = "E.coli")
## Normality Test
shapiro.test(masterdata$E.coli)
## Skewness
skewness (masterdata$E.coli, na.rm = TRUE)
## Outliers
boxplot(masterdata$E.coli)
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$E.coli))
## BestNormalize Transformation
BNobject <- bestNormalize::bestNormalize(masterdata$E.coli)
BNobject #Ordernorm transformation was chosen as the best transformation
masterdata$E.coli_ON <- predict(BNobject$chosen_transform) 
## Checking if the transformation worked
ggplot (data = masterdata, aes (x = E.coli_ON)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggqqplot(masterdata$E.coli_ON, title = "E.coli")
shapiro.test(masterdata$E.coli_ON)
skewness (masterdata$E.coli_ON, na.rm = TRUE)
boxplot(masterdata$E.coli_ON) # There appears to be one outlier at the lower level
## Rosners Test to stastistically detect outliers
library(EnvStats)
rosnerTest((masterdata$E.coli_ON)) #There are no statistically significant outliers
## OrderNorm Transformed

# Salt
## Histogram
ggplot (data = masterdata, aes (x = Salt)) + geom_histogram()
## Density Distribution
ggplot (data = masterdata, aes (x = Salt)) + stat_density() 
ggplot (data = masterdata, aes (x = Salt)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
## QQ-Plot
ggqqplot(masterdata$Salt, title = "Salt")
## Normality Test
shapiro.test(masterdata$Salt)
## Skewness
skewness (masterdata$Salt, na.rm = TRUE)
## Outliers
boxplot(masterdata$Salt)
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$Salt))
## BestNormalize Transformation
BNobject <- bestNormalize::bestNormalize(masterdata$Salt)
BNobject #Ordernorm transformation was chosen as the best transformation
masterdata$Salt_ON <- predict(BNobject$chosen_transform) 
## Checking if the transformation worked
ggplot (data = masterdata, aes (x = Salt_ON)) + stat_density()
ggplot (data = masterdata, aes (x = Salt_ON)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggqqplot(masterdata$Salt_ON, title = "Salt")
shapiro.test(masterdata$Salt_ON)
skewness (masterdata$Salt_ON, na.rm = TRUE)
boxplot(masterdata$Salt_ON) # There appears to be no outliers
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$Salt_ON)) #There are no statistically significant outliers
## OrderNorm Transformed

# Calcium
## Histogram
ggplot (data = masterdata, aes (x = Calcium)) + geom_histogram()
## Density Distribution
ggplot (data = masterdata, aes (x = Calcium)) + stat_density() 
ggplot (data = masterdata, aes (x = Calcium)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
## QQ-Plot
ggqqplot(masterdata$Calcium, title = "Calcium")
## Normality Test
shapiro.test(masterdata$Calcium)
## Skewness
skewness (masterdata$Calcium, na.rm = TRUE)
## Outliers
boxplot(masterdata$Calcium)
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$Calcium)) #There are no statistically significant outliers
## No transformation 


# Nitrates
## Histogram
ggplot (data = masterdata, aes (x = Nitrates)) + geom_histogram()
## Density Distribution
ggplot (data = masterdata, aes (x = Nitrates)) + stat_density() 
ggplot (data = masterdata, aes (x = Nitrates)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
## QQ-Plot
ggqqplot(masterdata$Nitrates, title = "Nitrates")
## Normality Test
shapiro.test(masterdata$Nitrates)
## Skewness
skewness (masterdata$Nitrates, na.rm = TRUE)
## Outliers
boxplot(masterdata$Nitrates)
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$Nitrates)) #There are no statistically significant outliers
## No transformation 

# Sodium
## Histogram
ggplot (data = masterdata, aes (x = Sodium)) + geom_histogram()
## Density Distribution
ggplot (data = masterdata, aes (x = Sodium)) + stat_density() 
ggplot (data = masterdata, aes (x = Sodium)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
## QQ-Plot
ggqqplot(masterdata$Sodium, title = "Sodium")
## Normality Test
shapiro.test(masterdata$Sodium)
## Skewness
skewness (masterdata$Sodium, na.rm = TRUE)
## Outliers
boxplot(masterdata$Sodium)
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$Sodium)) #Atleast 3 outliers
## BestNormalize Transformation
BNobject <- bestNormalize::bestNormalize(masterdata$Sodium)
BNobject #BoxCox transformation was chosen as the best transformation
masterdata$Sodium_BC <- predict(BNobject$chosen_transform) 
## Checking if the transformation worked
ggplot (data = masterdata, aes (x = Sodium_BC)) + stat_density()
ggplot (data = masterdata, aes (x = Sodium_BC)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggqqplot(masterdata$Sodium_BC, title = "Sodium")
shapiro.test(masterdata$Sodium_BC)
skewness (masterdata$Sodium_BC, na.rm = TRUE)
boxplot(masterdata$Sodium_BC) # There appears to be outliers
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$Sodium_BC)) #There are no statistically significant outliers
## BoxCox Transformed


# Potassium
## Histogram
ggplot (data = masterdata, aes (x = Potassium)) + geom_histogram()
## Density Distribution
ggplot (data = masterdata, aes (x = Potassium)) + stat_density() 
ggplot (data = masterdata, aes (x = Potassium)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
## QQ-Plot
ggqqplot(masterdata$Potassium, title = "Potassium")
## Normality Test
shapiro.test(masterdata$Potassium)
## Skewness
skewness (masterdata$Potassium, na.rm = TRUE)
## Outliers
boxplot(masterdata$Potassium)
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$Potassium)) 
## BestNormalize Transformation
BNobject <- bestNormalize::bestNormalize(masterdata$Potassium)
BNobject #BoxCox transformation was chosen as the best transformation
masterdata$Potassium_BC <- predict(BNobject$chosen_transform) 
## Checking if the transformation worked
ggplot (data = masterdata, aes (x = Potassium_BC)) + stat_density()
ggplot (data = masterdata, aes (x = Potassium_BC)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggqqplot(masterdata$Potassium_BC, title = "Potassium")
shapiro.test(masterdata$Potassium_BC)
skewness (masterdata$Potassium_BC, na.rm = TRUE)
boxplot(masterdata$Potassium_BC) # There appears to be no outliers
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$Potassium_BC)) #There are no statistically significant outliers
## BoxCox Transformed


# pH
## Histogram
ggplot (data = masterdata, aes (x = pH)) + geom_histogram()
## Density Distribution
ggplot (data = masterdata, aes (x = pH)) + stat_density() 
ggplot (data = masterdata, aes (x = pH)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
## QQ-Plot
ggqqplot(masterdata$pH, title = "pH")
## Normality Test
shapiro.test(masterdata$pH)
## Skewness
skewness (masterdata$pH, na.rm = TRUE)
## Outliers
boxplot(masterdata$pH)
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$pH)) 
## BestNormalize Transformation
BNobject <- bestNormalize::bestNormalize(masterdata$pH)
BNobject #OrderNorm transformation was chosen as the best transformation
masterdata$pH_ON <- predict(BNobject$chosen_transform) 
## Checking if the transformation worked
ggplot (data = masterdata, aes (x = pH_ON)) + stat_density()
ggplot (data = masterdata, aes (x = pH_ON)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggqqplot(masterdata$pH_ON, title = "pH")
shapiro.test(masterdata$pH_ON)
skewness (masterdata$pH_ON, na.rm = TRUE)
boxplot(masterdata$pH_ON) # There appears to be 2 outliers
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$pH_ON)) #There are no statistically significant outliers
## OrderNorm Transformed


# Conductivity
## Histogram
ggplot (data = masterdata, aes (x = Conductivity)) + geom_histogram()
## Density Distribution
ggplot (data = masterdata, aes (x = Conductivity)) + stat_density() 
ggplot (data = masterdata, aes (x = Conductivity)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
## QQ-Plot
ggqqplot(masterdata$Conductivity, title = "Conductivity")
## Normality Test
shapiro.test(masterdata$Conductivity)
## Skewness
skewness (masterdata$Conductivity, na.rm = TRUE)
## Outliers
boxplot(masterdata$Conductivity)
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$Conductivity)) 
## BestNormalize Transformation
BNobject <- bestNormalize::bestNormalize(masterdata$Conductivity)
BNobject #BoxCox transformation was chosen as the best transformation
masterdata$Conductivity_BC <- predict(BNobject$chosen_transform) 
## Checking if the transformation worked
ggplot (data = masterdata, aes (x = Conductivity_BC)) + stat_density()
ggplot (data = masterdata, aes (x = Conductivity_BC)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggqqplot(masterdata$Conductivity_BC, title = "Conductivity")
shapiro.test(masterdata$Conductivity_BC)
skewness (masterdata$Conductivity_BC, na.rm = TRUE)
boxplot(masterdata$Conductivity_BC) # There appears to be no outliers
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$Conductivity_BC)) #There are no statistically significant outliers
## BoxCox Transformed


# Total_Solids
## Histogram
ggplot (data = masterdata, aes (x = Total_Solids)) + geom_histogram()
## Density Distribution
ggplot (data = masterdata, aes (x = Total_Solids)) + stat_density() 
ggplot (data = masterdata, aes (x = Total_Solids)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
## QQ-Plot
ggqqplot(masterdata$Total_Solids, title = "Total_Solids")
## Normality Test
shapiro.test(masterdata$Total_Solids)
## Skewness
skewness (masterdata$Total_Solids, na.rm = TRUE)
## Outliers
boxplot(masterdata$Total_Solids)
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$Total_Solids)) 
## BestNormalize Transformation
BNobject <- bestNormalize::bestNormalize(masterdata$Total_Solids)
BNobject #OrderNorm transformation was chosen as the best transformation
masterdata$Total_Solids_ON <- predict(BNobject$chosen_transform) 
## Checking if the transformation worked
ggplot (data = masterdata, aes (x = Total_Solids_ON)) + stat_density()
ggplot (data = masterdata, aes (x = Total_Solids_ON)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggqqplot(masterdata$Total_Solids_ON, title = "Total_Solids")
shapiro.test(masterdata$Total_Solids_ON)
skewness (masterdata$Total_Solids_ON, na.rm = TRUE)
boxplot(masterdata$Total_Solids_ON) # There appears to be no outliers
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$Total_Solids_ON)) #There are no statistically significant outliers
## OrderNorm Transformed


# Volatile_Solids
## Histogram
ggplot (data = masterdata, aes (x = Volatile_Solids)) + geom_histogram()
## Density Distribution
ggplot (data = masterdata, aes (x = Volatile_Solids)) + stat_density() 
ggplot (data = masterdata, aes (x = Volatile_Solids)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
## QQ-Plot
ggqqplot(masterdata$Volatile_Solids, title = "Volatile_Solids")
## Normality Test
shapiro.test(masterdata$Volatile_Solids)
## Skewness
skewness (masterdata$Volatile_Solids, na.rm = TRUE)
## Outliers
boxplot(masterdata$Volatile_Solids)
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$Volatile_Solids)) 
## BestNormalize Transformation
BNobject <- bestNormalize::bestNormalize(masterdata$Volatile_Solids)
BNobject #SQRT (x+a) transformation was chosen as the best transformation
masterdata$Volatile_Solids_SQRT <- predict(BNobject$chosen_transform) 
## Checking if the transformation worked
ggplot (data = masterdata, aes (x = Volatile_Solids_SQRT)) + stat_density()
ggplot (data = masterdata, aes (x = Volatile_Solids_SQRT)) + stat_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggqqplot(masterdata$Volatile_Solids_SQRT, title = "Volatile_Solids")
shapiro.test(masterdata$Volatile_Solids_SQRT)
skewness (masterdata$Volatile_Solids_SQRT, na.rm = TRUE)
boxplot(masterdata$Volatile_Solids_SQRT) # There appears to be no outliers
## Rosners Test to stastistically detect outliers
rosnerTest((masterdata$Volatile_Solids_SQRT)) #There are no statistically significant outliers
## SQRT (x+a) Transformed


# Editing the masterdata set
# --------------------------------------------------------------------------
masterdata <- masterdata [, -c(4, 7:13)]

masterdata <- masterdata %>% dplyr::rename (Conductivity = Conductivity_BC, 
                                     Total_Solids = Total_Solids_ON, 
                                     Volatile_Solids = Volatile_Solids_SQRT, 
                                     Sodium = Sodium_BC, 
                                     Salt = Salt_ON,
                                     pH = pH_ON,
                                     Potassium = Potassium_BC, 
                                     E.coli = E.coli_ON)



# Redoing the Publication Figures with the normality transformed master dataset 
# -----------------------------------------------------------------------------

# Visualizing model variable correlations
library (corrplot)
masterdata_num <- masterdata [, c(4:13)]
masterdata_num <- na.omit (masterdata_num)
anyNA (masterdata_num)
masterdata_num <- masterdata_num %>% dplyr::rename (EC = Conductivity, 
                                             TS = Total_Solids, 
                                             VS = Volatile_Solids, 
                                             Na = Sodium, 
                                             K = Potassium, 
                                             Ca = Calcium, 
                                             NO3 = Nitrates)
correlations = cor(masterdata_num)
col3 <- colorRampPalette(c("red", "white", "blue")) 
correlogram <- corrplot(correlations, type = "lower", 
                        tl.srt = 1, tl.pos = "d", tl.col = "black", tl.cex = 1.3, 
                        outline = TRUE, order = "hclust", col = col3(10))






# Visualizing the relationship of individual variables to the outcome
##Continuous Variables (for publication purposes)
library (ggplot2)
##Positive 
p1 <- ggplot(data=masterdata, mapping = aes(x = Potassium, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "blue")
p2 <- ggplot(data=masterdata, mapping = aes(x = Nitrates, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "blue")
p3 <- ggplot(data=masterdata, mapping = aes(x = Calcium, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "blue")
p4 <- ggplot(data=masterdata, mapping = aes(x = Sodium, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "blue")
p5 <- ggplot(data=masterdata, mapping = aes(x = Salt, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "blue")
##Negative 
p6 <- ggplot(data=masterdata, mapping = aes(x = Total_Solids, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "red")
p7 <- ggplot(data=masterdata, mapping = aes(x = pH, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "red")
p8 <- ggplot(data=masterdata, mapping = aes(x = Conductivity, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "red")
p9 <- ggplot(data=masterdata, mapping = aes(x = Volatile_Solids, y = E.coli)) +
  geom_point(size = 1) +
  geom_smooth(method = "glm", color = "red")
##Categorical Variables
##Box and Whiskers Plot
bp1 <- ggplot (masterdata, aes (x = Treatment, y = E.coli)) + 
  geom_boxplot()
bp2 <- ggplot (masterdata, aes (x = Season, y = E.coli)) + 
  geom_boxplot()
bp3 <- ggplot (masterdata, aes (x = Stage, y = E.coli)) + 
  geom_boxplot()
##Creating a multiple plot 
library (Rmisc)
multiplot (p1,p2,p3,p4,p5,p6,p7,p8,p9,bp1, bp2, bp3, cols = 3)








# ---------------------------------------------------------------------
# 3: PRE-PROCESSING THE TRAINING & TEST DATASETS AFTER DATA SPLITTING  
# ---------------------------------------------------------------------


# Splitting the master dataset into training and test dataset, based on outcome=CFU
set.seed (100)
## Step 1: Get row numbers for the training data
TrainRowNumbers <- createDataPartition(masterdata$E.coli, p=0.8, list=FALSE)
## Step 2: Create the training dataset
TrainData <- masterdata[TrainRowNumbers,]
## Step 3: Create the test dataset
TestData <- masterdata[-TrainRowNumbers,]
## Store X & Y from training dataset for future use
x_train = TrainData[, 1:12]
y_train = TrainData$E.coli
x_test = TestData[, 1:12]
y_test = TestData$E.coli


# (1) Data Imputation Model: Creating a Data Imputation Model (knn imputation model) using the Training Dataset
## [This preprocessing step, while imputing the missing data, also transforms
## numerical varaibles by centering (subtracting by mean) and scaling (dividing by std deviation)]
MissingDataModel <- preProcess(TrainData, method='knnImpute') 
MissingDataModel


# (2) Dummy Variable Model: Creating a Dummy Variable Model using the Training Dataset
## [One-hot means only one of the dummy variables are 1, and all the others are 0.]
## [Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.]
## [Creating the dummy variables using this function removes the outcome variable]
DummiesModel <- dummyVars(E.coli ~ ., data = TrainData)
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
TrainData$E.coli <- y_train
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
TestData$E.coli <- y_test
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
lmProfile_rf <- rfe(x=TrainData[, 1:17], y=TrainData$E.coli,
                    sizes = subsets,
                    rfeControl = ctrl_rf)
lmProfile_lm <- rfe(x=TrainData[, 1:17], y=TrainData$E.coli,
                    sizes = subsets,
                    rfeControl = ctrl_lm)
lmProfile_tb <- rfe(x=TrainData[, 1:17], y=TrainData$E.coli,
                    sizes = subsets,
                    rfeControl = ctrl_tb)
lmProfile_ct <- rfe(x=TrainData[, 1:17], y=TrainData$E.coli,
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
                             repeats=20,
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
Models <- caretList(E.coli ~ ., data=TrainData, trControl=trainControl, methodList=Algorithms) 
## Obtaining the results of the model sets using resampling and trainControl as defined earlier
Results_models <- resamples(Models)
## Summarizing the results of the model sets 
summary (Results_models)
## Looking at all three parameters but mainly considering RMSE
parallelplot(Results_models, metric = "RMSE")
parallelplot(Results_models, metric = "Rsquared")
parallelplot(Results_models, metric = "MAE")


## Box plots to compare models using the bw plot function
scales <- list(x=list(relation="free"), y=list(relation="free"))
bw_models <- bwplot(Results_models, scales=scales)
## RMSE: Box plots to compare models using the ggplot2 plot function (which gives us more control but is tougher to perform)
library(tidyverse)
Results_models$values %>%                   #extract the values
  select(1, ends_with("RMSE")) %>%          #select the first column and all columns with a name ending with "RMSE"
  gather(model, RMSE, -1) %>%               #convert to long table
  mutate(model = sub("~RMSE", "", model)) %>%              #leave just the model names
  ggplot()+                                                #call ggplot
  geom_boxplot(aes(x = RMSE, y = model))+                  #and plot the box plot
  theme(plot.margin = margin (t = 80, r = 80, b = 40, l = 40),
        axis.title.x = element_text(size = 28, margin = margin(t = 20, r = 20, b = 0, l = 0)),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=20, margin = margin(t = 10)),
        axis.text.y = element_text(size=20, margin = margin (r = 10))) -> p1
p1

## R2: Box Plots using ggplot2
##Results_models$values %>%                  
## select(1, ends_with("Rsquared")) %>%          
## gather(model, Rsquared, -1) %>%               
## mutate(model = sub("~Rsquared", "", model)) %>%              
## ggplot()+                                                
## geom_boxplot(aes(x = Rsquared, y = model))+                  
## theme(plot.margin = margin (t = 80, r = 80, b = 40, l = 40),
##        axis.title.x = element_text(size = 28, margin = margin(t = 20, r = 20, b = 0, l = 0)),
##        axis.title.y = element_blank(),
##        axis.text.x = element_text(size=20, margin = margin(t = 10)),
##        axis.text.y = element_text(size=20, margin = margin (r = 10)))  -> p2
##p2
##library (Rmisc)
##multiplot(p1, p2, cols = 2)

# Calculating p-values to determine if the models are statistically different (using RMSE)
ModelDiffs<-diff(Results_models, metric="RMSE")
summary(ModelDiffs)



# (A) Combing all 28 models to form an ensemble model using GLM
# -----------------------------------------------------------------------
library (caretEnsemble)
## Create the trainControl
stackControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=20,
                             savePredictions=TRUE, 
                             classProbs=TRUE)
## Ensemble the predictions of `models` to form a new combined prediction based on glm
stack.glm.all <- caretStack(Models, method="glm", metric="RMSE", trControl=stackControl)
print(stack.glm.all)
summary (stack.glm.all)
# Computing individual model importance in the Combined Prediction based glm model
varimp_combined <- varImp(stack.glm.all$ens_model, useModel = TRUE, nonpara = FALSE, scale = TRUE)
varimp_combined
varimp_combined$importance
plot(varimp_combined, main="Variable Importance with Ensemble GLM Model")


# (B) 26-model GLM Ensemble (28 models minus Rborist which had zero importance & minus SVMLinear 2 which did not yield any value since it was correlated; compared to the 28-model ensemble)
set.seed(100)
trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=20,
                             savePredictions=TRUE, 
                             classProbs=TRUE)
Algorithms_imp <- c('bayesglm', 'glm', 'glmStepAIC',
                    'xgbDART', 'xgbLinear', 'xgbTree',
                    'svmRadialSigma', 'svmRadial','svmLinear', 'svmLinear3','svmPoly','svmRadialCost',
                    'avNNet', 'nnet','brnn',
                    'rf','cforest','parRF','qrf','ranger', 'rfRules', 'RRF','RRFglobal',
                    'earth', 'widekernelpls', 'enet')
Models_imp <- caretList(E.coli ~ ., data=TrainData, trControl=trainControl, methodList=Algorithms_imp) 
Results_models_imp <- resamples(Models_imp)
summary(Results_models_imp)
stack.glm.imp <- caretStack(Models_imp, method="glm", metric="rmse", trControl=stackControl)
print(stack.glm.imp)
summary (stack.glm.imp)
# Computing individual model importance in the Combined Prediction based glm model
varimp_combined_imp <- varImp(stack.glm.imp$ens_model, useModel = TRUE, nonpara = FALSE, scale = TRUE)
varimp_combined_imp
varimp_combined_imp$importance
plot(varimp_combined_imp, main="Variable Importance with 26-model GLM")



# (C) Running one of the 2 best Models = svmRadialSigma in more detail
#-------------------------------------------------------------------
set.seed(100)
## Looking up the parameters present in the model
modelLookup("svmRadialSigma")
## Training the model
model_svmRadialSigma = train (E.coli ~., data=TrainData, method='svmRadialSigma')
## Visualizing the model parameters chosen
model_svmRadialSigma
plot (model_svmRadialSigma)
## Creating an universal traincontrol function
trainControl <- trainControl(method="repeatedcv", 
                             number=10,
                             repeats = 20,
                             savePredictions=TRUE, 
                             classProbs=TRUE)
## Hypertuning the parameters of the model 
set.seed(100)
model_svmRadialSigma = train(E.coli ~ ., data=TrainData, method='svmRadialSigma', tuneLength=10, trControl = trainControl)
model_svmRadialSigma
plot (model_svmRadialSigma)
model_svmRadialSigma$finalModel
## Fitting the trained model to the Training Data to predict the outcome (only for visualization since it is done automatically in the model)
fitted_svmRadialSigma <- predict (model_svmRadialSigma)
fitted_svmRadialSigma
## Computing the importance of each variable in the model
varimp_svmRadialSigma <- varImp(model_svmRadialSigma, useModel = FALSE, nonpara = FALSE, scale = TRUE)
varimp_svmRadialSigma
plot(varimp_svmRadialSigma, cex = 1)


# (D) Running one of the 2 best Models  = Ranger in more detail
#-------------------------------------------------------------------
set.seed(100)
## Looking up the parameters present in the model
modelLookup("ranger")
## Training the model
model_ranger = train (E.coli ~., data=TrainData, method='ranger')
## Visualizing the model parameters chosen
model_ranger
plot (model_ranger)
## Creating an universal traincontrol function
trainControl <- trainControl(method="repeatedcv", 
                             number=10,
                             repeats = 20,
                             savePredictions=TRUE, 
                             classProbs=TRUE)
## Hypertuning the parameters of the model 
set.seed(100)
model_ranger = train(E.coli ~ ., data=TrainData, method='ranger', tuneLength=10, trControl = trainControl)
model_ranger
plot (model_ranger)
model_ranger$finalModel
## Fitting the trained model to the Training Data to predict the outcome (only for visualization since it is done automatically in the model)
fitted_ranger <- predict (model_ranger)
fitted_ranger
## Computing the importance of each variable in the model
varimp_ranger <- varImp(model_ranger, useModel = FALSE, nonpara = FALSE, scale = TRUE)
varimp_ranger
plot(varimp_ranger, cex = 1)



# (E) Combining the best performing (RMSE median) models in each of the 7 families to create ensemble using GLM
# -------------------------------------------------------------------------------------------------------------
##Not good to choose the best performing models for ensemble because closely correlated models cause problems
#Algorithms_best <- c('glmStepAIC', 'ranger', 'xgbDART', 'svmRadialSigma', 'earth', 'brnn','widekernelpls')
#Models_best <- caretList(E.coli ~ ., data=TrainData, trControl=trainControl, methodList=Algorithms_best) 
#Results_models_best <- resamples(Models_best)
#summary(Results_models_best)
#stack.glm.best <- caretStack(Models_best, method="glm", metric="rmse", trControl=stackControl)
#print(stack.glm.best)


# ------------------------------------------------------------------
# 5.MODEL TESTING: USING THE TEST DATASET
# -------------------------------------------------------------------

# Combined 28-model GLM Ensemble: Predicting the outcome in the Test dataset using the trained model
stack_predicteds <- predict(stack.glm.all, newdata=TestData)
stack_predicteds
# Measures of regression between the actual outcome and the predicted outcome in the Test Dataset
postResample(pred = stack_predicteds, obs = TestData$E.coli)


# Combined 26-model GLM Ensemble: Predicting the outcome in the Test dataset using the trained model
stack_predicteds_imp <- predict(stack.glm.imp, newdata=TestData)
stack_predicteds_imp
# Measures of regression between the actual outcome and the predicted outcome in the Test Dataset
postResample(pred = stack_predicteds_imp, obs = TestData$E.coli)


# svmRadialSigma Model: Predicting the outcome in the Test dataset using the trained model
predicted_svmRadialSigma <- predict(model_svmRadialSigma, TestData)
predicted_svmRadialSigma
# Measures of regression between the actual outcome and the predicted outcome in the Test Dataset
postResample(pred = predicted_svmRadialSigma, obs = TestData$E.coli)


## Determining the Model Independant variable importance metric for the best fit independent model, svmRadialSigma
varimp_model <- varImp(model_svmRadialSigma, useModel = FALSE, nonpara = TRUE, scale = TRUE)
varimp_model
plot(varimp_svmRadialSigma, cex = 1)



