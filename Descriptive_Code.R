# *DAIRY AGROECOSYSTEMS*

# Libraries
library(readxl)
library (tidyverse)
library (arsenal)
library (magrittr)

# Importing dataset
masterdata <- read_excel("masterdata.xlsx")
# Converting Date values to Date format
masterdata$Date <- as.Date(masterdata$Date, format = "%Y-%m-%d" )
# Creating a concise dataset restricted to sample averages
masterdata_av <- masterdata [, c(1:7,11,15,19,23,27,31,35:37,42)]

# Summary Statistics & ANOVA
# Creating tableby controls variable (Summary stats & ANOVA)
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
table1 <- tableby (Treatment ~., data = masterdata_av, control = tcontrols, title = "ANOVA")
table2 <- tableby (Stage ~., data = masterdata_av, control = tcontrols)
table3 <- tableby (Season ~., data = masterdata_av, control = tcontrols)
table4 <- tableby (interaction (Treatment, Stage) ~., data = masterdata_av, control = tcontrols)
table1a <- table1 [c(7:16)]
table2a <- table2 [c(7:16)]
table3a <- table3 [c(7:16)]
table4a <- table4 [c(7:16)]
write2html(table1a, "table1.html", 
           title = "TABLE 1: COMPARING FARM TREATMENTS: ANALYSIS OF VARIANCE (ANOVA) & DESCRIPTIVE STATS")
write2html(table2a, "table2.html", 
           title = "TABLE 2: COMPARING STAGES: ANALYSIS OF VARIANCE (ANOVA) & DESCRIPTIVE STATS")
write2html(table3a, "table3.html",
           title = "TABLE 3: COMPARING SEASONS: ANALYSIS OF VARIANCE (ANOVA) & DESCRIPTIVE STATS")
write2html(table4a, "table4.html",
           title = "TABLE 4: COMPARING TREATMENTS & STAGES: ANALYSIS OF VARIANCE (ANOVA) & DESCRIPTIVE STATS")



