DAIRY EFFLUENTS: COMPARING TREATMENTS & STAGES 

The ANOVA tests show the following: 
- Comparing Treatments (Table 1): Significant difference in K, pH, and CFU
- Comparing Stages (Table 2): Significant difference in Ca, NO3, Na, K, TS, and CFU
- Comparing Seasons (Table 3): No significant difference in any variables
- Comparing Treatments and stages (Table 4): Signficant difference in NO3, Na, K, TS, and CFU

Thus, to summarize: 
- There appears to be no seasonal difference in variables
- CFU and K varies significantly across treatments, stages, and treatment-stage interactions
- Na, NO3, and TS varies significantly across both stages and treatment-stage interactions

DIARY EFFLUENTS: REGRESSION MODEL FOR TS  

TS ~ Treatment + Stage + Season

The Regression Outliers Diagnostics shows the following: 
- Bonferroni Outlier Test: Row 42 is the most extreme outlier
- QQ plot is close to the linear line, but sample 42 seem to be outliers 
- Leverage Plot: 42 seems to have high leverage (i.e., influence on the model, in addition to being outliers)

DIARY EFFLUENTS: REGRESSION MODEL FOR CFU  

CFU ~ Treatment + Stage + Season + pH + VS

The Regression Outliers Diagnostics shows the following: 
- Bonferroni Outlier Test: Row 55 and 52 is the most extreme outlier
- QQ plot is close to the linear line, but samples 52 and 55 seem to be outliers 
- Leverage Plot: Both 55 and 52 seem to have high leverage (i.e., influence on the model, in addition to being outliers)
