# Master Analysis Function - Auto-detects and runs the right test

Master Analysis Function - Auto-detects and runs the right test

## Usage

``` r
analyze(
  x = NULL,
  y = NULL,
  data = NULL,
  formula = NULL,
  mu = 0,
  paired = FALSE,
  nonparam = FALSE,
  conf.level = 0.95,
  var_name = "Variable",
  var1_name = "Variable 1",
  var2_name = "Variable 2",
  method = "pearson"
)
```

## Arguments

- x:

  A numeric vector (required always)

- y:

  A numeric vector, factor, or character group variable (optional)

- data:

  A data frame (required if using a formula)

- formula:

  A formula of the form outcome ~ predictor or outcome ~ group1 \*
  group2 or cbind(y1, y2) ~ group (optional)

- mu:

  Hypothesised mean for one-sample t-test. Default 0.

- paired:

  Logical. TRUE for paired t-test. Default FALSE.

- nonparam:

  Logical. TRUE to use non-parametric tests. Default FALSE.

- conf.level:

  Confidence level. Default 0.95.

- var_name:

  Optional label for the report.

- var1_name:

  Optional name for first variable in correlation.

- var2_name:

  Optional name for second variable in correlation.

- method:

  Correlation method: "pearson", "spearman", or "kendall". Default
  "pearson".

## Value

A printed analysis report from the appropriate test

## Examples

``` r
# Descriptive only
analyze(x = c(23, 45, 12, 67, 34))
#> [statease] Single numeric vector -> Running Descriptive Statistics
#> Warning: Sample size is small (n < 10). Interpret descriptive statistics with caution.
#> 
#> -- statease Descriptive Report ----------------------------------
#>   Variable     : Variable
#>   N            : 5  |  Missing: 0
#> -----------------------------------------------------------------
#>   Mean         : 36.20
#>   Median       : 34.00
#>   Std Dev      : 21.16
#>   Min          : 12.00  |  Max: 67.00
#>   Q1           : 23.00  |  Q3: 45.00
#>   IQR          : 22.00
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The distribution is approximately symmetric.
#>   Spread shows high variability (CV = 58.5%).
#>   Shapiro-Wilk test suggests normality is reasonable (W = 0.979, p = 0.9276).
#> -----------------------------------------------------------------
#> 

# Auto t-test
analyze(x = c(23,45,12,67,34), y = c(19,38,22,51,29))
#> [statease] Two numeric vectors detected -> Running T-Test
#> Warning: Sample size in x is small (n < 10). Interpret results with caution.
#> Warning: Sample size in y is small (n < 10). Interpret results with caution.
#> 
#> -- statease T-Test Report ----------------------------------------
#>   Test         : Independent Samples T-Test
#>   Variable     : Variable
#>   Groups       : Group 1: n = 5  |  Group 2: n = 5
#> -----------------------------------------------------------------
#>   t-statistic  : 0.396
#>   df           : 6.6
#>   p-value      : 0.7043
#>   95% CI      : [-22.146, 30.946]
#>   Cohen's d    : 0.251 (small effect)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant (p = 0.704 > alpha 0.05).
#>   Group 1 had a higher mean (36.20 vs 31.80).
#>   Effect size is small (d = 0.251).
#>   95% CI: true difference lies between -22.146 and 30.946.
#> -----------------------------------------------------------------
#> 

# Auto Mann-Whitney (non-parametric)
analyze(x = c(23,45,12,67,34), y = c(19,38,22,51,29),
        nonparam = TRUE)
#> [statease] nonparam = TRUE -> Running Mann-Whitney U Test
#> Warning: Sample size is small (n < 10). Interpret results with caution.
#> 
#> -- statease Mann-Whitney U Test Report --------------------------
#>   Variable     : Variable
#>   Group 1      : n = 5  |  Median = 34.00
#>   Group 2      : n = 5  |  Median = 29.00
#> -----------------------------------------------------------------
#>   W statistic  : 14.000
#>   p-value      : 0.8413
#>   95% CI      : [-26.000, 38.000]
#>   Effect size  : 0.063 (negligible)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant (p = 0.8413 > alpha 0.05).
#>   Values in Group 1 appear stochastically greater than values in Group 2. (Reported medians: Group 1 = 34.00, Group 2 = 29.00)
#>   Effect size is negligible (r = 0.063).
#>   Note: Mann-Whitney tests stochastic superiority,
#>   not differences in medians.
#> -----------------------------------------------------------------
#> 

# Auto correlation
analyze(x = c(23,45,12,67,34), y = c(19,38,22,51,29),
        var1_name = "Scores", var2_name = "Hours")
#> [statease] Two numeric vectors with variable names -> Running Correlation
#> Warning: Sample size is small (n < 10). Interpret correlation with caution.
#> 
#> -- statease Correlation Report -----------------------------------
#>   Method       : Pearson Product-Moment Correlation
#>   Variables    : Scores & Hours
#>   N            : 5  |  Missing: 0
#> -----------------------------------------------------------------
#>   r            : 0.9626
#>   p-value      : 0.0086
#>   95% CI      : [0.5332, 0.9976]
#>   Strength     : very large
#>   Direction    : positive (as one variable increases, the other tends to increase)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The correlation is statistically significant (p = 0.0086 < alpha 0.05).
#>   The relationship between Scores and Hours is
#>   very large and positive (as one variable increases, the other tends to increase) in direction.
#> -----------------------------------------------------------------
#> 

# Auto One-Way ANOVA
df <- data.frame(
  score = c(23,45,12,67,34,89,56,43,78,90,11,34),
  group = rep(c("A","B","C"), each = 4)
)
analyze(formula = score ~ group, data = df)
#> [statease] 3+ groups detected -> Running One-Way ANOVA
#> Warning: One or more groups have small sample sizes (n < 10). Interpret with caution.
#> 
#> -- statease ANOVA Report -----------------------------------------
#>   Outcome      : score
#>   Group        : group  (3 levels)
#> -----------------------------------------------------------------
#>   Group Means:
#>     A            : Mean = 36.75  (n = 4)
#>     B            : Mean = 55.50  (n = 4)
#>     C            : Mean = 53.25  (n = 4)
#> -----------------------------------------------------------------
#>   F-statistic  : 0.494
#>   df           : 2, 9
#>   p-value      : 0.6260
#>   Eta squared  : 0.0988 (moderate effect)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The overall ANOVA result is not statistically significant (p = 0.6260 > alpha 0.05).
#>   Group differences explain 9.9% of variance
#>   (eta^2 = 0.0988, moderate effect).
#> 
#>   Post-hoc tests not run (overall result not significant).
#> -----------------------------------------------------------------
#> 

# Auto Kruskal-Wallis (non-parametric)
analyze(formula = score ~ group, data = df, nonparam = TRUE)
#> [statease] 3+ groups + nonparam = TRUE -> Running Kruskal-Wallis Test
#> Warning: One or more groups have very small sample sizes (n < 5).
#> 
#> -- statease Kruskal-Wallis Test Report --------------------------
#>   Outcome      : score
#>   Group        : group  (3 levels)
#>   N            : 12
#> -----------------------------------------------------------------
#>   Group Medians:
#>     A            : Median = 34.00  (n = 4)
#>     B            : Median = 49.50  (n = 4)
#>     C            : Median = 56.00  (n = 4)
#> -----------------------------------------------------------------
#>   H statistic  : 0.762
#>   df           : 2
#>   p-value      : 0.6831
#>   Eta squared  : 0.0000 (negligible effect)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant (p = 0.6831 > alpha 0.05).
#>   The result is not statistically significant (p = 0.6831 > alpha 0.05).
#>   Effect size is negligible (eta^2 = 0.0000).
#>   Note: Kruskal-Wallis tests stochastic superiority
#>   across groups, not differences in medians.
#>   Medians are reported for descriptive purposes only.
#> 
#>   Post-hoc tests not run (overall result not significant).
#> -----------------------------------------------------------------
#> 

# Auto Two-Way ANOVA
df2 <- data.frame(
  score  = c(23,45,12,67,34,89,56,43,78,90,11,34),
  method = rep(c("Online","Traditional"), each = 6),
  gender = rep(c("Male","Female"), times = 6)
)
analyze(formula = score ~ method * gender, data = df2)
#> [statease] Two grouping variables detected -> Running Two-Way ANOVA
#> Warning: Sample size is small (n < 20). Interpret results with caution.
#> 
#>  statease Two-Way ANOVA Report 
#>   Outcome      : score
#>   Factor 1     : method
#>   Factor 2     : gender
#>   N            : 12
#> 
#>   Means by method:
#>     Online          : 45.00
#>     Traditional     : 52.00
#> 
#>   Means by gender:
#>     Female          : 61.33
#>     Male            : 35.67
#> 
#>   Interaction Means:
#>             Female  Male
#> Online       67.00 23.00
#> Traditional  55.67 48.33
#> 
#>   ANOVA Results:
#>   method               : F = 0.220  df = 1,8  not significant (p = 0.6517)  eta^2 = 0.0173 (small)
#>   gender               : F = 2.955  df = 1,8  not significant (p = 0.1240)  eta^2 = 0.2330 (large)
#>   Interaction          : F = 1.507  df = 1,8  not significant (p = 0.2544)  eta^2 = 0.1189 (moderate)
#> 
#>   Interpretation:
#>   Main effect of method is not significant (p = 0.6517).
#>   Main effect of gender is not significant (p = 0.1240).
#>   Interaction (method x gender) is not significant (p = 0.2544).
#> -----------------------------------------------------------------
#> 

# Auto Regression
df3 <- data.frame(
  exam_score  = c(23,45,12,67,34,89,56,43,78,90),
  study_hours = c(2,5,1,7,3,9,6,4,8,10)
)
analyze(formula = exam_score ~ study_hours, data = df3)
#> [statease] Numeric predictor detected -> Running Simple Linear Regression
#> 
#> -- statease Simple Linear Regression Report ---------------------
#>   Outcome      : exam_score
#>   Predictor    : study_hours
#>   N            : 10
#> -----------------------------------------------------------------
#>   Model Equation:
#>   exam_score = 4.800 + 8.891 * study_hours
#> -----------------------------------------------------------------
#>   Coefficients:
#>   Intercept    : 4.800
#>   Slope        : 8.891  (SE = 0.336)
#>   t-statistic  : 26.442
#>   p-value      : 0.0000
#>   95% CI      : [8.116, 9.666]
#> -----------------------------------------------------------------
#>   Model Fit:
#>   R-squared    : 0.9887
#>   Adj R-squared: 0.9873
#>   F-statistic  : 699.184 (df = 1, 8)  p = 0.0000
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The predictor study_hours is statistically significant (p = 0.0000 < alpha 0.05).
#>   The slope is positive - as study_hours increases by 1 unit, exam_score increases by 8.891 units.
#>   R-squared = 0.9887: study_hours explains 98.9% of the
#>   variance in exam_score (large effect).
#> -----------------------------------------------------------------
#> 

# Auto Multiple Regression
df4 <- data.frame(
  exam_score  = c(23,45,12,67,34,89,56,43,78,90),
  study_hours = c(2,5,1,7,3,9,6,4,8,10),
  attendance  = c(60,80,50,90,70,95,85,75,88,92)
)
analyze(formula = exam_score ~ study_hours + attendance, data = df4)
#> [statease] Multiple numeric predictors -> Running Multiple Linear Regression
#> Warning: Sample size is small (n < 20). Interpret results with caution.
#> 
#> -- statease Multiple Linear Regression Report -------------------
#>   Outcome      : exam_score
#>   Predictors   : study_hours, attendance
#>   N            : 10
#> -----------------------------------------------------------------
#>   Model Equation:
#>   exam_score = -2.664 + 8.240*study_hours + 0.141*attendance
#> -----------------------------------------------------------------
#>   Overall Model Fit:
#>   R-squared    : 0.9893 (large effect)
#>   Adj R-squared: 0.9862
#>   F-statistic  : 322.885 (df = 2, 7)  p = 0.0000
#>   The overall model is statistically significant (p = 0.0000 < alpha 0.05).
#> -----------------------------------------------------------------
#>   Individual Predictors:
#> 
#>   study_hours
#>     Coefficient  : 8.240  (SE = 1.106)
#>     t-statistic  : 7.452
#>     p-value      : 0.0001  [significant]
#>     95% CI      : [5.626, 10.855]
#>     Direction    : positive (b = 8.240)
#> 
#>   attendance
#>     Coefficient  : 0.141  (SE = 0.227)
#>     t-statistic  : 0.620
#>     p-value      : 0.5549  [not significant]
#>     95% CI      : [-0.396, 0.677]
#>     Direction    : positive (b = 0.141)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The model explains 98.9% of the variance in exam_score
#>   (R-squared = 0.9893, large effect).
#>   Adjusted R-squared = 0.9862 accounting for
#>   the number of predictors in the model.
#> 
#>   Significant predictors: study_hours
#>   Non-significant predictors: attendance
#> -----------------------------------------------------------------
#> 

# Auto MANOVA
df5 <- data.frame(
  math    = c(23,45,12,67,34,89,56,43,78,90,11,34),
  english = c(34,56,23,78,45,90,67,54,89,95,22,45),
  group   = rep(c("A","B","C"), each = 4)
)
analyze(formula = cbind(math, english) ~ group, data = df5)
#> [statease] Multiple outcomes detected -> Running MANOVA
#> Warning: Sample size is small (n < 20). Interpret results with caution.
#> 
#> -- statease MANOVA Report ----------------------------------------
#>   Outcomes     : math, english
#>   Group        : group  (3 levels)
#>   N            : 12
#> -----------------------------------------------------------------
#>   Group Means:
#> 
#>   math:
#>     A            : 36.75
#>     B            : 55.50
#>     C            : 53.25
#> 
#>   english:
#>     A            : 47.75
#>     B            : 64.00
#>     C            : 62.75
#> -----------------------------------------------------------------
#>   Multivariate Test Results:
#>   Pillai's Trace : 0.1372
#>   Wilks' Lambda  : 0.8645
#>   F-statistic    : 0.331  (df = 4, 18)
#>   p-value        : 0.8532
#>   Effect size    : small (Pillai = 0.1372)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The overall MANOVA result is not statistically significant (p = 0.8532 > alpha 0.05).
#>   Pillai's Trace = 0.1372 indicates a small effect.
#> -----------------------------------------------------------------
#>   Follow-Up Univariate ANOVAs:
#> 
#>   math
#>     F = 0.494  (df = 2, 9)  p = 0.6260  [not significant]
#> 
#>   english
#>     F = 0.444  (df = 2, 9)  p = 0.6550  [not significant]
#> 
#>   Note: Follow-up ANOVAs identify which outcomes
#>   differ significantly across groups.
#> -----------------------------------------------------------------
#> 

# Chi-square
analyze(
  x = c("Yes","No","Yes","Yes","No"),
  y = c("Male","Female","Male","Female","Male")
)
#> [statease] Two categorical vectors detected -> Running Chi-Square Test
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> 
#> -- statease Chi-Square Test Report ------------------------------
#>   N            : 5
#> -----------------------------------------------------------------
#>   Contingency Table (Observed):
#>      y
#> x     Female Male
#>   No       1    1
#>   Yes      1    2
#> 
#>   Expected Frequencies:
#>      y
#> x     Female Male
#>   No     0.8  1.2
#>   Yes    1.2  1.8
#> 
#> -----------------------------------------------------------------
#>   Chi-square   : 0.000
#>   df           : 1
#>   p-value      : 1.0000
#>   Cramer's V   : 0.000 (negligible effect)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant (p = 1.0000 > alpha 0.05).
#>   There is no significant association between the two variables.
#>   Effect size is negligible (V = 0.000).
#> 
#>   WARNING: Some expected frequencies are less than 5. Interpret with caution.
#> -----------------------------------------------------------------
#> 
```
