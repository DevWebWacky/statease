# Introduction to statease

## Overview

**statease** is an R package designed to simplify statistical analysis
by combining a wide range of tests with automatic plain-English
interpretation of results. Whether you are a student, researcher, or
educator, statease gives you numbers and meaning in one command.

------------------------------------------------------------------------

## Installation

``` r

# Install from CRAN
install.packages("statease")

# Load the package
library(statease)
```

------------------------------------------------------------------------

## Dataset

Throughout this vignette we use a simulated dataset of 90 students
examining the effect of teaching methods on exam performance.

``` r

set.seed(42)

tutorial_data <- data.frame(
  student_id = 1:90,
  method     = rep(c("Traditional", "Online", "Hybrid"), each = 30),
  gender     = rep(c("Male", "Female"), times = 45),
  exam_score = c(
    round(rnorm(30, mean = 65, sd = 10)),
    round(rnorm(30, mean = 72, sd = 10)),
    round(rnorm(30, mean = 78, sd = 10))
  ),
  pre_test = c(
    round(rnorm(30, mean = 55, sd = 10)),
    round(rnorm(30, mean = 58, sd = 10)),
    round(rnorm(30, mean = 57, sd = 10))
  ),
  age        = round(rnorm(90, mean = 22, sd = 3)),
  passed     = rbinom(90, 1, prob = 0.7)
)

head(tutorial_data)
#>   student_id      method gender exam_score pre_test age passed
#> 1          1 Traditional   Male         79       69  22      0
#> 2          2 Traditional Female         59       50  20      0
#> 3          3 Traditional   Male         69       62  23      1
#> 4          4 Traditional Female         71       69  23      1
#> 5          5 Traditional   Male         69       44  21      1
#> 6          6 Traditional Female         64       46  18      0
```

------------------------------------------------------------------------

## 1. Descriptive Statistics

The
[`describe()`](https://devwebwacky.github.io/statease/reference/describe.md)
function provides a full summary of a numeric variable including
measures of central tendency, spread, and a normality check.

``` r

result <- describe(tutorial_data$exam_score,
                   var_name = "Exam Score")
print(result)
#> 
#> -- statease Descriptive Report ----------------------------------
#>   Variable     : Exam Score
#>   N            : 90  |  Missing: 0
#> -----------------------------------------------------------------
#>   Mean         : 72.10
#>   Median       : 74.00
#>   Std Dev      : 11.94
#>   Min          : 38.00  |  Max: 93.00
#>   Q1           : 64.00  |  Q3: 80.00
#>   IQR          : 16.00
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The distribution is approximately symmetric.
#>   Spread shows moderate variability (CV = 16.6%).
#>   Shapiro-Wilk test suggests non-normality (W = 0.958, p = 0.0054).
#> -----------------------------------------------------------------
```

You can also extract individual values:

``` r

result$mean
#> [1] 72.1
result$sd
#> [1] 11.93903
result$skew_label
#> [1] "approximately symmetric"
```

------------------------------------------------------------------------

## 2. T-Tests

### Independent Samples T-Test

Compare exam scores between male and female students:

``` r

males   <- tutorial_data$exam_score[tutorial_data$gender == "Male"]
females <- tutorial_data$exam_score[tutorial_data$gender == "Female"]

result <- ttest_interpret(
  males, females,
  var_name = "Exam Score by Gender"
)
print(result)
#> 
#> -- statease T-Test Report ----------------------------------------
#>   Test         : Independent Samples T-Test
#>   Variable     : Exam Score by Gender
#>   Groups       : Group 1: n = 45  |  Group 2: n = 45
#> -----------------------------------------------------------------
#>   t-statistic  : 0.026
#>   df           : 88.0
#>   p-value      : 0.9790
#>   95% CI      : [-4.964, 5.097]
#>   Cohen's d    : 0.006 (negligible effect)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant (p = 0.979 > alpha 0.05).
#>   Group 1 had a higher mean (72.13 vs 72.07).
#>   Effect size is negligible (d = 0.006).
#>   95% CI: true difference lies between -4.964 and 5.097.
#> -----------------------------------------------------------------
```

### One-Sample T-Test

Test whether the average exam score is significantly different from 70:

``` r

result <- ttest_interpret(
  tutorial_data$exam_score,
  mu = 70,
  var_name = "Exam Score"
)
print(result)
#> 
#> -- statease T-Test Report ----------------------------------------
#>   Test         : One-Sample T-Test
#>   Variable     : Exam Score
#>   Groups       : n = 90  |  Hypothesised mean (mu) = 70.00
#> -----------------------------------------------------------------
#>   t-statistic  : 1.669
#>   df           : 89.0
#>   p-value      : 0.0987
#>   95% CI      : [69.599, 74.601]
#>   Cohen's d    : 0.176 (negligible effect)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant (p = 0.099 > alpha 0.05).
#>   Sample mean was 72.10 vs hypothesised 70.00.
#>   Effect size is negligible (d = 0.176).
#>   95% CI: true difference lies between 69.599 and 74.601.
#> 
#>   WARNING: Shapiro-Wilk suggests x may not be normally distributed.
#> -----------------------------------------------------------------
```

### Paired T-Test

Test whether students improved from pre-test to final exam:

``` r

result <- ttest_interpret(
  tutorial_data$exam_score,
  tutorial_data$pre_test,
  paired   = TRUE,
  var_name = "Score Improvement"
)
print(result)
#> 
#> -- statease T-Test Report ----------------------------------------
#>   Test         : Paired Samples T-Test
#>   Variable     : Score Improvement
#>   Groups       : n (pairs) = 90
#> -----------------------------------------------------------------
#>   t-statistic  : 10.296
#>   df           : 89.0
#>   p-value      : 0.0000
#>   95% CI      : [13.262, 19.605]
#>   Cohen's d    : 1.085 (large effect)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is statistically significant (p = 0.000 < alpha 0.05).
#>   Group 1 had a higher mean (72.10 vs 55.67).
#>   Effect size is large (d = 1.085).
#>   95% CI: true difference lies between 13.262 and 19.605.
#> 
#>   WARNING: Shapiro-Wilk suggests x may not be normally distributed.
#> -----------------------------------------------------------------
```

------------------------------------------------------------------------

## 3. One-Way ANOVA

Test whether teaching method affects exam scores:

``` r

result <- anova_interpret(
  exam_score ~ method,
  data = tutorial_data
)
print(result)
#> 
#> -- statease ANOVA Report -----------------------------------------
#>   Outcome      : exam_score
#>   Group        : method  (3 levels)
#> -----------------------------------------------------------------
#>   Group Means:
#>     Hybrid       : Mean = 79.90  (n = 30)
#>     Online       : Mean = 70.77  (n = 30)
#>     Traditional  : Mean = 65.63  (n = 30)
#> -----------------------------------------------------------------
#>   F-statistic  : 14.267
#>   df           : 2, 87
#>   p-value      : 0.0000
#>   Eta squared  : 0.2470 (large effect)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The overall ANOVA result is statistically significant (p = 0.0000 < alpha 0.05).
#>   Group differences explain 24.7% of variance
#>   (eta^2 = 0.2470, large effect).
#>   WARNING: Bartlett's test suggests unequal variances (p = 0.0421).
#> 
#> -- Post-Hoc Tukey HSD --------------------------------------------
#>   Online-Hybrid
#>     Mean diff = -9.133  |  p adj = 0.0031  |  [significant]
#>   Traditional-Hybrid
#>     Mean diff = -14.267  |  p adj = 0.0000  |  [significant]
#>   Traditional-Online
#>     Mean diff = -5.133  |  p adj = 0.1456  |  [not significant]
#> -----------------------------------------------------------------
#>   Note: Tukey HSD controls for family-wise error rate.
#> -----------------------------------------------------------------
```

------------------------------------------------------------------------

## 4. Two-Way ANOVA

Test the effect of both teaching method and gender on exam scores:

``` r

result <- anova2_interpret(
  exam_score ~ method * gender,
  data = tutorial_data
)
print(result)
#> 
#>  statease Two-Way ANOVA Report 
#>   Outcome      : exam_score
#>   Factor 1     : method
#>   Factor 2     : gender
#>   N            : 90
#> 
#>   Means by method:
#>     Hybrid          : 79.90
#>     Online          : 70.77
#>     Traditional     : 65.63
#> 
#>   Means by gender:
#>     Female          : 72.07
#>     Male            : 72.13
#> 
#>   Interaction Means:
#>             Female  Male
#> Hybrid       81.27 78.53
#> Online       71.60 69.93
#> Traditional  63.33 67.93
#> 
#>   ANOVA Results:
#>   method               : F = 14.123  df = 2,84  significant (p = 0.0000)  eta^2 = 0.2470 (large)
#>   gender               : F = 0.001  df = 1,84  not significant (p = 0.9761)  eta^2 = 0.0000 (negligible)
#>   Interaction          : F = 1.061  df = 2,84  not significant (p = 0.3506)  eta^2 = 0.0186 (small)
#> 
#>   Interpretation:
#>   Main effect of method is significant (p = 0.0000).
#>   Main effect of gender is not significant (p = 0.9761).
#>   Interaction (method x gender) is not significant (p = 0.3506).
#> 
#> -- Post-Hoc Tukey HSD (method) ------------------------------
#>   Online-Hybrid : diff = -9.133  p adj = 0.0033  [significant]
#>   Traditional-Hybrid : diff = -14.267  p adj = 0.0000  [significant]
#>   Traditional-Online : diff = -5.133  p adj = 0.1485  [not significant]
#> -----------------------------------------------------------------
```

------------------------------------------------------------------------

## 5. MANOVA

Test the combined effect of teaching method on both exam score and
pre-test score simultaneously:

``` r

result <- manova_interpret(
  cbind(exam_score, pre_test) ~ method,
  data = tutorial_data
)
print(result)
#> 
#> -- statease MANOVA Report ----------------------------------------
#>   Outcomes     : exam_score, pre_test
#>   Group        : method  (3 levels)
#>   N            : 90
#> -----------------------------------------------------------------
#>   Group Means:
#> 
#>   exam_score:
#>     Hybrid       : 79.90
#>     Online       : 70.77
#>     Traditional  : 65.63
#> 
#>   pre_test:
#>     Hybrid       : 56.87
#>     Online       : 55.37
#>     Traditional  : 54.77
#> -----------------------------------------------------------------
#>   Multivariate Test Results:
#>   Pillai's Trace : 0.2565
#>   Wilks' Lambda  : 0.7435
#>   F-statistic    : 6.400  (df = 4, 174)
#>   p-value        : 0.0001
#>   Effect size    : small (Pillai = 0.2565)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The overall MANOVA result is statistically significant (p = 0.0001 < alpha 0.05).
#>   Pillai's Trace = 0.2565 indicates a small effect.
#> -----------------------------------------------------------------
#>   Follow-Up Univariate ANOVAs:
#> 
#>   exam_score
#>     F = 14.267  (df = 2, 87)  p = 0.0000  [significant]
#> 
#>   pre_test
#>     F = 0.403  (df = 2, 87)  p = 0.6695  [not significant]
#> 
#>   Note: Follow-up ANOVAs identify which outcomes
#>   differ significantly across groups.
#> 
#>   WARNING: 'exam_score' may not be normally distributed (Shapiro-Wilk p = 0.0054).
#> -----------------------------------------------------------------
```

------------------------------------------------------------------------

## 6. Chi-Square Test

Test whether there is an association between teaching method and
pass/fail outcome:

``` r

tutorial_data$passed_label <- ifelse(tutorial_data$passed == 1,
                                      "Pass", "Fail")

result <- chisq_interpret(
  tutorial_data$method,
  tutorial_data$passed_label
)
print(result)
#> 
#> -- statease Chi-Square Test Report ------------------------------
#>   N            : 90
#> -----------------------------------------------------------------
#>   Contingency Table (Observed):
#>              y
#> x             Fail Pass
#>   Hybrid         6   24
#>   Online         8   22
#>   Traditional   10   20
#> 
#>   Expected Frequencies:
#>              y
#> x             Fail Pass
#>   Hybrid         8   22
#>   Online         8   22
#>   Traditional    8   22
#> 
#> -----------------------------------------------------------------
#>   Chi-square   : 1.364
#>   df           : 2
#>   p-value      : 0.5057
#>   Cramer's V   : 0.123 (small effect)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant (p = 0.5057 > alpha 0.05).
#>   There is no significant association between the two variables.
#>   Effect size is small (V = 0.123).
#> -----------------------------------------------------------------
```

------------------------------------------------------------------------

## 7. Correlation Analysis

Test the relationship between pre-test scores and exam scores:

``` r

result <- cor_interpret(
  tutorial_data$pre_test,
  tutorial_data$exam_score,
  var1_name = "Pre-Test Score",
  var2_name = "Exam Score"
)
print(result)
#> 
#> -- statease Correlation Report -----------------------------------
#>   Method       : Pearson Product-Moment Correlation
#>   Variables    : Pre-Test Score & Exam Score
#>   N            : 90  |  Missing: 0
#> -----------------------------------------------------------------
#>   r            : -0.0038
#>   p-value      : 0.9720
#>   95% CI      : [-0.2107, 0.2035]
#>   Strength     : negligible
#>   Direction    : negative (as one variable increases, the other tends to decrease)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The correlation is not statistically significant (p = 0.9720 > alpha 0.05).
#>   The relationship between Pre-Test Score and Exam Score is
#>   negligible and negative (as one variable increases, the other tends to decrease) in direction.
#> 
#>   WARNING: One or both variables may not be normally distributed. Consider using method = 'spearman' instead.
#> -----------------------------------------------------------------
```

### Spearman Correlation

``` r

result <- cor_interpret(
  tutorial_data$pre_test,
  tutorial_data$exam_score,
  method    = "spearman",
  var1_name = "Pre-Test Score",
  var2_name = "Exam Score"
)
#> Warning in cor.test.default(x, y, method = method, conf.level = conf.level):
#> cannot compute exact p-value with ties
print(result)
#> 
#> -- statease Correlation Report -----------------------------------
#>   Method       : Spearman Rank Correlation
#>   Variables    : Pre-Test Score & Exam Score
#>   N            : 90  |  Missing: 0
#> -----------------------------------------------------------------
#>   r            : 0.0750
#>   p-value      : 0.4826
#>   Strength     : negligible
#>   Direction    : positive (as one variable increases, the other tends to increase)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The correlation is not statistically significant (p = 0.4826 > alpha 0.05).
#>   The relationship between Pre-Test Score and Exam Score is
#>   negligible and positive (as one variable increases, the other tends to increase) in direction.
#> -----------------------------------------------------------------
```

------------------------------------------------------------------------

## 8. Simple Linear Regression

Predict exam score from pre-test score:

``` r

result <- reg_interpret(
  exam_score ~ pre_test,
  data = tutorial_data
)
print(result)
#> 
#> -- statease Simple Linear Regression Report ---------------------
#>   Outcome      : exam_score
#>   Predictor    : pre_test
#>   N            : 90
#> -----------------------------------------------------------------
#>   Model Equation:
#>   exam_score = 72.369 + -0.005 * pre_test
#> -----------------------------------------------------------------
#>   Coefficients:
#>   Intercept    : 72.369
#>   Slope        : -0.005  (SE = 0.137)
#>   t-statistic  : -0.035
#>   p-value      : 0.9720
#>   95% CI      : [-0.278, 0.268]
#> -----------------------------------------------------------------
#>   Model Fit:
#>   R-squared    : 0.0000
#>   Adj R-squared: -0.0113
#>   F-statistic  : 0.001 (df = 1, 88)  p = 0.9720
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The predictor pre_test is not statistically significant (p = 0.9720 > alpha 0.05).
#>   The slope is negative - as pre_test increases by 1 unit, exam_score decreases by 0.005 units.
#>   R-squared = 0.0000: pre_test explains 0.0% of the
#>   variance in exam_score (negligible effect).
#> 
#>   WARNING: Residuals may not be normally distributed (Shapiro-Wilk p = 0.0056).
#> -----------------------------------------------------------------
```

------------------------------------------------------------------------

## 9. Multiple Linear Regression

Predict exam score from both pre-test score and age:

``` r

result <- mlr_interpret(
  exam_score ~ pre_test + age,
  data = tutorial_data
)
print(result)
#> 
#> -- statease Multiple Linear Regression Report -------------------
#>   Outcome      : exam_score
#>   Predictors   : pre_test, age
#>   N            : 90
#> -----------------------------------------------------------------
#>   Model Equation:
#>   exam_score = 70.019 + 0.001*pre_test + 0.093*age
#> -----------------------------------------------------------------
#>   Overall Model Fit:
#>   R-squared    : 0.0005 (negligible effect)
#>   Adj R-squared: -0.0225
#>   F-statistic  : 0.022 (df = 2, 87)  p = 0.9782
#>   The overall model is not statistically significant (p = 0.9782 > alpha 0.05).
#> -----------------------------------------------------------------
#>   Individual Predictors:
#> 
#>   pre_test
#>     Coefficient  : 0.001  (SE = 0.141)
#>     t-statistic  : 0.008
#>     p-value      : 0.9936  [not significant]
#>     95% CI      : [-0.279, 0.282]
#>     Direction    : positive (b = 0.001)
#> 
#>   age
#>     Coefficient  : 0.093  (SE = 0.447)
#>     t-statistic  : 0.207
#>     p-value      : 0.8366  [not significant]
#>     95% CI      : [-0.796, 0.982]
#>     Direction    : positive (b = 0.093)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The model explains 0.1% of the variance in exam_score
#>   (R-squared = 0.0005, negligible effect).
#>   Adjusted R-squared = -0.0225 accounting for
#>   the number of predictors in the model.
#>   Non-significant predictors: pre_test, age
#> 
#>   WARNING: Residuals may not be normally distributed (Shapiro-Wilk p = 0.0048).
#> -----------------------------------------------------------------
```

------------------------------------------------------------------------

## 10. Logistic Regression

Predict pass/fail outcome from pre-test score and age:

``` r

result <- logistic_interpret(
  passed ~ pre_test + age,
  data = tutorial_data
)
#> Waiting for profiling to be done...
print(result)
#> 
#> -- statease Logistic Regression Report --------------------------
#>   Outcome      : passed
#>   Predictors   : pre_test, age
#>   N            : 90
#> -----------------------------------------------------------------
#>   Overall Model Fit:
#>   Chi-square   : 3.668  (df = 2)  p = 0.1598
#>   Nagelkerke R2: 0.0582 (small effect)
#>   The overall model is not statistically significant (p = 0.1598 > alpha 0.05).
#> -----------------------------------------------------------------
#>   Individual Predictors:
#> 
#>   pre_test
#>     Coefficient  : -0.050  (SE = 0.027)
#>     z-statistic  : -1.842
#>     p-value      : 0.0655  [not significant]
#>     Odds Ratio   : 0.951
#>     95% CI (OR) : [0.899, 1.002]
#>     Interpretation: each unit increase in pre_test decreases the odds by 4.9%.
#> 
#>   age
#>     Coefficient  : -0.006  (SE = 0.086)
#>     z-statistic  : -0.065
#>     p-value      : 0.9482  [not significant]
#>     Odds Ratio   : 0.994
#>     95% CI (OR) : [0.838, 1.180]
#>     Interpretation: each unit increase in age decreases the odds by 0.6%.
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The model is not statistically significant (p = 0.1598 > alpha 0.05).
#>   Nagelkerke R2 = 0.0582 suggests a small amount of
#>   variance in passed is explained by the predictors.
#>   Non-significant predictors: pre_test, age
#> -----------------------------------------------------------------
```

------------------------------------------------------------------------

## 11. Non-Parametric Tests

### Mann-Whitney U Test

Non-parametric alternative to the independent samples t-test:

``` r

result <- mannwhitney_interpret(
  males, females,
  var_name = "Exam Score by Gender"
)
print(result)
#> 
#> -- statease Mann-Whitney U Test Report --------------------------
#>   Variable     : Exam Score by Gender
#>   Group 1      : n = 45  |  Median = 74.00
#>   Group 2      : n = 45  |  Median = 75.00
#> -----------------------------------------------------------------
#>   W statistic  : 1026.500
#>   p-value      : 0.9120
#>   95% CI      : [-5.000, 5.000]
#>   Effect size  : 0.012 (negligible)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant (p = 0.9120 > alpha 0.05).
#>   Group 2 had a higher median (75.00 vs 74.00).
#>   Effect size is negligible (r = 0.012).
#> -----------------------------------------------------------------
```

### Wilcoxon Signed Rank Test

Non-parametric alternative to the paired t-test:

``` r

result <- wilcoxon_interpret(
  tutorial_data$exam_score,
  tutorial_data$pre_test,
  var_name = "Score Improvement"
)
print(result)
#> 
#> -- statease Wilcoxon Signed Rank Test Report --------------------
#>   Variable     : Score Improvement
#>   N (pairs)    : 90
#>   Pre Median   : 55.00
#>   Post Median  : 74.00
#> -----------------------------------------------------------------
#>   V statistic  : 3711.500
#>   p-value      : 0.0000
#>   95% CI      : [14.000, 20.000]
#>   Effect size  : 0.737 (large)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is statistically significant (p = 0.0000 < alpha 0.05).
#>   Post-measurement median higher (74.00 vs 55.00).
#>   Effect size is large (r = 0.737).
#> -----------------------------------------------------------------
```

### Kruskal-Wallis Test

Non-parametric alternative to one-way ANOVA:

``` r

result <- kruskal_interpret(
  exam_score ~ method,
  data = tutorial_data
)
print(result)
#> 
#> -- statease Kruskal-Wallis Test Report --------------------------
#>   Outcome      : exam_score
#>   Group        : method  (3 levels)
#>   N            : 90
#> -----------------------------------------------------------------
#>   Group Medians:
#>     Hybrid       : Median = 81.00  (n = 30)
#>     Online       : Median = 73.50  (n = 30)
#>     Traditional  : Median = 64.00  (n = 30)
#> -----------------------------------------------------------------
#>   H statistic  : 23.052
#>   df           : 2
#>   p-value      : 0.0000
#>   Eta squared  : 0.2420 (large effect)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is statistically significant (p = 0.0000 < alpha 0.05).
#>   Effect size is large (eta^2 = 0.2420).
#> 
#> -- Post-Hoc Pairwise Comparisons (Wilcoxon) ---------------------
#>   Hybrid vs Online
#>     p = 0.0006  [significant]
#>   Hybrid vs Traditional
#>     p = 0.0000  [significant]
#>   Online vs Traditional
#>     p = 0.0653  [not significant]
#> 
#>   Note: Pairwise Wilcoxon tests used for post-hoc comparisons.
#> -----------------------------------------------------------------
```

------------------------------------------------------------------------

## 12. P-Value Interpretation

Interpret any p-value in plain English:

``` r

result <- interpret_p(
  0.03,
  context = "teaching method effect on exam scores"
)
print(result)
#> 
#> -- statease P-Value Interpretation ------------------------------
#>   Context      : teaching method effect on exam scores
#>   P-value      : 0.0300
#>   Alpha        : 0.05
#> -----------------------------------------------------------------
#>   Decision     : REJECT the null hypothesis at alpha = 0.05
#>   Evidence     : There is moderate evidence against the null hypothesis.
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is statistically significant. The observed data is unlikely to have occurred by chance if the null hypothesis were true.
#> 
#>   Note: Statistical significance does not imply practical importance. Always consider effect size alongside the p-value.
#> -----------------------------------------------------------------
```

------------------------------------------------------------------------

## 13. The Master Function

The
[`analyze()`](https://devwebwacky.github.io/statease/reference/analyze.md)
function automatically detects the right test based on your input — no
need to remember which function to use!

``` r

# Descriptive statistics
analyze(x = tutorial_data$exam_score, var_name = "Exam Score")
#> [statease] Single numeric vector -> Running Descriptive Statistics
#> 
#> -- statease Descriptive Report ----------------------------------
#>   Variable     : Exam Score
#>   N            : 90  |  Missing: 0
#> -----------------------------------------------------------------
#>   Mean         : 72.10
#>   Median       : 74.00
#>   Std Dev      : 11.94
#>   Min          : 38.00  |  Max: 93.00
#>   Q1           : 64.00  |  Q3: 80.00
#>   IQR          : 16.00
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The distribution is approximately symmetric.
#>   Spread shows moderate variability (CV = 16.6%).
#>   Shapiro-Wilk test suggests non-normality (W = 0.958, p = 0.0054).
#> -----------------------------------------------------------------
```

``` r

# Auto t-test
analyze(
  x        = males,
  y        = females,
  var_name = "Exam Score by Gender"
)
#> [statease] Two numeric vectors detected -> Running T-Test
#> 
#> -- statease T-Test Report ----------------------------------------
#>   Test         : Independent Samples T-Test
#>   Variable     : Exam Score by Gender
#>   Groups       : Group 1: n = 45  |  Group 2: n = 45
#> -----------------------------------------------------------------
#>   t-statistic  : 0.026
#>   df           : 88.0
#>   p-value      : 0.9790
#>   95% CI      : [-4.964, 5.097]
#>   Cohen's d    : 0.006 (negligible effect)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant (p = 0.979 > alpha 0.05).
#>   Group 1 had a higher mean (72.13 vs 72.07).
#>   Effect size is negligible (d = 0.006).
#>   95% CI: true difference lies between -4.964 and 5.097.
#> -----------------------------------------------------------------
```

``` r

# Auto ANOVA
analyze(formula = exam_score ~ method, data = tutorial_data)
#> [statease] 3+ groups detected -> Running One-Way ANOVA
#> 
#> -- statease ANOVA Report -----------------------------------------
#>   Outcome      : exam_score
#>   Group        : method  (3 levels)
#> -----------------------------------------------------------------
#>   Group Means:
#>     Hybrid       : Mean = 79.90  (n = 30)
#>     Online       : Mean = 70.77  (n = 30)
#>     Traditional  : Mean = 65.63  (n = 30)
#> -----------------------------------------------------------------
#>   F-statistic  : 14.267
#>   df           : 2, 87
#>   p-value      : 0.0000
#>   Eta squared  : 0.2470 (large effect)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The overall ANOVA result is statistically significant (p = 0.0000 < alpha 0.05).
#>   Group differences explain 24.7% of variance
#>   (eta^2 = 0.2470, large effect).
#>   WARNING: Bartlett's test suggests unequal variances (p = 0.0421).
#> 
#> -- Post-Hoc Tukey HSD --------------------------------------------
#>   Online-Hybrid
#>     Mean diff = -9.133  |  p adj = 0.0031  |  [significant]
#>   Traditional-Hybrid
#>     Mean diff = -14.267  |  p adj = 0.0000  |  [significant]
#>   Traditional-Online
#>     Mean diff = -5.133  |  p adj = 0.1456  |  [not significant]
#> -----------------------------------------------------------------
#>   Note: Tukey HSD controls for family-wise error rate.
#> -----------------------------------------------------------------
```

``` r

# Auto non-parametric
analyze(
  formula  = exam_score ~ method,
  data     = tutorial_data,
  nonparam = TRUE
)
#> [statease] 3+ groups + nonparam = TRUE -> Running Kruskal-Wallis Test
#> 
#> -- statease Kruskal-Wallis Test Report --------------------------
#>   Outcome      : exam_score
#>   Group        : method  (3 levels)
#>   N            : 90
#> -----------------------------------------------------------------
#>   Group Medians:
#>     Hybrid       : Median = 81.00  (n = 30)
#>     Online       : Median = 73.50  (n = 30)
#>     Traditional  : Median = 64.00  (n = 30)
#> -----------------------------------------------------------------
#>   H statistic  : 23.052
#>   df           : 2
#>   p-value      : 0.0000
#>   Eta squared  : 0.2420 (large effect)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is statistically significant (p = 0.0000 < alpha 0.05).
#>   Effect size is large (eta^2 = 0.2420).
#> 
#> -- Post-Hoc Pairwise Comparisons (Wilcoxon) ---------------------
#>   Hybrid vs Online
#>     p = 0.0006  [significant]
#>   Hybrid vs Traditional
#>     p = 0.0000  [significant]
#>   Online vs Traditional
#>     p = 0.0653  [not significant]
#> 
#>   Note: Pairwise Wilcoxon tests used for post-hoc comparisons.
#> -----------------------------------------------------------------
```

``` r

# Auto regression
analyze(formula = exam_score ~ pre_test, data = tutorial_data)
#> [statease] Numeric predictor detected -> Running Simple Linear Regression
#> 
#> -- statease Simple Linear Regression Report ---------------------
#>   Outcome      : exam_score
#>   Predictor    : pre_test
#>   N            : 90
#> -----------------------------------------------------------------
#>   Model Equation:
#>   exam_score = 72.369 + -0.005 * pre_test
#> -----------------------------------------------------------------
#>   Coefficients:
#>   Intercept    : 72.369
#>   Slope        : -0.005  (SE = 0.137)
#>   t-statistic  : -0.035
#>   p-value      : 0.9720
#>   95% CI      : [-0.278, 0.268]
#> -----------------------------------------------------------------
#>   Model Fit:
#>   R-squared    : 0.0000
#>   Adj R-squared: -0.0113
#>   F-statistic  : 0.001 (df = 1, 88)  p = 0.9720
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The predictor pre_test is not statistically significant (p = 0.9720 > alpha 0.05).
#>   The slope is negative - as pre_test increases by 1 unit, exam_score decreases by 0.005 units.
#>   R-squared = 0.0000: pre_test explains 0.0% of the
#>   variance in exam_score (negligible effect).
#> 
#>   WARNING: Residuals may not be normally distributed (Shapiro-Wilk p = 0.0056).
#> -----------------------------------------------------------------
```

``` r

# Auto MANOVA
analyze(
  formula = cbind(exam_score, pre_test) ~ method,
  data    = tutorial_data
)
#> [statease] Multiple outcomes detected -> Running MANOVA
#> 
#> -- statease MANOVA Report ----------------------------------------
#>   Outcomes     : exam_score, pre_test
#>   Group        : method  (3 levels)
#>   N            : 90
#> -----------------------------------------------------------------
#>   Group Means:
#> 
#>   exam_score:
#>     Hybrid       : 79.90
#>     Online       : 70.77
#>     Traditional  : 65.63
#> 
#>   pre_test:
#>     Hybrid       : 56.87
#>     Online       : 55.37
#>     Traditional  : 54.77
#> -----------------------------------------------------------------
#>   Multivariate Test Results:
#>   Pillai's Trace : 0.2565
#>   Wilks' Lambda  : 0.7435
#>   F-statistic    : 6.400  (df = 4, 174)
#>   p-value        : 0.0001
#>   Effect size    : small (Pillai = 0.2565)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The overall MANOVA result is statistically significant (p = 0.0001 < alpha 0.05).
#>   Pillai's Trace = 0.2565 indicates a small effect.
#> -----------------------------------------------------------------
#>   Follow-Up Univariate ANOVAs:
#> 
#>   exam_score
#>     F = 14.267  (df = 2, 87)  p = 0.0000  [significant]
#> 
#>   pre_test
#>     F = 0.403  (df = 2, 87)  p = 0.6695  [not significant]
#> 
#>   Note: Follow-up ANOVAs identify which outcomes
#>   differ significantly across groups.
#> 
#>   WARNING: 'exam_score' may not be normally distributed (Shapiro-Wilk p = 0.0054).
#> -----------------------------------------------------------------
```

------------------------------------------------------------------------

## Summary

| Test | Function | analyze() support |
|----|----|----|
| Descriptive stats | [`describe()`](https://devwebwacky.github.io/statease/reference/describe.md) | ✔ |
| Independent t-test | [`ttest_interpret()`](https://devwebwacky.github.io/statease/reference/ttest_interpret.md) | ✔ |
| One-sample t-test | [`ttest_interpret()`](https://devwebwacky.github.io/statease/reference/ttest_interpret.md) | ✔ |
| Paired t-test | [`ttest_interpret()`](https://devwebwacky.github.io/statease/reference/ttest_interpret.md) | ✔ |
| One-way ANOVA | [`anova_interpret()`](https://devwebwacky.github.io/statease/reference/anova_interpret.md) | ✔ |
| Two-way ANOVA | [`anova2_interpret()`](https://devwebwacky.github.io/statease/reference/anova2_interpret.md) | ✔ |
| MANOVA | [`manova_interpret()`](https://devwebwacky.github.io/statease/reference/manova_interpret.md) | ✔ |
| Chi-square | [`chisq_interpret()`](https://devwebwacky.github.io/statease/reference/chisq_interpret.md) | ✔ |
| Correlation | [`cor_interpret()`](https://devwebwacky.github.io/statease/reference/cor_interpret.md) | ✔ |
| Simple regression | [`reg_interpret()`](https://devwebwacky.github.io/statease/reference/reg_interpret.md) | ✔ |
| Multiple regression | [`mlr_interpret()`](https://devwebwacky.github.io/statease/reference/mlr_interpret.md) | ✔ |
| Logistic regression | [`logistic_interpret()`](https://devwebwacky.github.io/statease/reference/logistic_interpret.md) | ✔ |
| Mann-Whitney U | [`mannwhitney_interpret()`](https://devwebwacky.github.io/statease/reference/mannwhitney_interpret.md) | ✔ |
| Wilcoxon Signed Rank | [`wilcoxon_interpret()`](https://devwebwacky.github.io/statease/reference/wilcoxon_interpret.md) | ✔ |
| Kruskal-Wallis | [`kruskal_interpret()`](https://devwebwacky.github.io/statease/reference/kruskal_interpret.md) | ✔ |
| P-value interpretation | [`interpret_p()`](https://devwebwacky.github.io/statease/reference/interpret_p.md) | \- |
