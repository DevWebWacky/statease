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
  method = "pearson",
  test_type = NULL,
  effect_size = NULL,
  power = 0.8,
  n_groups = 2,
  n_predictors = 1,
  check = FALSE
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

- test_type:

  For power analysis: one of "ttest.one", "ttest.two", "ttest.paired",
  "anova", "correlation", "chisq", "regression".

- effect_size:

  For power analysis: the expected effect size.

- power:

  For power analysis: desired power level. Default 0.80.

- n_groups:

  For power analysis ANOVA: number of groups. Default 2.

- n_predictors:

  For power analysis regression: number of predictors. Default 1.

- check:

  Logical. TRUE to run assumption checks before analysis. Default FALSE.

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

# Auto ANOVA
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

# Power analysis
analyze(test_type = "ttest.two", effect_size = 0.5)
#> [statease] Power analysis requested -> Running Power Analysis
#> 
#> -- statease Power Analysis Report  
#>   Test         : Independent Samples T-Test
#>   Mode         : Calculate required sample size
#> -----------------------------------------------------------------
#>   Effect size  : 0.500 (large)
#>   Alpha        : 0.05
#>   Desired power: 0.80 (80%)
#>   Required n   : 64
#>   Total N      : 128 (2 groups x 64)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   To detect a large effect (effect size = 0.50) with 80% power at alpha = 0.05, you need at least 64 participants per group (128 total for 2 groups).
#> 
#>   NOTE: Power analysis results are estimates based on assumptions about effect size, alpha, and power. Actual results may differ depending on the true effect size in the population.
#>   NOTE: Effect sizes should ideally be based on previous research, pilot studies, or theoretically justified values — not chosen arbitrarily to reduce required sample size.
#>   NOTE: A power of 0.80 is a conventional minimum. In high stakes research such as clinical trials, a higher power of 0.90 or 0.95 is often recommended.
#>   NOTE: Power analysis assumes that the chosen statistical test and its assumptions are appropriate for the data.
#> -----------------------------------------------------------------
#> 

# Check assumptions
analyze(x = c(23,45,12,67,34), y = c(19,38,22,51,29),
        check = TRUE)
#> [statease] check = TRUE -> Checking assumptions for ttest
#> 
#> -- statease Assumption Check Report -------------------------------
#>   Test         : ttest
#> ---------------------------------------------------------------------
#> 
#>   [PASSED] Normality (x)
#>     Shapiro-Wilk test: statistic = 0.979, p = 0.9276. Normality assumption appears satisfied.
#> 
#>   [WARNING] Sample size guidance (x)
#>     n = 5. Small sample size. There is no formal assumption of sample size adequacy, but results should be interpreted with caution.
#> 
#>   [PASSED] Normality (y)
#>     Shapiro-Wilk test: statistic = 0.936, p = 0.6369. Normality assumption appears satisfied.
#> 
#>   [WARNING] Sample size guidance (y)
#>     n = 5. Small sample size. There is no formal assumption of sample size adequacy, but results should be interpreted with caution.
#> 
#>   [PASSED] Homogeneity of variance
#>     Levene's Test: p = 0.4080. Variances appear approximately equal.
#> 
#> ---------------------------------------------------------------------
#>   NOTE: Assumption checks are based on statistical tests and
#>   heuristics. They provide guidance but should not be
#>   interpreted as definitive proof that assumptions are met
#>   or violated.
#> 
#>   NOTE: Failure to reject an assumption test does not prove
#>   that the assumption has been satisfied.
#> 
#>   NOTE: Visual inspection of residual plots is always
#>   recommended alongside formal assumption tests.
#> ---------------------------------------------------------------------
#> 
```
