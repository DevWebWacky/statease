# Check Statistical Assumptions Before Running a Test

Check Statistical Assumptions Before Running a Test

## Usage

``` r
check_assumptions(test, x = NULL, y = NULL, data = NULL, formula = NULL)
```

## Arguments

- test:

  The test you plan to run. One of "ttest", "anova", "anova2",
  "correlation", "regression".

- x:

  A numeric vector (required for most tests)

- y:

  A numeric vector, factor, or character group variable (optional
  depending on test)

- data:

  A data frame (required for anova, anova2, regression)

- formula:

  A formula (required for anova, anova2, regression)

## Value

An object of class `statease_assumptions` containing assumption check
results. Use [`print()`](https://rdrr.io/r/base/print.html) to display
the formatted report.

## Examples

``` r
x <- c(23, 45, 12, 67, 34, 89, 56, 43, 78, 90)
y <- c(19, 38, 22, 51, 29, 74, 44, 38, 65, 80)
result <- check_assumptions("ttest", x = x, y = y)
print(result)
#> 
#> -- statease Assumption Check Report -------------------------------
#>   Test         : ttest
#> ---------------------------------------------------------------------
#> 
#>   [PASSED] Normality (x)
#>     Shapiro-Wilk test: statistic = 0.953, p = 0.7030. Normality assumption appears satisfied.
#> 
#>   [PASSED] Sample size guidance (x)
#>     n = 10. Sample size appears reasonable.
#> 
#>   [PASSED] Normality (y)
#>     Shapiro-Wilk test: statistic = 0.938, p = 0.5330. Normality assumption appears satisfied.
#> 
#>   [PASSED] Sample size guidance (y)
#>     n = 10. Sample size appears reasonable.
#> 
#>   [PASSED] Homogeneity of variance
#>     Levene's Test: p = 0.3689. Variances appear approximately equal.
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
