# Power Analysis with Plain English Interpretation

Power Analysis with Plain English Interpretation

## Usage

``` r
power_interpret(
  test,
  effect_size,
  n = NULL,
  alpha = 0.05,
  power = 0.8,
  n_groups = 2,
  n_predictors = 1
)
```

## Arguments

- test:

  The statistical test. One of "ttest.one", "ttest.two", "ttest.paired",
  "anova", "correlation", "chisq", "regression".

- effect_size:

  The expected effect size. Use Cohen's conventions: small = 0.2, medium
  = 0.5, large = 0.8 for t-tests; small = 0.10, medium = 0.25, large =
  0.40 for ANOVA; small = 0.10, medium = 0.30, large = 0.50 for
  correlation.

- n:

  Sample size per group. If provided, calculates achieved power. If
  NULL, calculates required sample size.

- alpha:

  Significance level. Default 0.05.

- power:

  Desired power level. Default 0.80.

- n_groups:

  Number of groups (for ANOVA only). Default 2.

- n_predictors:

  Number of predictors (for regression only). Default 1.

## Value

An object of class `statease_power` containing power analysis results
and interpretation. Use [`print()`](https://rdrr.io/r/base/print.html)
to display the formatted report.

## Examples

``` r
# Calculate required sample size for independent t-test
result <- power_interpret("ttest.two", effect_size = 0.5)
print(result)
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

# Calculate achieved power for given sample size
result2 <- power_interpret("ttest.two", effect_size = 0.5, n = 30)
print(result2)
#> 
#> -- statease Power Analysis Report  
#>   Test         : Independent Samples T-Test
#>   Mode         : Calculate achieved power
#> -----------------------------------------------------------------
#>   Effect size  : 0.500 (large)
#>   Alpha        : 0.05
#>   Sample size  : 30
#>   Achieved power: 0.4779 (47.8%)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   With n = 30 and a large effect size (0.50), the achieved power is 47.8%.
#> 
#>   WARNING: Power is less than 0.80. The study may be underpowered — there is a meaningful risk of failing to detect a true effect (Type II error). Consider increasing the sample size.
#>   WARNING: Post-hoc (observed) power calculations should be interpreted cautiously, as they are largely determined by the observed p-value and effect size.
#> 
#>   NOTE: Power analysis results are estimates based on assumptions about effect size, alpha, and power. Actual results may differ depending on the true effect size in the population.
#>   NOTE: Effect sizes should ideally be based on previous research, pilot studies, or theoretically justified values — not chosen arbitrarily to reduce required sample size.
#>   NOTE: A power of 0.80 is a conventional minimum. In high stakes research such as clinical trials, a higher power of 0.90 or 0.95 is often recommended.
#>   NOTE: Power analysis assumes that the chosen statistical test and its assumptions are appropriate for the data.
#> -----------------------------------------------------------------
#> 
```
