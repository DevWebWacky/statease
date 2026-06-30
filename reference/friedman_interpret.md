# Friedman Test with Plain-English Interpretation

Friedman Test with Plain-English Interpretation

## Usage

``` r
friedman_interpret(formula, data, conf.level = 0.95)
```

## Arguments

- formula:

  A formula of the form outcome ~ time \| subject

- data:

  A data frame containing the variables

- conf.level:

  Confidence level. Default 0.95.

## Value

An object of class `statease_friedman` containing test results and
interpretation. Use [`print()`](https://rdrr.io/r/base/print.html) to
display the formatted report.

## Examples

``` r
df <- data.frame(
  score   = c(23,45,12,67,34,89,56,43,78,90,11,34),
  time    = rep(c("T1","T2","T3"), each = 4),
  subject = rep(1:4, times = 3)
)
result <- friedman_interpret(score ~ time | subject, data = df)
#> Warning: The Friedman Test may have low statistical power with very small sample sizes. Interpret non-significant results with caution.
print(result)
#> 
#> -- statease Friedman Test Report 
#>   Outcome      : score
#>   Time/Group   : time (3 levels)
#>   Subjects     : subject (n = 4)
#> -----------------------------------------------------------------
#>   Group Medians (descriptive only):
#>     T1           : 34.00
#>     T2           : 49.50
#>     T3           : 56.00
#> -----------------------------------------------------------------
#>   Chi-square   : 0.500
#>   df           : 2
#>   p-value      : 0.7788
#>   Kendall's W  : 0.0625 (negligible effect)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant (p = 0.7788 > alpha 0.05).
#>   There is insufficient evidence of a significant difference in ranks across the related groups or repeated measurements.
#> 
#>   NOTE: Medians are reported for descriptive purposes only.
#>   The Friedman Test assesses whether rank distributions
#>   differ across groups and does not directly test for
#>   differences in medians.
#> 
#>   Post-hoc tests not run (overall result not significant).
#> 
#>   WARNING: The Friedman Test assumes that the blocks (subjects) are independent of each other. Violation of this assumption may affect the validity of the results.
#> 
#>   NOTE: Normality assumption appears reasonable. If the assumptions of repeated measures ANOVA are met, consider using repeated measures ANOVA for greater statistical power.
#> -----------------------------------------------------------------
#> 
```
