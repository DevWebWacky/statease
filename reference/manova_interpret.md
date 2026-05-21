# MANOVA with Plain-English Interpretation

MANOVA with Plain-English Interpretation

## Usage

``` r
manova_interpret(formula, data, conf.level = 0.95)
```

## Arguments

- formula:

  A formula of the form cbind(outcome1, outcome2, ...) ~ group

- data:

  A data frame containing the variables

- conf.level:

  Confidence level. Default 0.95.

## Value

An object of class `statease_manova` containing MANOVA results and
interpretation. Use [`print()`](https://rdrr.io/r/base/print.html) to
display the formatted report.

## Examples

``` r
df <- data.frame(
  math    = c(23,45,12,67,34,89,56,43,78,90,11,34),
  english = c(34,56,23,78,45,90,67,54,89,95,22,45),
  group   = rep(c("A","B","C"), each = 4)
)
result <- manova_interpret(cbind(math, english) ~ group, data = df)
#> Warning: Sample size is small (n < 20). Interpret results with caution.
print(result)
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
```
