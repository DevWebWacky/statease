# Kruskal-Wallis Test with Plain-English Interpretation

Kruskal-Wallis Test with Plain-English Interpretation

## Usage

``` r
kruskal_interpret(formula, data, conf.level = 0.95)
```

## Arguments

- formula:

  A formula of the form outcome ~ group

- data:

  A data frame containing the variables

- conf.level:

  Confidence level. Default 0.95.

## Value

An object of class `statease_kruskal` containing test results and
interpretation. Use [`print()`](https://rdrr.io/r/base/print.html) to
display the formatted report.

## Examples

``` r
df <- data.frame(
  score = c(23,45,12,67,34,89,56,43,78,90,11,34),
  group = rep(c("A","B","C"), each = 4)
)
result <- kruskal_interpret(score ~ group, data = df)
#> Warning: One or more groups have very small sample sizes (n < 5).
print(result)
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
#>   Note: Kruskal-Wallis test compares multiple groups using ranked values
#>   not differences in medians.
#>   Medians are reported for descriptive purposes only.
#> 
#>   Post-hoc tests not run (overall result not significant).
#> -----------------------------------------------------------------
#> 
```
