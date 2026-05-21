# One-Way ANOVA with Post-Hoc Tukey and Plain-English Interpretation

One-Way ANOVA with Post-Hoc Tukey and Plain-English Interpretation

## Usage

``` r
anova_interpret(formula, data, conf.level = 0.95)
```

## Arguments

- formula:

  A formula of the form outcome ~ group

- data:

  A data frame containing the variables

- conf.level:

  Confidence level. Default 0.95

## Value

An object of class `statease_anova` containing ANOVA results, effect
size, and post-hoc comparisons. Use
[`print()`](https://rdrr.io/r/base/print.html) to display the formatted
report.

## Examples

``` r
df <- data.frame(
  score = c(23,45,12,67,34,89,56,43,78,90,11,34),
  group = rep(c("A","B","C"), each = 4)
)
result <- anova_interpret(score ~ group, data = df)
#> Warning: One or more groups have small sample sizes (n < 10). Interpret with caution.
print(result)
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
```
