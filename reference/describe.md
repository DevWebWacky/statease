# Descriptive Statistics with Interpretation

Descriptive Statistics with Interpretation

## Usage

``` r
describe(x, var_name = "Variable")
```

## Arguments

- x:

  A numeric vector

- var_name:

  Optional name for the variable (used in the report)

## Value

An object of class `statease_describe` containing descriptive statistics
and interpretation. Use [`print()`](https://rdrr.io/r/base/print.html)
to display the formatted report.

## Examples

``` r
result <- describe(c(23, 45, 12, 67, 34, 89, 56))
#> Warning: Sample size is small (n < 10). Interpret descriptive statistics with caution.
print(result)
#> 
#> -- statease Descriptive Report ----------------------------------
#>   Variable     : Variable
#>   N            : 7  |  Missing: 0
#> -----------------------------------------------------------------
#>   Mean         : 46.57
#>   Median       : 45.00
#>   Std Dev      : 26.51
#>   Min          : 12.00  |  Max: 89.00
#>   Q1           : 28.50  |  Q3: 61.50
#>   IQR          : 33.00
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The distribution is approximately symmetric.
#>   Spread shows high variability (CV = 56.9%).
#>   Shapiro-Wilk test suggests normality is reasonable (W = 0.984, p = 0.9760).
#> -----------------------------------------------------------------
#> 
```
