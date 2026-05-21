# Wilcoxon Signed Rank Test with Plain-English Interpretation

Wilcoxon Signed Rank Test with Plain-English Interpretation

## Usage

``` r
wilcoxon_interpret(x, y, conf.level = 0.95, var_name = "Variable")
```

## Arguments

- x:

  A numeric vector (first measurement)

- y:

  A numeric vector (second measurement)

- conf.level:

  Confidence level. Default 0.95.

- var_name:

  Optional label for the report. Default "Variable"

## Value

An object of class `statease_wilcoxon` containing test results and
interpretation. Use [`print()`](https://rdrr.io/r/base/print.html) to
display the formatted report.

## Examples

``` r
x <- c(23, 45, 12, 67, 34, 89, 56)
y <- c(19, 38, 22, 51, 29, 74, 44)
result <- wilcoxon_interpret(x, y)
#> Warning: Sample size is small (n < 10). Interpret results with caution.
print(result)
#> 
#> -- statease Wilcoxon Signed Rank Test Report --------------------
#>   Variable     : Variable
#>   N (pairs)    : 7
#>   Pre Median   : 38.00
#>   Post Median  : 45.00
#> -----------------------------------------------------------------
#>   V statistic  : 24.000
#>   p-value      : 0.1094
#>   95% CI      : [-2.500, 15.000]
#>   Effect size  : 0.605 (large)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant (p = 0.1094 > alpha 0.05).
#>   Post-measurement median higher (45.00 vs 38.00).
#>   Effect size is large (r = 0.605).
#> -----------------------------------------------------------------
#> 
```
