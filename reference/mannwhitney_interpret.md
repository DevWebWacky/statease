# Mann-Whitney U Test with Plain-English Interpretation

Mann-Whitney U Test with Plain-English Interpretation

## Usage

``` r
mannwhitney_interpret(x, y, conf.level = 0.95, var_name = "Variable")
```

## Arguments

- x:

  A numeric vector (group 1)

- y:

  A numeric vector (group 2)

- conf.level:

  Confidence level. Default 0.95.

- var_name:

  Optional label for the report. Default "Variable"

## Value

An object of class `statease_mannwhitney` containing test results and
interpretation. Use [`print()`](https://rdrr.io/r/base/print.html) to
display the formatted report.

## Examples

``` r
x <- c(23, 45, 12, 67, 34, 89, 56)
y <- c(19, 38, 22, 51, 29, 74, 44)
result <- mannwhitney_interpret(x, y)
#> Warning: Sample size is small (n < 10). Interpret results with caution.
print(result)
#> 
#> -- statease Mann-Whitney U Test Report --------------------------
#>   Variable     : Variable
#>   Group 1      : n = 7  |  Median = 45.00
#>   Group 2      : n = 7  |  Median = 38.00
#> -----------------------------------------------------------------
#>   W statistic  : 29.000
#>   p-value      : 0.6200
#>   95% CI      : [-21.000, 38.000]
#>   Effect size  : 0.133 (small)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant (p = 0.6200 > alpha 0.05).
#>   Group 1 had a higher median (45.00 vs 38.00).
#>   Effect size is small (r = 0.133).
#> -----------------------------------------------------------------
#> 
```
