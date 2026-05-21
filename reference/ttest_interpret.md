# T-Test with Plain-English Interpretation

T-Test with Plain-English Interpretation

## Usage

``` r
ttest_interpret(
  x,
  y = NULL,
  mu = 0,
  paired = FALSE,
  conf.level = 0.95,
  var_name = "Variable"
)
```

## Arguments

- x:

  A numeric vector (group 1, or the only group for one-sample)

- y:

  A numeric vector (group 2, for independent samples). Default NULL.

- mu:

  Hypothesised mean for one-sample t-test. Default 0.

- paired:

  Logical. TRUE for paired t-test. Default FALSE.

- conf.level:

  Confidence level. Default 0.95.

- var_name:

  Optional label for the report. Default "Variable"

## Value

An object of class `statease_ttest` containing test results and
interpretation. Use [`print()`](https://rdrr.io/r/base/print.html) to
display the formatted report.

## Examples

``` r
result <- ttest_interpret(c(23,45,12,67,34), c(19,38,22,51,29))
#> Warning: Sample size in x is small (n < 10). Interpret results with caution.
#> Warning: Sample size in y is small (n < 10). Interpret results with caution.
print(result)
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
```
