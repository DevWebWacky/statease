# McNemar's Test with Plain-English Interpretation

McNemar's Test with Plain-English Interpretation

## Usage

``` r
mcnemar_interpret(x, y, conf.level = 0.95)
```

## Arguments

- x:

  A factor or character vector (first measurement)

- y:

  A factor or character vector (second measurement)

- conf.level:

  Confidence level. Default 0.95.

## Value

An object of class `statease_mcnemar` containing test results and
interpretation. Use [`print()`](https://rdrr.io/r/base/print.html) to
display the formatted report.

## Examples

``` r
x <- c("Yes","No","Yes","Yes","No","Yes","No","No","Yes","Yes")
y <- c("No","No","Yes","Yes","No","Yes","Yes","No","Yes","No")
result <- mcnemar_interpret(x, y)
print(result)
#> 
#> statease McNemar's Test Report --------------------------------
#>   N            : 10
#>   Table size   : 2 x 2
#>   Discordant   : 3 pairs
#> -----------------------------------------------------------------
#>   Contingency Table:
#>      y
#> x     No Yes
#>   No   3   1
#>   Yes  2   4
#> 
#> -----------------------------------------------------------------
#>   p-value      : 1.0000
#>   Matched OR   : 0.500
#>   95% CI      : [0.045, 5.514]
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant (p = 1.0000 > alpha 0.05).
#>   There is insufficient evidence of a significant difference in paired proportions between the two measurements.
#>   Matched OR = 0.500: More subjects changed from the second category to the first category than vice versa.
#>   95% CI [0.045, 5.514] includes 1: No statistically significant evidence of a difference in paired proportions.
#> 
#>   WARNING: The number of discordant pairs is very small (less than 10). Results may be unreliable. Interpret with caution.
#>   WARNING: McNemar's Test assumes that observations are paired and independent across pairs. Violation of this assumption may affect the validity of the results.
#> 
#>   NOTE: McNemar's Test requires paired or matched data. Ensure that each row in your data represents the same subject measured twice or a matched pair.
#> -----------------------------------------------------------------
#> 
```
