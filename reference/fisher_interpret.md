# Fisher's Exact Test with Plain-English Interpretation

Fisher's Exact Test with Plain-English Interpretation

## Usage

``` r
fisher_interpret(x, y, conf.level = 0.95, simulate.p.value = FALSE)
```

## Arguments

- x:

  A factor or character vector (first categorical variable)

- y:

  A factor or character vector (second categorical variable)

- conf.level:

  Confidence level. Default 0.95.

- simulate.p.value:

  Logical. Whether to use simulation to compute p-values for larger
  tables. Default FALSE.

## Value

An object of class `statease_fisher` containing test results and
interpretation. Use [`print()`](https://rdrr.io/r/base/print.html) to
display the formatted report.

## Examples

``` r
x <- c("Yes","No","Yes","Yes","No","Yes","No","No","Yes","Yes")
y <- c("Male","Female","Male","Female","Male",
       "Female","Male","Female","Male","Female")
result <- fisher_interpret(x, y)
print(result)
#> 
#> --- statease Fisher's Exact Test Report ---------------
#>   N            : 10
#>   Table size   : 2 x 2
#> 
#>   Contingency Table (Observed):
#>      y
#> x     Female Male
#>   No       2    2
#>   Yes      3    3
#> 
#>   Expected Frequencies:
#>      y
#> x     Female Male
#>   No       2    2
#>   Yes      3    3
#> 
#> -----------------------------------------------------------------
#>   p-value      : 1.0000
#>   Odds Ratio   : 1.000
#>   95% CI      : [0.042, 23.671]
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant (p = 1.0000 > alpha 0.05).
#>   There is insufficient evidence of an association between the two variables.
#>   OR = 1.000: No association between exposure and outcome.
#>   95% CI [0.042, 23.671] includes 1: No statistically significant evidence of an association.
#> -----------------------------------------------------------------
#> 
```
