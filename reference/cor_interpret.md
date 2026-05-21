# Correlation Analysis with Plain-English Interpretation

Correlation Analysis with Plain-English Interpretation

## Usage

``` r
cor_interpret(
  x,
  y,
  method = "pearson",
  conf.level = 0.95,
  var1_name = "Variable 1",
  var2_name = "Variable 2"
)
```

## Arguments

- x:

  A numeric vector

- y:

  A numeric vector

- method:

  Correlation method: "pearson", "spearman", or "kendall". Default
  "pearson".

- conf.level:

  Confidence level. Default 0.95.

- var1_name:

  Optional name for first variable. Default "Variable 1"

- var2_name:

  Optional name for second variable. Default "Variable 2"

## Value

An object of class `statease_cor` containing correlation results and
interpretation. Use [`print()`](https://rdrr.io/r/base/print.html) to
display the formatted report.

## Examples

``` r
x <- c(23, 45, 12, 67, 34, 89, 56, 43, 78, 90)
y <- c(19, 42, 15, 70, 30, 85, 52, 48, 80, 88)
result <- cor_interpret(x, y)
print(result)
#> 
#> -- statease Correlation Report -----------------------------------
#>   Method       : Pearson Product-Moment Correlation
#>   Variables    : Variable 1 & Variable 2
#>   N            : 10  |  Missing: 0
#> -----------------------------------------------------------------
#>   r            : 0.9911
#>   p-value      : 0.0000
#>   95% CI      : [0.9612, 0.9980]
#>   Strength     : very large
#>   Direction    : positive (as one variable increases, the other tends to increase)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The correlation is statistically significant (p = 0.0000 < alpha 0.05).
#>   The relationship between Variable 1 and Variable 2 is
#>   very large and positive (as one variable increases, the other tends to increase) in direction.
#> -----------------------------------------------------------------
#> 
```
