# Standalone P-Value Interpreter

Standalone P-Value Interpreter

## Usage

``` r
interpret_p(p, alpha = 0.05, context = NULL)
```

## Arguments

- p:

  A numeric p-value between 0 and 1

- alpha:

  Significance level. Default 0.05

- context:

  Optional string describing the test context

## Value

An object of class `statease_pvalue` containing the p-value
interpretation. Use [`print()`](https://rdrr.io/r/base/print.html) to
display the report.

## Examples

``` r
result <- interpret_p(0.03)
print(result)
#> 
#> -- statease P-Value Interpretation ------------------------------
#>   P-value      : 0.0300
#>   Alpha        : 0.05
#> -----------------------------------------------------------------
#>   Decision     : REJECT the null hypothesis at alpha = 0.05
#>   Evidence     : There is moderate evidence against the null hypothesis.
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is statistically significant. The observed data is unlikely to have occurred by chance if the null hypothesis were true.
#> 
#>   Note: Statistical significance does not imply practical importance. Always consider effect size alongside the p-value.
#> -----------------------------------------------------------------
#> 

result2 <- interpret_p(0.12, alpha = 0.05, context = "treatment vs control")
print(result2)
#> 
#> -- statease P-Value Interpretation ------------------------------
#>   Context      : treatment vs control
#>   P-value      : 0.1200
#>   Alpha        : 0.05
#> -----------------------------------------------------------------
#>   Decision     : FAIL TO REJECT the null hypothesis at alpha = 0.05
#>   Evidence     : There is little to no evidence against the null hypothesis.
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant. The observed data does not provide sufficient evidence to reject the null hypothesis.
#> 
#>   Note: A non-significant result does not prove the null hypothesis. It may reflect insufficient power or a small sample size.
#> -----------------------------------------------------------------
#> 
```
