# Chi-Square Test with Plain-English Interpretation

Chi-Square Test with Plain-English Interpretation

## Usage

``` r
chisq_interpret(x, y, correct = TRUE, conf.level = 0.95, context = NULL)
```

## Arguments

- x:

  A factor or character vector (first categorical variable)

- y:

  A factor or character vector (second categorical variable)

- correct:

  Logical. Apply Yates continuity correction. Default TRUE.

- conf.level:

  Confidence level. Default 0.95.

- context:

  Optional description of the study design or sampling method, echoed
  back in the printed report. Default NULL.

## Value

An object of class `statease_chisq` containing test results and
interpretation. Use [`print()`](https://rdrr.io/r/base/print.html) to
display the formatted report.

## Examples

``` r
x <- c("Yes","No","Yes","Yes","No","Yes","No","No","Yes","Yes")
y <- c("Male","Female","Male","Female","Male","Female","Male","Female","Male","Female")
result <- chisq_interpret(x, y)
print(result)
#> 
#> -- statease Chi-Square Test Report ------------------------------
#>   N            : 10
#> -----------------------------------------------------------------
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
#>   Chi-square   : 0.000
#>   df           : 1
#>   p-value      : 1.0000
#>   Cramer's V   : 0.000 (negligible effect)
#> -----------------------------------------------------------------
#>   Assumption Checks:
#>     Expected cell frequencies : WARNING  (one or more cells < 5)
#>     Sample independence       : NOTE     (assumed from study design, not testable from data)
#> 
#>   NOTE: Assumption checks are diagnostic tools and may be
#>   influenced by sample size and other characteristics of the
#>   data. Passing a check does not prove that an assumption is
#>   satisfied, and a warning does not automatically invalidate
#>   the analysis. Interpret these results alongside your
#>   knowledge of the data.
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The result is not statistically significant (p = 1.0000 > alpha 0.05).
#>   There is no significant association between the two variables.
#>   Effect size is negligible (V = 0.000).
#> -----------------------------------------------------------------
#> 
```
