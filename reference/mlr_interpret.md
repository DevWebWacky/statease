# Multiple Linear Regression with Plain-English Interpretation

Multiple Linear Regression with Plain-English Interpretation

## Usage

``` r
mlr_interpret(formula, data, conf.level = 0.95)
```

## Arguments

- formula:

  A formula of the form outcome ~ predictor1 + predictor2 + ...

- data:

  A data frame containing the variables

- conf.level:

  Confidence level. Default 0.95.

## Value

An object of class `statease_mlr` containing multiple regression results
and interpretation. Use [`print()`](https://rdrr.io/r/base/print.html)
to display the formatted report.

## Examples

``` r
df <- data.frame(
  exam_score  = c(23,45,12,67,34,89,56,43,78,90),
  study_hours = c(2,5,1,7,3,9,6,4,8,10),
  attendance  = c(60,80,50,90,70,95,85,75,88,92)
)
result <- mlr_interpret(exam_score ~ study_hours + attendance, data = df)
#> Warning: Sample size is small (n < 20). Interpret results with caution.
print(result)
#> 
#> -- statease Multiple Linear Regression Report -------------------
#>   Outcome      : exam_score
#>   Predictors   : study_hours, attendance
#>   N            : 10
#> -----------------------------------------------------------------
#>   Model Equation:
#>   exam_score = -2.664 + 8.240*study_hours + 0.141*attendance
#> -----------------------------------------------------------------
#>   Overall Model Fit:
#>   R-squared    : 0.9893 (large effect)
#>   Adj R-squared: 0.9862
#>   F-statistic  : 322.885 (df = 2, 7)  p = 0.0000
#>   The overall model is statistically significant (p = 0.0000 < alpha 0.05).
#> -----------------------------------------------------------------
#>   Individual Predictors:
#> 
#>   study_hours
#>     Coefficient  : 8.240  (SE = 1.106)
#>     t-statistic  : 7.452
#>     p-value      : 0.0001  [significant]
#>     95% CI      : [5.626, 10.855]
#>     Direction    : positive (b = 8.240)
#> 
#>   attendance
#>     Coefficient  : 0.141  (SE = 0.227)
#>     t-statistic  : 0.620
#>     p-value      : 0.5549  [not significant]
#>     95% CI      : [-0.396, 0.677]
#>     Direction    : positive (b = 0.141)
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The model explains 98.9% of the variance in exam_score
#>   (R-squared = 0.9893, large effect).
#>   Adjusted R-squared = 0.9862 accounting for
#>   the number of predictors in the model.
#> 
#>   Significant predictors: study_hours
#>   Non-significant predictors: attendance
#> -----------------------------------------------------------------
#> 
```
