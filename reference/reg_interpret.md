# Simple Linear Regression with Plain-English Interpretation

Simple Linear Regression with Plain-English Interpretation

## Usage

``` r
reg_interpret(formula, data, conf.level = 0.95)
```

## Arguments

- formula:

  A formula of the form outcome ~ predictor

- data:

  A data frame containing the variables

- conf.level:

  Confidence level. Default 0.95.

## Value

An object of class `statease_reg` containing regression results and
interpretation. Use [`print()`](https://rdrr.io/r/base/print.html) to
display the formatted report.

## Examples

``` r
df <- data.frame(
  exam_score = c(23,45,12,67,34,89,56,43,78,90),
  study_hours = c(2,5,1,7,3,9,6,4,8,10)
)
result <- reg_interpret(exam_score ~ study_hours, data = df)
print(result)
#> 
#> -- statease Simple Linear Regression Report ---------------------
#>   Outcome      : exam_score
#>   Predictor    : study_hours
#>   N            : 10
#> -----------------------------------------------------------------
#>   Model Equation:
#>   exam_score = 4.800 + 8.891 * study_hours
#> -----------------------------------------------------------------
#>   Coefficients:
#>   Intercept    : 4.800
#>   Slope        : 8.891  (SE = 0.336)
#>   t-statistic  : 26.442
#>   p-value      : 0.0000
#>   95% CI      : [8.116, 9.666]
#> -----------------------------------------------------------------
#>   Model Fit:
#>   R-squared    : 0.9887
#>   Adj R-squared: 0.9873
#>   F-statistic  : 699.184 (df = 1, 8)  p = 0.0000
#> -----------------------------------------------------------------
#>   Interpretation:
#>   The predictor study_hours is statistically significant (p = 0.0000 < alpha 0.05).
#>   The slope is positive - as study_hours increases by 1 unit, exam_score increases by 8.891 units.
#>   R-squared = 0.9887: study_hours explains 98.9% of the
#>   variance in exam_score (large effect).
#> -----------------------------------------------------------------
#> 
```
