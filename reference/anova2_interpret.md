# Two-Way ANOVA with Plain-English Interpretation

Uses Type-2 SS by default (safe for unbalanced designs). Automatically
switches to Type-3 SS when an interaction term is detected and sets the
correct contrasts. Users are warned when interpreting main effects in
the presence of a significant interaction.

## Usage

``` r
anova2_interpret(formula, data, type = 2, conf.level = 0.95)
```

## Arguments

- formula:

  A formula of the form outcome ~ group1 \* group2

- data:

  A data frame containing the variables

- type:

  ANOVA type: 2 or 3. Default is 2. Type 3 is automatically used when an
  interaction term is detected in the formula.

- conf.level:

  Confidence level. Default 0.95.

## Value

An object of class `statease_anova2` containing two-way ANOVA results
and interpretation. Use [`print()`](https://rdrr.io/r/base/print.html)
to display the formatted report.

## Examples

``` r
df <- data.frame(
  score  = c(23,45,12,67,34,89,56,43,78,90,11,34),
  method = rep(c("Online","Traditional"), each = 6),
  gender = rep(c("Male","Female"), times = 6)
)
result <- anova2_interpret(score ~ method * gender, data = df)
#> Warning: Sample size is small (n < 20). Interpret results with caution.
print(result)
#> 
#>  statease Two-Way ANOVA Report 
#>   Outcome      : score
#>   Factor 1     : method
#>   Factor 2     : gender
#>   N            : 12
#> 
#>   Means by method:
#>     Online          : 45.00
#>     Traditional     : 52.00
#> 
#>   Means by gender:
#>     Female          : 61.33
#>     Male            : 35.67
#> 
#>   Interaction Means:
#>             Female  Male
#> Online       67.00 23.00
#> Traditional  55.67 48.33
#> 
#>   ANOVA Results:
#>   method               : F = 0.220  df = 1,8  not significant (p = 0.6517)  eta^2 = 0.0173 (small)
#>   gender               : F = 2.955  df = 1,8  not significant (p = 0.1240)  eta^2 = 0.2330 (large)
#>   Interaction          : F = 1.507  df = 1,8  not significant (p = 0.2544)  eta^2 = 0.1189 (moderate)
#> 
#>   Interpretation:
#>   Main effect of method is not significant (p = 0.6517).
#>   Main effect of gender is not significant (p = 0.1240).
#>   Interaction (method x gender) is not significant (p = 0.2544).
#> -----------------------------------------------------------------
#> 
```
