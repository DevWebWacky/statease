# statease

![CRAN Downloads](https://cranlogs.r-pkg.org/badges/statease)![CRAN
Total](https://cranlogs.r-pkg.org/badges/grand-total/statease)![CRAN
Version](https://www.r-pkg.org/badges/version/statease)

> Simplified statistical analysis with plain-English interpretation for
> R

## Overview

**statease** is an R package that runs a wide range of statistical
analyses and tells you in plain English what the results mean. No more
copy-pasting output into interpretation guides. One function call gives
you the full picture.

## Installation

``` r

install.packages("statease")
```

For the development version from GitHub:

``` r

# install.packages("devtools")
devtools::install_github("DevWebWacky/statease")
```

## Live App

Try statease directly in your browser without installing R:

🌐 [Launch statease Shiny
App](https://devwebwacky.shinyapps.io/statease/)

## Functions

| Function | What it does |
|----|----|
| [`analyze()`](https://devwebwacky.github.io/statease/reference/analyze.md) | Master function - auto-detects and runs the right test |
| [`describe()`](https://devwebwacky.github.io/statease/reference/describe.md) | Descriptive statistics with interpretation |
| [`ttest_interpret()`](https://devwebwacky.github.io/statease/reference/ttest_interpret.md) | T-tests with Cohen’s d and CI interpretation |
| [`anova_interpret()`](https://devwebwacky.github.io/statease/reference/anova_interpret.md) | One-way ANOVA with Tukey post-hoc and eta squared |
| [`anova2_interpret()`](https://devwebwacky.github.io/statease/reference/anova2_interpret.md) | Two-way ANOVA with Type II/III SS |
| [`manova_interpret()`](https://devwebwacky.github.io/statease/reference/manova_interpret.md) | MANOVA with Pillai’s trace and follow-up ANOVAs |
| [`chisq_interpret()`](https://devwebwacky.github.io/statease/reference/chisq_interpret.md) | Chi-square test with Cramer’s V effect size |
| [`fisher_interpret()`](https://devwebwacky.github.io/statease/reference/fisher_interpret.md) | Fisher’s Exact Test with Odds Ratio |
| [`mcnemar_interpret()`](https://devwebwacky.github.io/statease/reference/mcnemar_interpret.md) | McNemar’s Test for paired categorical data |
| [`cor_interpret()`](https://devwebwacky.github.io/statease/reference/cor_interpret.md) | Correlation analysis (Pearson, Spearman, Kendall) |
| [`reg_interpret()`](https://devwebwacky.github.io/statease/reference/reg_interpret.md) | Simple linear regression with diagnostics |
| [`mlr_interpret()`](https://devwebwacky.github.io/statease/reference/mlr_interpret.md) | Multiple linear regression with diagnostics |
| [`logistic_interpret()`](https://devwebwacky.github.io/statease/reference/logistic_interpret.md) | Logistic regression with odds ratios |
| [`mannwhitney_interpret()`](https://devwebwacky.github.io/statease/reference/mannwhitney_interpret.md) | Mann-Whitney U test (non-parametric) |
| [`wilcoxon_interpret()`](https://devwebwacky.github.io/statease/reference/wilcoxon_interpret.md) | Wilcoxon Signed Rank test (non-parametric) |
| [`kruskal_interpret()`](https://devwebwacky.github.io/statease/reference/kruskal_interpret.md) | Kruskal-Wallis test with post-hoc comparisons |
| [`friedman_interpret()`](https://devwebwacky.github.io/statease/reference/friedman_interpret.md) | Friedman Test with Kendall’s W |
| [`check_assumptions()`](https://devwebwacky.github.io/statease/reference/check_assumptions.md) | Automated assumption checking before analysis |
| [`power_interpret()`](https://devwebwacky.github.io/statease/reference/power_interpret.md) | Statistical power analysis and sample size calculation |
| [`interpret_p()`](https://devwebwacky.github.io/statease/reference/interpret_p.md) | Standalone p-value interpreter |

## Usage

### One command does it all

``` r

library(statease)

# Descriptive statistics
analyze(x = c(23, 45, 12, 67, 34), var_name = "Exam Scores")

# Independent samples t-test (auto-detected)
analyze(x = c(23,45,12,67,34), y = c(19,38,22,51,29),
        var_name = "Scores")

# Check assumptions first
analyze(x = c(23,45,12,67,34), y = c(19,38,22,51,29),
        check = TRUE)

# Non-parametric alternative (auto-detected)
analyze(x = c(23,45,12,67,34), y = c(19,38,22,51,29),
        nonparam = TRUE, var_name = "Scores")

# Correlation (auto-detected)
analyze(x = c(23,45,12,67,34), y = c(19,38,22,51,29),
        var1_name = "Exam Score", var2_name = "Study Hours")

# Chi-square (auto-detected)
analyze(
  x = c("Yes","No","Yes","Yes","No"),
  y = c("Male","Female","Male","Female","Male")
)

# One-way ANOVA (auto-detected)
df <- data.frame(
  score = c(23,45,12,67,34,89,56,43,78,90,11,34),
  group = rep(c("A","B","C"), each = 4)
)
analyze(formula = score ~ group, data = df)

# Two-way ANOVA (auto-detected)
df2 <- data.frame(
  score  = c(23,45,12,67,34,89,56,43,78,90,11,34),
  method = rep(c("Online","Traditional"), each = 6),
  gender = rep(c("Male","Female"), times = 6)
)
analyze(formula = score ~ method * gender, data = df2)

# Simple linear regression (auto-detected)
df3 <- data.frame(
  exam_score  = c(23,45,12,67,34,89,56,43,78,90),
  study_hours = c(2,5,1,7,3,9,6,4,8,10)
)
analyze(formula = exam_score ~ study_hours, data = df3)

# Power analysis
analyze(test_type = "ttest.two", effect_size = 0.5)

# Interpret any p-value
interpret_p(0.03, context = "treatment vs control group")
```

## Why statease?

Most R output gives you numbers. statease gives you **numbers +
meaning**. Perfect for: - Students learning statistics - Researchers who
want fast readable output - Educators teaching statistical concepts

## Changelog

### v1.3.0

- Added
  [`fisher_interpret()`](https://devwebwacky.github.io/statease/reference/fisher_interpret.md)
  for Fisher’s Exact Test
- Added
  [`mcnemar_interpret()`](https://devwebwacky.github.io/statease/reference/mcnemar_interpret.md)
  for McNemar’s Test
- Added
  [`friedman_interpret()`](https://devwebwacky.github.io/statease/reference/friedman_interpret.md)
  for Friedman Test
- Added
  [`check_assumptions()`](https://devwebwacky.github.io/statease/reference/check_assumptions.md)
  for automated assumption checking
- Added
  [`power_interpret()`](https://devwebwacky.github.io/statease/reference/power_interpret.md)
  for power analysis and sample size
- Added Shiny app via
  [`run_app()`](https://devwebwacky.github.io/statease/reference/run_app.md)
  for point-and-click analysis
- Updated
  [`analyze()`](https://devwebwacky.github.io/statease/reference/analyze.md)
  with `check` and `test_type` arguments

### v1.2.1

- Fixed non-parametric interpretation — stochastic superiority correctly
  reported instead of median differences

### v1.2.0

- Added
  [`mlr_interpret()`](https://devwebwacky.github.io/statease/reference/mlr_interpret.md)
  for multiple linear regression
- Added
  [`logistic_interpret()`](https://devwebwacky.github.io/statease/reference/logistic_interpret.md)
  for logistic regression
- Added
  [`manova_interpret()`](https://devwebwacky.github.io/statease/reference/manova_interpret.md)
  for MANOVA
- Added
  [`mannwhitney_interpret()`](https://devwebwacky.github.io/statease/reference/mannwhitney_interpret.md)
  for Mann-Whitney U test
- Added
  [`wilcoxon_interpret()`](https://devwebwacky.github.io/statease/reference/wilcoxon_interpret.md)
  for Wilcoxon Signed Rank test
- Added
  [`kruskal_interpret()`](https://devwebwacky.github.io/statease/reference/kruskal_interpret.md)
  for Kruskal-Wallis test
- Updated
  [`analyze()`](https://devwebwacky.github.io/statease/reference/analyze.md)
  with `nonparam` argument

### v1.1.0

- Added
  [`chisq_interpret()`](https://devwebwacky.github.io/statease/reference/chisq_interpret.md)
  for chi-square tests
- Added
  [`cor_interpret()`](https://devwebwacky.github.io/statease/reference/cor_interpret.md)
  for correlation analysis
- Added
  [`reg_interpret()`](https://devwebwacky.github.io/statease/reference/reg_interpret.md)
  for simple linear regression
- Added
  [`anova2_interpret()`](https://devwebwacky.github.io/statease/reference/anova2_interpret.md)
  for two-way ANOVA
- Updated
  [`analyze()`](https://devwebwacky.github.io/statease/reference/analyze.md)
  to auto-detect all new tests

### v1.0.0

- Initial CRAN release
- [`describe()`](https://devwebwacky.github.io/statease/reference/describe.md),
  [`ttest_interpret()`](https://devwebwacky.github.io/statease/reference/ttest_interpret.md),
  [`anova_interpret()`](https://devwebwacky.github.io/statease/reference/anova_interpret.md),
  [`interpret_p()`](https://devwebwacky.github.io/statease/reference/interpret_p.md),
  [`analyze()`](https://devwebwacky.github.io/statease/reference/analyze.md)

## License

MIT
