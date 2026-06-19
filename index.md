# statease

![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/statease)![CRAN
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

Try statease without installing R: 🌐
<https://devwebwacky.shinyapps.io/statease/>

## Functions

| Function | What it does |
|----|----|
| [`analyze()`](https://devwebwacky.github.io/statease/reference/analyze.md) | Master function - auto-detects and runs the right test |
| [`describe()`](https://devwebwacky.github.io/statease/reference/describe.md) | Descriptive statistics with interpretation |
| [`ttest_interpret()`](https://devwebwacky.github.io/statease/reference/ttest_interpret.md) | T-tests with Cohen’s d and CI interpretation |
| [`anova_interpret()`](https://devwebwacky.github.io/statease/reference/anova_interpret.md) | One-way ANOVA with Tukey post-hoc and eta squared |
| [`anova2_interpret()`](https://devwebwacky.github.io/statease/reference/anova2_interpret.md) | Two-way ANOVA with interaction effects |
| [`manova_interpret()`](https://devwebwacky.github.io/statease/reference/manova_interpret.md) | MANOVA with Pillai’s trace and follow-up ANOVAs |
| [`chisq_interpret()`](https://devwebwacky.github.io/statease/reference/chisq_interpret.md) | Chi-square test with Cramer’s V effect size |
| [`cor_interpret()`](https://devwebwacky.github.io/statease/reference/cor_interpret.md) | Correlation analysis (Pearson, Spearman, Kendall) |
| [`reg_interpret()`](https://devwebwacky.github.io/statease/reference/reg_interpret.md) | Simple linear regression with diagnostics |
| [`mlr_interpret()`](https://devwebwacky.github.io/statease/reference/mlr_interpret.md) | Multiple linear regression with diagnostics |
| [`logistic_interpret()`](https://devwebwacky.github.io/statease/reference/logistic_interpret.md) | Logistic regression with odds ratios |
| [`mannwhitney_interpret()`](https://devwebwacky.github.io/statease/reference/mannwhitney_interpret.md) | Mann-Whitney U test (non-parametric) |
| [`wilcoxon_interpret()`](https://devwebwacky.github.io/statease/reference/wilcoxon_interpret.md) | Wilcoxon Signed Rank test (non-parametric) |
| [`kruskal_interpret()`](https://devwebwacky.github.io/statease/reference/kruskal_interpret.md) | Kruskal-Wallis test with post-hoc comparisons |
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

# Kruskal-Wallis (non-parametric ANOVA alternative)
analyze(formula = score ~ group, data = df, nonparam = TRUE)

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

# Multiple linear regression (auto-detected)
df4 <- data.frame(
  exam_score  = c(23,45,12,67,34,89,56,43,78,90),
  study_hours = c(2,5,1,7,3,9,6,4,8,10),
  attendance  = c(60,80,50,90,70,95,85,75,88,92)
)
analyze(formula = exam_score ~ study_hours + attendance, data = df4)

# MANOVA (auto-detected)
df5 <- data.frame(
  math    = c(23,45,12,67,34,89,56,43,78,90,11,34),
  english = c(34,56,23,78,45,90,67,54,89,95,22,45),
  group   = rep(c("A","B","C"), each = 4)
)
analyze(formula = cbind(math, english) ~ group, data = df5)

# Interpret any p-value
interpret_p(0.03, context = "treatment vs control group")
```

## Why statease?

Most R output gives you numbers. statease gives you **numbers +
meaning**. Perfect for: - Students learning statistics - Researchers who
want fast readable output - Educators teaching statistical concepts

## Changelog

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
  with `nonparam` argument for non-parametric routing
- Fixed
  [`anova2_interpret()`](https://devwebwacky.github.io/statease/reference/anova2_interpret.md)
  to use Type II/III SS via car::Anova() instead of Type I SS for
  order-independent results

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
