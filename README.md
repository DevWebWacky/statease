# statease <img src="man/figures/logo.png" align="right" height="139" />

![CRAN Downloads](https://cranlogs.r-pkg.org/badges/statease)
![CRAN Version](https://www.r-pkg.org/badges/version/statease)

> Simplified statistical analysis with plain-English interpretation for R

## Overview

**statease** is an R package that runs a wide range of statistical
analyses and tells you in plain English what the results mean.
No more copy-pasting output into interpretation guides.
One function call gives you the full picture.

## Installation

```r
install.packages("statease")
```

For the development version from GitHub:
```r
# install.packages("devtools")
devtools::install_github("DevWebWacky/statease")
```

## Functions

| Function | What it does |
|---|---|
| `analyze()` | Master function - auto-detects and runs the right test |
| `describe()` | Descriptive statistics with interpretation |
| `ttest_interpret()` | T-tests with Cohen's d and CI interpretation |
| `anova_interpret()` | One-way ANOVA with Tukey post-hoc and eta squared |
| `anova2_interpret()` | Two-way ANOVA with interaction effects |
| `manova_interpret()` | MANOVA with Pillai's trace and follow-up ANOVAs |
| `chisq_interpret()` | Chi-square test with Cramer's V effect size |
| `cor_interpret()` | Correlation analysis (Pearson, Spearman, Kendall) |
| `reg_interpret()` | Simple linear regression with diagnostics |
| `mlr_interpret()` | Multiple linear regression with diagnostics |
| `logistic_interpret()` | Logistic regression with odds ratios |
| `mannwhitney_interpret()` | Mann-Whitney U test (non-parametric) |
| `wilcoxon_interpret()` | Wilcoxon Signed Rank test (non-parametric) |
| `kruskal_interpret()` | Kruskal-Wallis test with post-hoc comparisons |
| `interpret_p()` | Standalone p-value interpreter |

## Usage

### One command does it all

```r
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

Most R output gives you numbers. statease gives you **numbers + meaning**.
Perfect for:
- Students learning statistics
- Researchers who want fast readable output
- Educators teaching statistical concepts

## Changelog

### v1.2.0
- Added `mlr_interpret()` for multiple linear regression
- Added `logistic_interpret()` for logistic regression
- Added `manova_interpret()` for MANOVA
- Added `mannwhitney_interpret()` for Mann-Whitney U test
- Added `wilcoxon_interpret()` for Wilcoxon Signed Rank test
- Added `kruskal_interpret()` for Kruskal-Wallis test
- Updated `analyze()` with `nonparam` argument for non-parametric routing

### v1.1.0
- Added `chisq_interpret()` for chi-square tests
- Added `cor_interpret()` for correlation analysis
- Added `reg_interpret()` for simple linear regression
- Added `anova2_interpret()` for two-way ANOVA
- Updated `analyze()` to auto-detect all new tests

### v1.0.0
- Initial CRAN release
- `describe()`, `ttest_interpret()`, `anova_interpret()`,
  `interpret_p()`, `analyze()`

## License
MIT
