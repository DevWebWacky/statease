# statease: Simplified Statistical Analysis with Plain-English Interpretation

statease provides a suite of functions for performing common statistical
analyses and automatically interpreting the results in plain English. It
is designed for students, researchers, and educators who want fast,
readable statistical output without sacrificing rigour.

## Main Functions

- [`analyze`](https://devwebwacky.github.io/statease/reference/analyze.md):

  Master function — auto-detects and runs the right test

- [`describe`](https://devwebwacky.github.io/statease/reference/describe.md):

  Descriptive statistics with interpretation

- [`ttest_interpret`](https://devwebwacky.github.io/statease/reference/ttest_interpret.md):

  T-tests (one-sample, independent, paired) with Cohen's d

- [`anova_interpret`](https://devwebwacky.github.io/statease/reference/anova_interpret.md):

  One-way ANOVA with Tukey post-hoc and eta squared

- [`interpret_p`](https://devwebwacky.github.io/statease/reference/interpret_p.md):

  Standalone p-value interpreter

## Typical Workflow

The simplest way to use statease is through the master
[`analyze()`](https://devwebwacky.github.io/statease/reference/analyze.md)
function, which automatically detects what test to run based on your
input:


    # Descriptive statistics
    analyze(x = my_vector, var_name = "My Variable")

    # Independent samples t-test
    analyze(x = group1, y = group2, var_name = "Scores")

    # One-way ANOVA
    analyze(formula = score ~ group, data = my_df)

    # Interpret a p-value
    interpret_p(0.03, context = "treatment vs control")

## See also

Useful links:

- <https://github.com/DevWebWacky/statease>

- <https://devwebwacky.github.io/statease/>

- Report bugs at <https://github.com/DevWebWacky/statease/issues>

## Author

Uwakmfon Usen Paul
