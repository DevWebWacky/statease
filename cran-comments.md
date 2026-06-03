## R CMD check results
0 errors | 0 warnings | 0 notes

## Comments to CRAN
This is a submission of statease v1.2.0.

New functions added in this version:
1. mlr_interpret() - Multiple linear regression with diagnostics
2. logistic_interpret() - Logistic regression with odds ratios
3. manova_interpret() - MANOVA with Pillai's trace and follow-up ANOVAs
4. mannwhitney_interpret() - Mann-Whitney U test with effect size
5. wilcoxon_interpret() - Wilcoxon Signed Rank test with effect size
6. kruskal_interpret() - Kruskal-Wallis test with post-hoc comparisons
7. analyze() updated with nonparam argument for non-parametric routing

Bug fixes:
- anova2_interpret() now uses car::Anova() with Type II/III SS instead of Type I SS for order-independent results
