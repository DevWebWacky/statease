## R CMD check results
0 errors | 0 warnings | 1 note

## Notes
* checking for future file timestamps: unable to verify current time
  This is a local network issue and not related to the package itself.

## Comments to CRAN
This is a submission of statease v1.4.0.

Changes in this version:
1. Assumption checks (normality, variance homogeneity, multicollinearity,
   and others depending on the test) are now printed by default in
   every relevant interpretation function, rather than requiring a
   separate check_assumptions() call.
2. Added a context argument across all inferential functions and
   analyze(), letting users describe their study design and have it
   echoed back alongside the interpretation.
3. Bug fixes: a boundary-labelling issue in power_interpret(), a
   display bug in anova2_interpret(), a duplicated internal warning
   in chisq_interpret(), and a silent failure in homoscedasticity
   checking when lm()/glm() is fit inside a wrapper function.
