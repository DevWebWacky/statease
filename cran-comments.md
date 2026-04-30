## R CMD check results
0 errors | 0 warnings | 1 note

## Notes
* checking for future file timestamps: unable to verify current time
  This is a local network issue and not related to the package itself.

## Comments to CRAN
This is a resubmission of statease v1.0.0.

Changes made based on CRAN reviewer feedback:
1. Expanded the Description field with more detail about package functionality
2. Replaced cat()/print() output with proper S3 objects and print methods

The package provides functions for descriptive statistics, t-tests,
and ANOVA with automatic plain-English interpretation of results.
