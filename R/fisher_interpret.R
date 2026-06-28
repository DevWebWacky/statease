#' Fisher's Exact Test with Plain-English Interpretation
#'
#' @param x A factor or character vector (first categorical variable)
#' @param y A factor or character vector (second categorical variable)
#' @param conf.level Confidence level. Default 0.95.
#' @param simulate.p.value Logical. Whether to use simulation to
#'   compute p-values for larger tables. Default FALSE.
#'
#' @return An object of class \code{statease_fisher} containing test
#'   results and interpretation. Use \code{print()} to display the
#'   formatted report.
#' @export
#'
#' @examples
#' x <- c("Yes","No","Yes","Yes","No","Yes","No","No","Yes","Yes")
#' y <- c("Male","Female","Male","Female","Male",
#'        "Female","Male","Female","Male","Female")
#' result <- fisher_interpret(x, y)
#' print(result)
fisher_interpret <- function(x, y, conf.level = 0.95,
                             simulate.p.value = FALSE) {

  # Guard clauses
  if (!is.vector(x) && !is.factor(x)) {
    stop("x must be a vector or factor.")
  }
  if (!is.vector(y) && !is.factor(y)) {
    stop("y must be a vector or factor.")
  }
  if (length(x) != length(y)) {
    stop("x and y must have the same length.")
  }
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1.")
  }

  alpha <- 1 - conf.level

  # Convert to factor
  x <- as.factor(x)
  y <- as.factor(y)

  # Contingency table
  cont_table <- table(x, y)
  n          <- sum(cont_table)
  n_rows     <- nrow(cont_table)
  n_cols     <- ncol(cont_table)

  # Expected frequencies
  expected <- suppressWarnings(chisq.test(cont_table)$expected)

  # Run Fisher's Exact Test
  result <- fisher.test(cont_table,
                        conf.level = conf.level,
                        simulate.p.value = simulate.p.value)

  p_val <- result$p.value
  or    <- result$estimate
  ci    <- result$conf.int

  # Significance
  sig_label <- if (p_val < alpha) {
    sprintf("statistically significant (p = %.4f < alpha %.2f)",
            p_val, alpha)
  } else {
    sprintf("not statistically significant (p = %.4f > alpha %.2f)",
            p_val, alpha)
  }

  # Association interpretation
  assoc_label <- if (p_val < alpha) {
    "There is evidence of an association between the two categorical variables."
  } else {
    "There is insufficient evidence of an association between the two variables."
  }

  # OR interpretation
  or_label <- if (!is.null(or)) {
    if (or > 1) {
      sprintf("OR = %.3f: The odds of the outcome are higher in the first group.", or)
    } else if (or < 1) {
      sprintf("OR = %.3f: The odds of the outcome are lower in the first group.", or)
    } else {
      sprintf("OR = %.3f: No association between exposure and outcome.", or)
    }
  } else {
    NULL
  }

  # CI interpretation
  ci_label <- if (!is.null(ci)) {
    if (ci[1] > 1 || ci[2] < 1) {
      sprintf("%.0f%% CI [%.3f, %.3f] excludes 1: Evidence of a significant association.",
              conf.level * 100, ci[1], ci[2])
    } else {
      sprintf("%.0f%% CI [%.3f, %.3f] includes 1: No statistically significant evidence of an association.",
              conf.level * 100, ci[1], ci[2])
    }
  } else {
    NULL
  }

  #Warnings and Notes
  warnings_list <- c()
  notes_list    <- c()

  # Large sample warning
  if (n > 200) {
    warnings_list <- c(warnings_list,
                       paste("Sample size is relatively large.",
                             "A Chi-square test would likely produce similar",
                             "results and may be more computationally efficient.",
                             "Consider using chisq_interpret()."))
  }

  # Table larger than 2x2
  if (n_rows > 2 || n_cols > 2) {
    warnings_list <- c(warnings_list,
                       paste("The contingency table is larger than 2x2.",
                             "Fisher's Exact Test may require simulation or",
                             "approximation methods, which can increase",
                             "computation time and reduce exactness.",
                             "Interpret results with caution.",
                             "If simulate.p.value = TRUE was used,",
                             "explicitly state this in your report."))
  }

  # Zero cells
  if (any(cont_table == 0)) {
    warnings_list <- c(warnings_list,
                       paste("One or more cells contain zero counts.",
                             "Odds ratios may be unstable, undefined,",
                             "or infinite. Interpret results with caution."))
  }

  # All expected frequencies > 5
  if (all(expected >= 5)) {
    notes_list <- c(notes_list,
                    paste("NOTE: All expected frequencies exceed 5.",
                          "A Chi-square test would also be appropriate",
                          "and may be preferred for larger samples.",
                          "Consider using chisq_interpret()."))
  }

  # Simulate p value note
  if (simulate.p.value) {
    notes_list <- c(notes_list,
                    paste("NOTE: P-value was computed using simulation",
                          "(simulate.p.value = TRUE).",
                          "Please state this explicitly in your report."))
  }

  output <- list(
    cont_table        = cont_table,
    expected          = expected,
    n                 = n,
    n_rows            = n_rows,
    n_cols            = n_cols,
    p_val             = p_val,
    or                = or,
    ci                = ci,
    sig_label         = sig_label,
    assoc_label       = assoc_label,
    or_label          = or_label,
    ci_label          = ci_label,
    warnings_list     = warnings_list,
    notes_list        = notes_list,
    simulate.p.value  = simulate.p.value,
    conf.level        = conf.level,
    alpha             = alpha
  )

  class(output) <- "statease_fisher"
  output
}

#' @export
print.statease_fisher <- function(x, ...) {
  cat("\n")
  cat("--- statease Fisher's Exact Test Report ---------------\n")
  cat(sprintf("  N            : %d\n", x$n))
  cat(sprintf("  Table size   : %d x %d\n", x$n_rows, x$n_cols))
  cat("\n")
  cat("  Contingency Table (Observed):\n")
  print(x$cont_table)
  cat("\n")
  cat("  Expected Frequencies:\n")
  print(round(x$expected, 2))
  cat("\n")
  cat("-----------------------------------------------------------------\n")
  cat(sprintf("  p-value      : %.4f\n", x$p_val))
  if (!is.null(x$or)) {
    cat(sprintf("  Odds Ratio   : %.3f\n", x$or))
  }
  if (!is.null(x$ci)) {
    cat(sprintf("  %d%% CI      : [%.3f, %.3f]\n",
                as.integer(x$conf.level * 100),
                x$ci[1], x$ci[2]))
  }
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The result is %s.\n", x$sig_label))
  cat(sprintf("  %s\n", x$assoc_label))
  if (!is.null(x$or_label)) {
    cat(sprintf("  %s\n", x$or_label))
  }
  if (!is.null(x$ci_label)) {
    cat(sprintf("  %s\n", x$ci_label))
  }
  if (length(x$warnings_list) > 0) {
    cat("\n")
    for (w in x$warnings_list) {
      cat(sprintf("  WARNING: %s\n", w))
    }
  }
  if (length(x$notes_list) > 0) {
    cat("\n")
    for (n in x$notes_list) {
      cat(sprintf("  %s\n", n))
    }
  }
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
