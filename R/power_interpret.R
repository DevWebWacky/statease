#' Power Analysis with Plain English Interpretation
#'
#' @param test The statistical test. One of "ttest.one",
#'   "ttest.two", "ttest.paired", "anova", "correlation",
#'   "chisq", "regression".
#' @param effect_size The expected effect size. Use Cohen's
#'   conventions: small = 0.2, medium = 0.5, large = 0.8
#'   for t-tests; small = 0.10, medium = 0.25, large = 0.40
#'   for ANOVA; small = 0.10, medium = 0.30, large = 0.50
#'   for correlation.
#' @param n Sample size per group. If provided, calculates
#'   achieved power. If NULL, calculates required sample size.
#' @param alpha Significance level. Default 0.05.
#' @param power Desired power level. Default 0.80.
#' @param n_groups Number of groups (for ANOVA only). Default 2.
#' @param n_predictors Number of predictors (for regression only).
#'   Default 1.
#'
#' @return An object of class \code{statease_power} containing
#'   power analysis results and interpretation. Use \code{print()}
#'   to display the formatted report.
#' @export
#'
#' @examples
#' # Calculate required sample size for independent t-test
#' result <- power_interpret("ttest.two", effect_size = 0.5)
#' print(result)
#'
#' # Calculate achieved power for given sample size
#' result2 <- power_interpret("ttest.two", effect_size = 0.5, n = 30)
#' print(result2)
power_interpret <- function(test, effect_size, n = NULL,
                            alpha = 0.05, power = 0.80,
                            n_groups = 2, n_predictors = 1) {

  #Guard clauses
  valid_tests <- c("ttest.one", "ttest.two", "ttest.paired",
                   "anova", "correlation", "chisq", "regression")
  if (!test %in% valid_tests) {
    stop(sprintf("test must be one of: %s",
                 paste(valid_tests, collapse = ", ")))
  }
  if (!is.numeric(effect_size) || effect_size <= 0) {
    stop("effect_size must be a positive numeric value.")
  }
  if (alpha <= 0 || alpha >= 1) {
    stop("alpha must be between 0 and 1.")
  }
  if (power <= 0 || power >= 1) {
    stop("power must be between 0 and 1.")
  }

  # -Mode
  mode <- if (is.null(n)) "sample_size" else "power"

  # Effect size label
  es_label <- function(es, type = "d") {
    thresholds <- switch(type,
                         "d" = c(0.2, 0.5, 0.8),
                         "f" = c(0.10, 0.25, 0.40),
                         "r" = c(0.10, 0.30, 0.50),
                         "w" = c(0.10, 0.30, 0.50),
                         "f2" = c(0.02, 0.15, 0.35),
                         c(0.2, 0.5, 0.8)
    )
    if (es < thresholds[1]) "small" else if
    (es < thresholds[2]) "medium" else "large"
  }

  # Run power analysis
  result_val  <- NULL
  test_label  <- NULL
  es_type     <- "d"

  if (test == "ttest.one") {
    test_label <- "One-Sample T-Test"
    es_type    <- "d"
    if (mode == "sample_size") {
      pw         <- pwr::pwr.t.test(d = effect_size,
                                    sig.level = alpha,
                                    power = power,
                                    type = "one.sample")
      result_val <- ceiling(pw$n)
    } else {
      pw         <- pwr::pwr.t.test(d = effect_size,
                                    n = n,
                                    sig.level = alpha,
                                    type = "one.sample")
      result_val <- pw$power
    }

  } else if (test == "ttest.two") {
    test_label <- "Independent Samples T-Test"
    es_type    <- "d"
    if (mode == "sample_size") {
      pw         <- pwr::pwr.t.test(d = effect_size,
                                    sig.level = alpha,
                                    power = power,
                                    type = "two.sample")
      result_val <- ceiling(pw$n)
    } else {
      pw         <- pwr::pwr.t.test(d = effect_size,
                                    n = n,
                                    sig.level = alpha,
                                    type = "two.sample")
      result_val <- pw$power
    }

  } else if (test == "ttest.paired") {
    test_label <- "Paired Samples T-Test"
    es_type    <- "d"
    if (mode == "sample_size") {
      pw         <- pwr::pwr.t.test(d = effect_size,
                                    sig.level = alpha,
                                    power = power,
                                    type = "paired")
      result_val <- ceiling(pw$n)
    } else {
      pw         <- pwr::pwr.t.test(d = effect_size,
                                    n = n,
                                    sig.level = alpha,
                                    type = "paired")
      result_val <- pw$power
    }

  } else if (test == "anova") {
    test_label <- "One-Way ANOVA"
    es_type    <- "f"
    if (mode == "sample_size") {
      pw         <- pwr::pwr.anova.test(k = n_groups,
                                        f = effect_size,
                                        sig.level = alpha,
                                        power = power)
      result_val <- ceiling(pw$n)
    } else {
      pw         <- pwr::pwr.anova.test(k = n_groups,
                                        n = n,
                                        f = effect_size,
                                        sig.level = alpha)
      result_val <- pw$power
    }

  } else if (test == "correlation") {
    test_label <- "Correlation"
    es_type    <- "r"
    if (mode == "sample_size") {
      pw         <- pwr::pwr.r.test(r = effect_size,
                                    sig.level = alpha,
                                    power = power)
      result_val <- ceiling(pw$n)
    } else {
      pw         <- pwr::pwr.r.test(r = effect_size,
                                    n = n,
                                    sig.level = alpha)
      result_val <- pw$power
    }

  } else if (test == "chisq") {
    test_label <- "Chi-Square Test"
    es_type    <- "w"
    if (mode == "sample_size") {
      pw         <- pwr::pwr.chisq.test(w = effect_size,
                                        sig.level = alpha,
                                        power = power)
      result_val <- ceiling(pw$N)
    } else {
      pw         <- pwr::pwr.chisq.test(w = effect_size,
                                        N = n,
                                        sig.level = alpha)
      result_val <- pw$power
    }

  } else if (test == "regression") {
    test_label <- sprintf("Multiple Regression (fixed model, %d predictors)",
                          n_predictors)
    es_type    <- "f2"
    if (mode == "sample_size") {
      pw         <- pwr::pwr.f2.test(u = n_predictors,
                                     f2 = effect_size,
                                     sig.level = alpha,
                                     power = power)
      result_val <- ceiling(pw$v + n_predictors + 1)
    } else {
      pw         <- pwr::pwr.f2.test(u = n_predictors,
                                     v = n - n_predictors - 1,
                                     f2 = effect_size,
                                     sig.level = alpha)
      result_val <- pw$power
    }
  }

  # Interpretation
  effect_label <- es_label(effect_size, es_type)

  if (mode == "sample_size") {
    result_interpretation <- sprintf(
      paste("To detect a %s effect (effect size = %.2f) with",
            "%.0f%% power at alpha = %.2f, you need at least",
            "%d participants%s."),
      effect_label, effect_size, power * 100, alpha,
      result_val,
      if (test == "ttest.two" || test == "anova") {
        sprintf(" per group (%d total for %d groups)",
                result_val * n_groups, n_groups)
      } else ""
    )
  } else {
    result_interpretation <- sprintf(
      paste("With n = %d and a %s effect size (%.2f),",
            "the achieved power is %.1f%%."),
      n, effect_label, effect_size, result_val * 100
    )
  }

  # Warnings and Notes
  warnings_list <- c()
  notes_list    <- c()

  if (mode == "power" && result_val < 0.80) {
    warnings_list <- c(warnings_list,
                       paste("Power is less than 0.80. The study may be",
                             "underpowered — there is a meaningful risk of",
                             "failing to detect a true effect (Type II error).",
                             "Consider increasing the sample size."))
  }

  if (effect_size < 0.10) {
    warnings_list <- c(warnings_list,
                       paste("The effect size provided is very small.",
                             "A very large sample size will be required",
                             "to detect this effect reliably. Ensure the",
                             "effect size is theoretically and practically",
                             "meaningful before proceeding."))
  }

  if (mode == "power") {
    warnings_list <- c(warnings_list,
                       paste("Post-hoc (observed) power calculations should",
                             "be interpreted cautiously, as they are largely",
                             "determined by the observed p-value and effect size."))
  }

  notes_list <- c(notes_list,
                  paste("Power analysis results are estimates based on",
                        "assumptions about effect size, alpha, and power.",
                        "Actual results may differ depending on the true",
                        "effect size in the population."))

  notes_list <- c(notes_list,
                  paste("Effect sizes should ideally be based on previous",
                        "research, pilot studies, or theoretically justified",
                        "values — not chosen arbitrarily to reduce required",
                        "sample size."))

  notes_list <- c(notes_list,
                  paste("A power of 0.80 is a conventional minimum.",
                        "In high stakes research such as clinical trials,",
                        "a higher power of 0.90 or 0.95 is often recommended."))

  notes_list <- c(notes_list,
                  paste("Power analysis assumes that the chosen statistical",
                        "test and its assumptions are appropriate for the data."))

  output <- list(
    test                     = test,
    test_label               = test_label,
    mode                     = mode,
    effect_size              = effect_size,
    effect_label             = effect_label,
    es_type                  = es_type,
    n                        = n,
    alpha                    = alpha,
    power                    = power,
    n_groups                 = n_groups,
    n_predictors             = n_predictors,
    result_val               = result_val,
    result_interpretation    = result_interpretation,
    warnings_list            = warnings_list,
    notes_list               = notes_list
  )

  class(output) <- "statease_power"
  output
}

#' @export
print.statease_power <- function(x, ...) {
  cat("\n")
  cat("-- statease Power Analysis Report  \n")
  cat(sprintf("  Test         : %s\n", x$test_label))
  cat(sprintf("  Mode         : %s\n",
              if (x$mode == "sample_size") {
                "Calculate required sample size"
              } else {
                "Calculate achieved power"
              }))
  cat("-----------------------------------------------------------------\n")
  cat(sprintf("  Effect size  : %.3f (%s)\n",
              x$effect_size, x$effect_label))
  cat(sprintf("  Alpha        : %.2f\n", x$alpha))
  if (x$mode == "sample_size") {
    cat(sprintf("  Desired power: %.2f (%.0f%%)\n",
                x$power, x$power * 100))
    cat(sprintf("  Required n   : %d\n", x$result_val))
    if (x$test %in% c("ttest.two", "anova")) {
      cat(sprintf("  Total N      : %d (%d groups x %d)\n",
                  x$result_val * x$n_groups,
                  x$n_groups, x$result_val))
    }
  } else {
    cat(sprintf("  Sample size  : %d\n", x$n))
    cat(sprintf("  Achieved power: %.4f (%.1f%%)\n",
                x$result_val, x$result_val * 100))
  }
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  %s\n", x$result_interpretation))

  if (length(x$warnings_list) > 0) {
    cat("\n")
    for (w in x$warnings_list) {
      cat(sprintf("  WARNING: %s\n", w))
    }
  }

  if (length(x$notes_list) > 0) {
    cat("\n")
    for (n in x$notes_list) {
      cat(sprintf("  NOTE: %s\n", n))
    }
  }
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
