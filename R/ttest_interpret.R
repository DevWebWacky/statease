#' T-Test with Plain-English Interpretation
#'
#' @param x A numeric vector (group 1, or the only group for one-sample)
#' @param y A numeric vector (group 2, for independent samples). Default NULL.
#' @param mu Hypothesised mean for one-sample t-test. Default 0.
#' @param paired Logical. TRUE for paired t-test. Default FALSE.
#' @param conf.level Confidence level. Default 0.95.
#' @param var_name Optional label for the report. Default "Variable"
#'
#' @return An object of class \code{statease_ttest} containing test
#'   results and interpretation. Use \code{print()} to display the
#'   formatted report.
#' @export
#'
#' @examples
#' result <- ttest_interpret(c(23,45,12,67,34), c(19,38,22,51,29))
#' print(result)
ttest_interpret <- function(x, y = NULL, mu = 0, paired = FALSE,
                            conf.level = 0.95, var_name = "Variable") {

  if (!is.numeric(x)) stop("x must be a numeric vector.")
  if (!is.null(y) && !is.numeric(y)) stop("y must be a numeric vector.")
  if (conf.level <= 0 || conf.level >= 1) stop("conf.level must be between 0 and 1.")
  if (length(na.omit(x)) < 2) stop("x must have at least 2 non-missing values.")

  x_clean <- na.omit(x)
  if (length(x_clean) < 10) {
    warning("Sample size in x is small (n < 10). Interpret results with caution.")
  }

  normality_note <- NULL
  if (length(x_clean) >= 3 && length(x_clean) <= 5000) {
    sw_x <- shapiro.test(x_clean)
    if (sw_x$p.value < 0.05) {
      normality_note <- "WARNING: Shapiro-Wilk suggests x may not be normally distributed."
    }
  }

  if (!is.null(y)) {
    y_clean <- na.omit(y)
    if (length(y_clean) < 2) stop("y must have at least 2 non-missing values.")
    if (length(y_clean) < 10) {
      warning("Sample size in y is small (n < 10). Interpret results with caution.")
    }
    if (length(y_clean) >= 3 && length(y_clean) <= 5000) {
      sw_y <- shapiro.test(y_clean)
      if (sw_y$p.value < 0.05) {
        normality_note <- paste(normality_note,
                                "WARNING: Shapiro-Wilk suggests y may not be normally distributed.")
      }
    }
  }

  variance_note <- NULL
  if (!is.null(y) && !paired) {
    y_clean   <- na.omit(y)
    var_test  <- var.test(x_clean, y_clean)
    if (var_test$p.value < 0.05) {
      variance_note <- "WARNING: Variances appear unequal. Welch correction applied."
    }
  }

  if (is.null(y)) {
    test_type  <- "One-Sample T-Test"
    result     <- t.test(x, mu = mu, conf.level = conf.level)
    group_info <- sprintf("n = %d  |  Hypothesised mean (mu) = %.2f",
                          length(x_clean), mu)
  } else if (paired) {
    if (length(x_clean) != length(na.omit(y))) {
      stop("For a paired t-test, x and y must have the same number of observations.")
    }
    test_type  <- "Paired Samples T-Test"
    result     <- t.test(x, y, paired = TRUE, conf.level = conf.level)
    group_info <- sprintf("n (pairs) = %d", length(x_clean))
  } else {
    test_type  <- "Independent Samples T-Test"
    result     <- t.test(x, y, paired = FALSE, conf.level = conf.level)
    group_info <- sprintf("Group 1: n = %d  |  Group 2: n = %d",
                          length(x_clean), length(na.omit(y)))
  }

  t_val <- result$statistic
  df    <- result$parameter
  p_val <- result$p.value
  ci    <- result$conf.int
  alpha <- 1 - conf.level

  if (is.null(y)) {
    d <- (mean(x_clean) - mu) / sd(x_clean)
  } else if (paired) {
    diff <- na.omit(x - y)
    d    <- mean(diff) / sd(diff)
  } else {
    pooled_sd <- sqrt((sd(x_clean)^2 + sd(na.omit(y))^2) / 2)
    d         <- (mean(x_clean) - mean(na.omit(y))) / pooled_sd
  }

  d_abs        <- abs(d)
  effect_label <- if (d_abs < 0.2) "negligible" else if
  (d_abs < 0.5) "small" else if
  (d_abs < 0.8) "moderate" else "large"

  sig_label <- if (p_val < alpha) {
    sprintf("statistically significant (p = %.3f < alpha %.2f)", p_val, alpha)
  } else {
    sprintf("not statistically significant (p = %.3f > alpha %.2f)", p_val, alpha)
  }

  if (!is.null(y)) {
    direction <- if (mean(x_clean) > mean(na.omit(y))) {
      sprintf("Group 1 had a higher mean (%.2f vs %.2f).",
              mean(x_clean), mean(na.omit(y)))
    } else {
      sprintf("Group 2 had a higher mean (%.2f vs %.2f).",
              mean(na.omit(y)), mean(x_clean))
    }
  } else {
    direction <- sprintf("Sample mean was %.2f vs hypothesised %.2f.",
                         mean(x_clean), mu)
  }

  output <- list(
    test_type      = test_type,
    var_name       = var_name,
    group_info     = group_info,
    t_val          = t_val,
    df             = df,
    p_val          = p_val,
    ci             = ci,
    conf.level     = conf.level,
    cohens_d       = d,
    effect_label   = effect_label,
    sig_label      = sig_label,
    direction      = direction,
    normality_note = normality_note,
    variance_note  = variance_note
  )

  class(output) <- "statease_ttest"
  output
}

#' @export
print.statease_ttest <- function(x, ...) {
  cat("\n")
  cat("-- statease T-Test Report ----------------------------------------\n")
  cat(sprintf("  Test         : %s\n", x$test_type))
  cat(sprintf("  Variable     : %s\n", x$var_name))
  cat(sprintf("  Groups       : %s\n", x$group_info))
  cat("-----------------------------------------------------------------\n")
  cat(sprintf("  t-statistic  : %.3f\n", x$t_val))
  cat(sprintf("  df           : %.1f\n", x$df))
  cat(sprintf("  p-value      : %.4f\n", x$p_val))
  cat(sprintf("  %d%% CI      : [%.3f, %.3f]\n",
              as.integer(x$conf.level * 100), x$ci[1], x$ci[2]))
  cat(sprintf("  Cohen's d    : %.3f (%s effect)\n", x$cohens_d, x$effect_label))
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The result is %s.\n", x$sig_label))
  cat(sprintf("  %s\n", x$direction))
  cat(sprintf("  Effect size is %s (d = %.3f).\n", x$effect_label, x$cohens_d))
  cat(sprintf("  %d%% CI: true difference lies between %.3f and %.3f.\n",
              as.integer(x$conf.level * 100), x$ci[1], x$ci[2]))
  if (!is.null(x$normality_note)) cat(sprintf("\n  %s\n", x$normality_note))
  if (!is.null(x$variance_note)) cat(sprintf("  %s\n", x$variance_note))
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
