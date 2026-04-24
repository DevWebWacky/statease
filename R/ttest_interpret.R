#' T-Test with Plain-English Interpretation
#'
#' @param x A numeric vector (group 1, or the only group for one-sample)
#' @param y A numeric vector (group 2, for independent samples). Default NULL.
#' @param mu Hypothesised mean for one-sample t-test. Default 0.
#' @param paired Logical. TRUE for paired t-test. Default FALSE.
#' @param conf.level Confidence level. Default 0.95.
#' @param var_name Optional label for the report. Default "Variable"
#'
#' @return A printed t-test report with interpretation
#' @export
#'
#' @examples
#' ttest_interpret(c(23,45,12,67,34), c(19,38,22,51,29))
ttest_interpret <- function(x, y = NULL, mu = 0, paired = FALSE,
                            conf.level = 0.95, var_name = "Variable") {

  # --- Guard clauses ---
  if (!is.numeric(x)) stop("x must be a numeric vector.")
  if (!is.null(y) && !is.numeric(y)) stop("y must be a numeric vector.")
  if (conf.level <= 0 || conf.level >= 1) stop("conf.level must be between 0 and 1.")
  if (length(na.omit(x)) < 2) stop("x must have at least 2 non-missing values.")

  x_clean <- na.omit(x)

  # --- Small sample warning ---
  if (length(x_clean) < 10) {
    warning("Sample size in x is small (n < 10). Interpret results with caution.")
  }

  # --- Normality check ---
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

  # --- Equal variance check (Levene approximation via F-test) ---
  variance_note <- NULL
  if (!is.null(y) && !paired) {
    y_clean <- na.omit(y)
    var_test <- var.test(x_clean, y_clean)
    if (var_test$p.value < 0.05) {
      variance_note <- "WARNING: Variances appear unequal (F-test p < .05). Welch correction applied."
    }
  }

  # --- Detect test type ---
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

  # --- Extract values ---
  t_val <- result$statistic
  df    <- result$parameter
  p_val <- result$p.value
  ci    <- result$conf.int
  alpha <- 1 - conf.level

  # --- Cohen's d ---
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

  # --- Print report ---
  cat("\n")
  cat("- statease T-Test Report -\n")
  cat(sprintf("  Test         : %s\n", test_type))
  cat(sprintf("  Variable     : %s\n", var_name))
  cat(sprintf("  Groups       : %s\n", group_info))
  cat("-\n")
  cat(sprintf("  t-statistic  : %.3f\n", t_val))
  cat(sprintf("  df           : %.1f\n", df))
  cat(sprintf("  p-value      : %.4f\n", p_val))
  cat(sprintf("  %d%% CI      : [%.3f, %.3f]\n",
              as.integer(conf.level * 100), ci[1], ci[2]))
  cat(sprintf("  Cohen's d    : %.3f (%s effect)\n", d, effect_label))
  cat("-\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The result is %s.\n", sig_label))
  cat(sprintf("  %s\n", direction))
  cat(sprintf("  Effect size is %s (d = %.3f).\n", effect_label, d))
  cat(sprintf("  %d%% CI suggests the true difference lies between\n",
              as.integer(conf.level * 100)))
  cat(sprintf("  %.3f and %.3f.\n", ci[1], ci[2]))
  if (!is.null(normality_note)) cat(sprintf("\n  %s\n", normality_note))
  if (!is.null(variance_note)) cat(sprintf("  %s\n", variance_note))
  cat("-\n\n")
}
