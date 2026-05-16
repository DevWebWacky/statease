#' Chi-Square Test with Plain-English Interpretation
#'
#' @param x A factor or character vector (first categorical variable)
#' @param y A factor or character vector (second categorical variable)
#' @param correct Logical. Apply Yates continuity correction. Default TRUE.
#' @param conf.level Confidence level. Default 0.95.
#'
#' @return An object of class \code{statease_chisq} containing test
#'   results and interpretation. Use \code{print()} to display the
#'   formatted report.
#' @export
#'
#' @examples
#' x <- c("Yes","No","Yes","Yes","No","Yes","No","No","Yes","Yes")
#' y <- c("Male","Female","Male","Female","Male","Female","Male","Female","Male","Female")
#' result <- chisq_interpret(x, y)
#' print(result)
chisq_interpret <- function(x, y, correct = TRUE, conf.level = 0.95) {

  # --- Guard clauses ---
  if (!is.vector(x) && !is.factor(x)) stop("x must be a vector or factor.")
  if (!is.vector(y) && !is.factor(y)) stop("y must be a vector or factor.")
  if (length(x) != length(y)) stop("x and y must have the same length.")
  if (conf.level <= 0 || conf.level >= 1) stop("conf.level must be between 0 and 1.")

  alpha <- 1 - conf.level

  # --- Convert to factor ---
  x <- as.factor(x)
  y <- as.factor(y)

  # --- Contingency table ---
  cont_table <- table(x, y)

  # --- Check expected frequencies ---
  expected   <- chisq.test(cont_table, correct = correct)$expected
  low_exp    <- any(expected < 5)
  low_exp_note <- if (low_exp) {
    "WARNING: Some expected frequencies are less than 5. Interpret with caution."
  } else NULL

  # --- Run chi-square test ---
  result  <- chisq.test(cont_table, correct = correct)
  chi_val <- result$statistic
  df      <- result$parameter
  p_val   <- result$p.value

  # --- Cramers V (effect size) ---
  n         <- sum(cont_table)
  min_dim   <- min(nrow(cont_table), ncol(cont_table)) - 1
  cramers_v <- sqrt(chi_val / (n * min_dim))

  v_label <- if (cramers_v < 0.1) {
    "negligible"
  } else if (cramers_v < 0.3) {
    "small"
  } else if (cramers_v < 0.5) {
    "moderate"
  } else {
    "large"
  }

  # --- Significance ---
  sig_label <- if (p_val < alpha) {
    sprintf("statistically significant (p = %.4f < alpha %.2f)", p_val, alpha)
  } else {
    sprintf("not statistically significant (p = %.4f > alpha %.2f)", p_val, alpha)
  }

  # --- Association direction ---
  assoc_label <- if (p_val < alpha) {
    "There is a significant association between the two variables."
  } else {
    "There is no significant association between the two variables."
  }

  output <- list(
    cont_table   = cont_table,
    expected     = expected,
    chi_val      = chi_val,
    df           = df,
    p_val        = p_val,
    cramers_v    = cramers_v,
    v_label      = v_label,
    sig_label    = sig_label,
    assoc_label  = assoc_label,
    low_exp_note = low_exp_note,
    alpha        = alpha,
    conf.level   = conf.level,
    n            = n
  )

  class(output) <- "statease_chisq"
  output
}

#' @export
print.statease_chisq <- function(x, ...) {
  cat("\n")
  cat("-- statease Chi-Square Test Report ------------------------------\n")
  cat(sprintf("  N            : %d\n", x$n))
  cat("-----------------------------------------------------------------\n")
  cat("  Contingency Table (Observed):\n")
  print(x$cont_table)
  cat("\n")
  cat("  Expected Frequencies:\n")
  print(round(x$expected, 2))
  cat("\n")
  cat("-----------------------------------------------------------------\n")
  cat(sprintf("  Chi-square   : %.3f\n", x$chi_val))
  cat(sprintf("  df           : %d\n", x$df))
  cat(sprintf("  p-value      : %.4f\n", x$p_val))
  cat(sprintf("  Cramer's V   : %.3f (%s effect)\n", x$cramers_v, x$v_label))
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The result is %s.\n", x$sig_label))
  cat(sprintf("  %s\n", x$assoc_label))
  cat(sprintf("  Effect size is %s (V = %.3f).\n", x$v_label, x$cramers_v))
  if (!is.null(x$low_exp_note)) {
    cat(sprintf("\n  %s\n", x$low_exp_note))
  }
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
