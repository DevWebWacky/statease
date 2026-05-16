#' Correlation Analysis with Plain-English Interpretation
#'
#' @param x A numeric vector
#' @param y A numeric vector
#' @param method Correlation method: "pearson", "spearman", or "kendall".
#'   Default "pearson".
#' @param conf.level Confidence level. Default 0.95.
#' @param var1_name Optional name for first variable. Default "Variable 1"
#' @param var2_name Optional name for second variable. Default "Variable 2"
#'
#' @return An object of class \code{statease_cor} containing correlation
#'   results and interpretation. Use \code{print()} to display the
#'   formatted report.
#' @export
#'
#' @examples
#' x <- c(23, 45, 12, 67, 34, 89, 56, 43, 78, 90)
#' y <- c(19, 42, 15, 70, 30, 85, 52, 48, 80, 88)
#' result <- cor_interpret(x, y)
#' print(result)
cor_interpret <- function(x, y, method = "pearson", conf.level = 0.95,
                          var1_name = "Variable 1",
                          var2_name = "Variable 2") {

  # --- Guard clauses ---
  if (!is.numeric(x)) stop("x must be a numeric vector.")
  if (!is.numeric(y)) stop("y must be a numeric vector.")
  if (length(x) != length(y)) stop("x and y must have the same length.")
  if (!method %in% c("pearson", "spearman", "kendall")) {
    stop("method must be one of 'pearson', 'spearman', or 'kendall'.")
  }
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1.")
  }
  if (length(na.omit(x)) < 3) stop("At least 3 non-missing observations are required.")

  alpha   <- 1 - conf.level
  n       <- length(na.omit(x))
  missing <- sum(is.na(x) | is.na(y))

  # --- Small sample warning ---
  if (n < 10) {
    warning("Sample size is small (n < 10). Interpret correlation with caution.")
  }

  # --- Normality check for Pearson ---
  normality_note <- NULL
  if (method == "pearson") {
    sw_x <- shapiro.test(na.omit(x))
    sw_y <- shapiro.test(na.omit(y))
    if (sw_x$p.value < 0.05 || sw_y$p.value < 0.05) {
      normality_note <- paste("WARNING: One or both variables may not be",
                              "normally distributed.",
                              "Consider using method = 'spearman' instead.")
    }
  }

  # --- Run correlation test ---
  result <- cor.test(x, y, method = method, conf.level = conf.level)
  r      <- result$estimate
  p_val  <- result$p.value
  ci     <- result$conf.int

  # --- Effect size label ---
  r_abs   <- abs(r)
  r_label <- if (r_abs < 0.1) {
    "negligible"
  } else if (r_abs < 0.3) {
    "small"
  } else if (r_abs < 0.5) {
    "moderate"
  } else if (r_abs < 0.7) {
    "large"
  } else {
    "very large"
  }

  # --- Direction ---
  direction <- if (r > 0) {
    "positive (as one variable increases, the other tends to increase)"
  } else {
    "negative (as one variable increases, the other tends to decrease)"
  }

  # --- Significance ---
  sig_label <- if (p_val < alpha) {
    sprintf("statistically significant (p = %.4f < alpha %.2f)", p_val, alpha)
  } else {
    sprintf("not statistically significant (p = %.4f > alpha %.2f)", p_val, alpha)
  }

  # --- Method label ---
  method_label <- switch(method,
                         "pearson"  = "Pearson Product-Moment Correlation",
                         "spearman" = "Spearman Rank Correlation",
                         "kendall"  = "Kendall Rank Correlation"
  )

  output <- list(
    var1_name      = var1_name,
    var2_name      = var2_name,
    method_label   = method_label,
    method         = method,
    n              = n,
    missing        = missing,
    r              = r,
    r_label        = r_label,
    direction      = direction,
    p_val          = p_val,
    ci             = ci,
    sig_label      = sig_label,
    conf.level     = conf.level,
    normality_note = normality_note,
    alpha          = alpha
  )

  class(output) <- "statease_cor"
  output
}

#' @export
print.statease_cor <- function(x, ...) {
  cat("\n")
  cat("-- statease Correlation Report -----------------------------------\n")
  cat(sprintf("  Method       : %s\n", x$method_label))
  cat(sprintf("  Variables    : %s & %s\n", x$var1_name, x$var2_name))
  cat(sprintf("  N            : %d  |  Missing: %d\n", x$n, x$missing))
  cat("-----------------------------------------------------------------\n")
  cat(sprintf("  r            : %.4f\n", x$r))
  cat(sprintf("  p-value      : %.4f\n", x$p_val))
  if (!is.null(x$ci)) {
    cat(sprintf("  %d%% CI      : [%.4f, %.4f]\n",
                as.integer(x$conf.level * 100), x$ci[1], x$ci[2]))
  }
  cat(sprintf("  Strength     : %s\n", x$r_label))
  cat(sprintf("  Direction    : %s\n", x$direction))
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The correlation is %s.\n", x$sig_label))
  cat(sprintf("  The relationship between %s and %s is\n",
              x$var1_name, x$var2_name))
  cat(sprintf("  %s and %s in direction.\n", x$r_label, x$direction))
  if (!is.null(x$normality_note)) {
    cat(sprintf("\n  %s\n", x$normality_note))
  }
  if (x$missing > 0) {
    cat(sprintf("  WARNING: %d missing value(s) were excluded.\n", x$missing))
  }
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
