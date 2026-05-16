#' Simple Linear Regression with Plain-English Interpretation
#'
#' @param formula A formula of the form outcome ~ predictor
#' @param data A data frame containing the variables
#' @param conf.level Confidence level. Default 0.95.
#'
#' @return An object of class \code{statease_reg} containing regression
#'   results and interpretation. Use \code{print()} to display the
#'   formatted report.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   exam_score = c(23,45,12,67,34,89,56,43,78,90),
#'   study_hours = c(2,5,1,7,3,9,6,4,8,10)
#' )
#' result <- reg_interpret(exam_score ~ study_hours, data = df)
#' print(result)
reg_interpret <- function(formula, data, conf.level = 0.95) {

  # --- Guard clauses ---
  if (!inherits(formula, "formula")) {
    stop("Please provide a valid formula e.g. outcome ~ predictor")
  }
  if (!is.data.frame(data)) stop("data must be a data frame.")
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1.")
  }

  alpha <- 1 - conf.level

  # --- Extract variable names ---
  vars      <- all.vars(formula)
  outcome   <- vars[1]
  predictor <- vars[2]

  # --- Check variables exist ---
  if (!outcome %in% names(data)) {
    stop(sprintf("Outcome variable '%s' not found in data.", outcome))
  }
  if (!predictor %in% names(data)) {
    stop(sprintf("Predictor variable '%s' not found in data.", predictor))
  }

  # --- Check variables are numeric ---
  if (!is.numeric(data[[outcome]])) {
    stop(sprintf("Outcome variable '%s' must be numeric.", outcome))
  }
  if (!is.numeric(data[[predictor]])) {
    stop(sprintf("Predictor variable '%s' must be numeric.", predictor))
  }

  # --- Sample size check ---
  n <- nrow(na.omit(data[, c(outcome, predictor)]))
  if (n < 3) stop("At least 3 complete observations are required.")
  if (n < 10) {
    warning("Sample size is small (n < 10). Interpret results with caution.")
  }

  # --- Run regression ---
  model     <- lm(formula, data = data)
  model_sum <- summary(model)
  coefs     <- coef(model_sum)
  ci        <- confint(model, level = conf.level)

  # --- Extract values ---
  intercept     <- coefs[1, 1]
  slope         <- coefs[2, 1]
  slope_se      <- coefs[2, 2]
  slope_t       <- coefs[2, 3]
  slope_p       <- coefs[2, 4]
  r_squared     <- model_sum$r.squared
  adj_r_squared <- model_sum$adj.r.squared
  f_val         <- model_sum$fstatistic[1]
  f_df1         <- model_sum$fstatistic[2]
  f_df2         <- model_sum$fstatistic[3]
  f_p           <- pf(f_val, f_df1, f_df2, lower.tail = FALSE)

  # --- R-squared interpretation ---
  r_sq_label <- if (r_squared < 0.01) {
    "negligible"
  } else if (r_squared < 0.09) {
    "small"
  } else if (r_squared < 0.25) {
    "moderate"
  } else {
    "large"
  }

  # --- Slope direction ---
  slope_direction <- if (slope > 0) {
    sprintf("positive - as %s increases by 1 unit, %s increases by %.3f units",
            predictor, outcome, slope)
  } else {
    sprintf("negative - as %s increases by 1 unit, %s decreases by %.3f units",
            predictor, outcome, abs(slope))
  }

  # --- Significance ---
  sig_label <- if (slope_p < alpha) {
    sprintf("statistically significant (p = %.4f < alpha %.2f)", slope_p, alpha)
  } else {
    sprintf("not statistically significant (p = %.4f > alpha %.2f)", slope_p, alpha)
  }

  # --- Residual diagnostics ---
  residuals     <- residuals(model)
  fitted_values <- fitted(model)

  normality_note <- NULL
  if (length(residuals) >= 3 && length(residuals) <= 5000) {
    sw <- shapiro.test(residuals)
    if (sw$p.value < 0.05) {
      normality_note <- sprintf(
        "WARNING: Residuals may not be normally distributed (Shapiro-Wilk p = %.4f).",
        sw$p.value)
    }
  }

  output <- list(
    outcome        = outcome,
    predictor      = predictor,
    n              = n,
    intercept      = intercept,
    slope          = slope,
    slope_se       = slope_se,
    slope_t        = slope_t,
    slope_p        = slope_p,
    ci             = ci,
    r_squared      = r_squared,
    adj_r_squared  = adj_r_squared,
    r_sq_label     = r_sq_label,
    f_val          = f_val,
    f_df1          = f_df1,
    f_df2          = f_df2,
    f_p            = f_p,
    slope_direction = slope_direction,
    sig_label      = sig_label,
    normality_note = normality_note,
    conf.level     = conf.level,
    alpha          = alpha
  )

  class(output) <- "statease_reg"
  output
}

#' @export
print.statease_reg <- function(x, ...) {
  cat("\n")
  cat("-- statease Simple Linear Regression Report ---------------------\n")
  cat(sprintf("  Outcome      : %s\n", x$outcome))
  cat(sprintf("  Predictor    : %s\n", x$predictor))
  cat(sprintf("  N            : %d\n", x$n))
  cat("-----------------------------------------------------------------\n")
  cat("  Model Equation:\n")
  cat(sprintf("  %s = %.3f + %.3f * %s\n",
              x$outcome, x$intercept, x$slope, x$predictor))
  cat("-----------------------------------------------------------------\n")
  cat("  Coefficients:\n")
  cat(sprintf("  Intercept    : %.3f\n", x$intercept))
  cat(sprintf("  Slope        : %.3f  (SE = %.3f)\n", x$slope, x$slope_se))
  cat(sprintf("  t-statistic  : %.3f\n", x$slope_t))
  cat(sprintf("  p-value      : %.4f\n", x$slope_p))
  cat(sprintf("  %d%% CI      : [%.3f, %.3f]\n",
              as.integer(x$conf.level * 100),
              x$ci[2, 1], x$ci[2, 2]))
  cat("-----------------------------------------------------------------\n")
  cat("  Model Fit:\n")
  cat(sprintf("  R-squared    : %.4f\n", x$r_squared))
  cat(sprintf("  Adj R-squared: %.4f\n", x$adj_r_squared))
  cat(sprintf("  F-statistic  : %.3f (df = %.0f, %.0f)  p = %.4f\n",
              x$f_val, x$f_df1, x$f_df2, x$f_p))
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The predictor %s is %s.\n", x$predictor, x$sig_label))
  cat(sprintf("  The slope is %s.\n", x$slope_direction))
  cat(sprintf("  R-squared = %.4f: %s explains %.1f%% of the\n",
              x$r_squared, x$predictor, x$r_squared * 100))
  cat(sprintf("  variance in %s (%s effect).\n", x$outcome, x$r_sq_label))
  if (!is.null(x$normality_note)) {
    cat(sprintf("\n  %s\n", x$normality_note))
  }
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
