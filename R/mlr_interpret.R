#' Multiple Linear Regression with Plain-English Interpretation
#'
#' @param formula A formula of the form outcome ~ predictor1 + predictor2 + ...
#' @param data A data frame containing the variables
#' @param conf.level Confidence level. Default 0.95.
#'
#' @return An object of class \code{statease_mlr} containing multiple
#'   regression results and interpretation. Use \code{print()} to
#'   display the formatted report.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   exam_score  = c(23,45,12,67,34,89,56,43,78,90),
#'   study_hours = c(2,5,1,7,3,9,6,4,8,10),
#'   attendance  = c(60,80,50,90,70,95,85,75,88,92)
#' )
#' result <- mlr_interpret(exam_score ~ study_hours + attendance, data = df)
#' print(result)
mlr_interpret <- function(formula, data, conf.level = 0.95) {

  # --- Guard clauses ---
  if (!inherits(formula, "formula")) {
    stop("Please provide a valid formula e.g. outcome ~ pred1 + pred2")
  }
  if (!is.data.frame(data)) stop("data must be a data frame.")
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1.")
  }

  alpha <- 1 - conf.level
  vars  <- all.vars(formula)

  if (length(vars) < 3) {
    stop("Please provide at least two predictors e.g. outcome ~ pred1 + pred2")
  }

  outcome    <- vars[1]
  predictors <- vars[-1]

  # --- Check variables exist ---
  for (v in vars) {
    if (!v %in% names(data)) {
      stop(sprintf("Variable '%s' not found in data.", v))
    }
  }

  # --- Check outcome is numeric ---
  if (!is.numeric(data[[outcome]])) {
    stop(sprintf("Outcome variable '%s' must be numeric.", outcome))
  }

  # --- Sample size check ---
  n <- nrow(na.omit(data[, vars]))
  if (n < length(vars) + 1) {
    stop("Not enough observations for the number of predictors.")
  }
  if (n < 20) {
    warning("Sample size is small (n < 20). Interpret results with caution.")
  }

  # --- Run regression ---
  model     <- lm(formula, data = data)
  model_sum <- summary(model)
  coefs     <- coef(model_sum)
  ci        <- confint(model, level = conf.level)

  # --- Extract overall model fit ---
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

  # --- Overall model significance ---
  model_sig <- if (f_p < alpha) {
    sprintf("statistically significant (p = %.4f < alpha %.2f)", f_p, alpha)
  } else {
    sprintf("not statistically significant (p = %.4f > alpha %.2f)", f_p, alpha)
  }

  # --- Individual predictors ---
  predictor_results <- list()
  for (i in seq_along(predictors)) {
    pred     <- predictors[i]
    row_idx  <- i + 1
    b        <- coefs[row_idx, 1]
    se       <- coefs[row_idx, 2]
    t_val    <- coefs[row_idx, 3]
    p_val    <- coefs[row_idx, 4]
    ci_low   <- ci[row_idx, 1]
    ci_high  <- ci[row_idx, 2]

    sig <- if (p_val < alpha) "significant" else "not significant"

    direction <- if (b > 0) {
      sprintf("positive (b = %.3f)", b)
    } else {
      sprintf("negative (b = %.3f)", b)
    }

    predictor_results[[pred]] <- list(
      name      = pred,
      b         = b,
      se        = se,
      t_val     = t_val,
      p_val     = p_val,
      ci_low    = ci_low,
      ci_high   = ci_high,
      sig       = sig,
      direction = direction
    )
  }

  # --- Residual diagnostics ---
  res            <- residuals(model)
  normality_note <- NULL
  if (length(res) >= 3 && length(res) <= 5000) {
    sw <- shapiro.test(res)
    if (sw$p.value < 0.05) {
      normality_note <- sprintf(
        "WARNING: Residuals may not be normally distributed (Shapiro-Wilk p = %.4f).",
        sw$p.value)
    }
  }

  output <- list(
    outcome           = outcome,
    predictors        = predictors,
    n                 = n,
    intercept         = coefs[1, 1],
    r_squared         = r_squared,
    adj_r_squared     = adj_r_squared,
    r_sq_label        = r_sq_label,
    f_val             = f_val,
    f_df1             = f_df1,
    f_df2             = f_df2,
    f_p               = f_p,
    model_sig         = model_sig,
    predictor_results = predictor_results,
    conf.level        = conf.level,
    alpha             = alpha,
    normality_note    = normality_note
  )

  class(output) <- "statease_mlr"
  output
}

#' @export
print.statease_mlr <- function(x, ...) {
  cat("\n")
  cat("-- statease Multiple Linear Regression Report -------------------\n")
  cat(sprintf("  Outcome      : %s\n", x$outcome))
  cat(sprintf("  Predictors   : %s\n", paste(x$predictors, collapse = ", ")))
  cat(sprintf("  N            : %d\n", x$n))
  cat("-----------------------------------------------------------------\n")
  cat("  Model Equation:\n")
  cat(sprintf("  %s = %.3f", x$outcome, x$intercept))
  for (pred in x$predictors) {
    b <- x$predictor_results[[pred]]$b
    if (b >= 0) {
      cat(sprintf(" + %.3f*%s", b, pred))
    } else {
      cat(sprintf(" - %.3f*%s", abs(b), pred))
    }
  }
  cat("\n")
  cat("-----------------------------------------------------------------\n")
  cat("  Overall Model Fit:\n")
  cat(sprintf("  R-squared    : %.4f (%s effect)\n",
              x$r_squared, x$r_sq_label))
  cat(sprintf("  Adj R-squared: %.4f\n", x$adj_r_squared))
  cat(sprintf("  F-statistic  : %.3f (df = %.0f, %.0f)  p = %.4f\n",
              x$f_val, x$f_df1, x$f_df2, x$f_p))
  cat(sprintf("  The overall model is %s.\n", x$model_sig))
  cat("-----------------------------------------------------------------\n")
  cat("  Individual Predictors:\n")
  for (pred in x$predictors) {
    pr <- x$predictor_results[[pred]]
    cat(sprintf("\n  %s\n", pred))
    cat(sprintf("    Coefficient  : %.3f  (SE = %.3f)\n", pr$b, pr$se))
    cat(sprintf("    t-statistic  : %.3f\n", pr$t_val))
    cat(sprintf("    p-value      : %.4f  [%s]\n", pr$p_val, pr$sig))
    cat(sprintf("    %d%% CI      : [%.3f, %.3f]\n",
                as.integer(x$conf.level * 100), pr$ci_low, pr$ci_high))
    cat(sprintf("    Direction    : %s\n", pr$direction))
  }
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The model explains %.1f%% of the variance in %s\n",
              x$r_squared * 100, x$outcome))
  cat(sprintf("  (R-squared = %.4f, %s effect).\n",
              x$r_squared, x$r_sq_label))
  cat(sprintf("  Adjusted R-squared = %.4f accounting for\n",
              x$adj_r_squared))
  cat(sprintf("  the number of predictors in the model.\n"))

  sig_preds <- Filter(function(p) p$sig == "significant",
                      x$predictor_results)
  nonsig_preds <- Filter(function(p) p$sig == "not significant",
                         x$predictor_results)

  if (length(sig_preds) > 0) {
    cat(sprintf("\n  Significant predictors: %s\n",
                paste(names(sig_preds), collapse = ", ")))
  }
  if (length(nonsig_preds) > 0) {
    cat(sprintf("  Non-significant predictors: %s\n",
                paste(names(nonsig_preds), collapse = ", ")))
  }
  if (!is.null(x$normality_note)) {
    cat(sprintf("\n  %s\n", x$normality_note))
  }
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
