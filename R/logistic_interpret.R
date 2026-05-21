#' Logistic Regression with Plain-English Interpretation
#'
#' @param formula A formula of the form outcome ~ predictor1 + predictor2 + ...
#' @param data A data frame containing the variables
#' @param conf.level Confidence level. Default 0.95.
#'
#' @return An object of class \code{statease_logistic} containing logistic
#'   regression results and interpretation. Use \code{print()} to
#'   display the formatted report.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   passed      = c(1,1,0,1,0,1,1,0,1,1,0,0,1,1,0),
#'   study_hours = c(9,8,3,7,2,9,8,3,7,6,2,1,8,7,3),
#'   attendance  = c(90,85,50,80,45,95,88,55,78,70,40,35,92,83,52)
#' )
#' result <- logistic_interpret(passed ~ study_hours + attendance, data = df)
#' print(result)
logistic_interpret <- function(formula, data, conf.level = 0.95) {

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

  outcome    <- vars[1]
  predictors <- vars[-1]

  # --- Check variables exist ---
  for (v in vars) {
    if (!v %in% names(data)) {
      stop(sprintf("Variable '%s' not found in data.", v))
    }
  }

  # --- Check outcome is binary ---
  outcome_vals <- unique(na.omit(data[[outcome]]))
  if (length(outcome_vals) != 2) {
    stop(sprintf(
      "Outcome variable '%s' must be binary (e.g. 0/1 or Yes/No).",
      outcome))
  }

  # --- Sample size check ---
  n <- nrow(na.omit(data[, vars]))
  if (n < 20) {
    warning("Sample size is small (n < 20). Interpret results with caution.")
  }

  # --- Run logistic regression ---
  model     <- glm(formula, data = data, family = binomial())
  model_sum <- summary(model)
  coefs     <- coef(model_sum)
  ci <- tryCatch(
    confint(model, level = conf.level),
    error = function(e) confint.default(model, level = conf.level),
    warning = function(w) confint.default(model, level = conf.level)
  )

  # --- Odds ratios ---
  or     <- exp(coef(model))
  or_ci  <- exp(ci)

  # --- Nagelkerke R-squared ---
  null_dev  <- model$null.deviance
  res_dev   <- model$deviance
  n_obs     <- model$df.null + 1
  cox_snell <- 1 - exp((res_dev - null_dev) / n_obs)
  nagelkerke <- cox_snell / (1 - exp(-null_dev / n_obs))

  # --- R-squared label ---
  r_sq_label <- if (nagelkerke < 0.01) {
    "negligible"
  } else if (nagelkerke < 0.09) {
    "small"
  } else if (nagelkerke < 0.25) {
    "moderate"
  } else {
    "large"
  }

  # --- Overall model significance (likelihood ratio test) ---
  chi_model <- null_dev - res_dev
  df_model  <- model$df.null - model$df.residual
  p_model   <- pchisq(chi_model, df_model, lower.tail = FALSE)

  model_sig <- if (p_model < alpha) {
    sprintf("statistically significant (p = %.4f < alpha %.2f)", p_model, alpha)
  } else {
    sprintf("not statistically significant (p = %.4f > alpha %.2f)", p_model, alpha)
  }

  # --- Individual predictors ---
  predictor_results <- list()
  for (i in seq_along(predictors)) {
    pred    <- predictors[i]
    row_idx <- i + 1
    b       <- coefs[row_idx, 1]
    se      <- coefs[row_idx, 2]
    z_val   <- coefs[row_idx, 3]
    p_val   <- coefs[row_idx, 4]
    odds    <- or[row_idx]
    ci_low  <- or_ci[row_idx, 1]
    ci_high <- or_ci[row_idx, 2]

    sig <- if (p_val < alpha) "significant" else "not significant"

    or_interpret <- if (odds > 1) {
      sprintf("each unit increase in %s increases the odds by %.1f%%",
              pred, (odds - 1) * 100)
    } else {
      sprintf("each unit increase in %s decreases the odds by %.1f%%",
              pred, (1 - odds) * 100)
    }

    predictor_results[[pred]] <- list(
      name         = pred,
      b            = b,
      se           = se,
      z_val        = z_val,
      p_val        = p_val,
      odds         = odds,
      ci_low       = ci_low,
      ci_high      = ci_high,
      sig          = sig,
      or_interpret = or_interpret
    )
  }

  output <- list(
    outcome           = outcome,
    predictors        = predictors,
    n                 = n,
    intercept         = coefs[1, 1],
    nagelkerke        = nagelkerke,
    r_sq_label        = r_sq_label,
    chi_model         = chi_model,
    df_model          = df_model,
    p_model           = p_model,
    model_sig         = model_sig,
    predictor_results = predictor_results,
    conf.level        = conf.level,
    alpha             = alpha
  )

  class(output) <- "statease_logistic"
  output
}

#' @export
print.statease_logistic <- function(x, ...) {
  cat("\n")
  cat("-- statease Logistic Regression Report --------------------------\n")
  cat(sprintf("  Outcome      : %s\n", x$outcome))
  cat(sprintf("  Predictors   : %s\n", paste(x$predictors, collapse = ", ")))
  cat(sprintf("  N            : %d\n", x$n))
  cat("-----------------------------------------------------------------\n")
  cat("  Overall Model Fit:\n")
  cat(sprintf("  Chi-square   : %.3f  (df = %d)  p = %.4f\n",
              x$chi_model, x$df_model, x$p_model))
  cat(sprintf("  Nagelkerke R2: %.4f (%s effect)\n",
              x$nagelkerke, x$r_sq_label))
  cat(sprintf("  The overall model is %s.\n", x$model_sig))
  cat("-----------------------------------------------------------------\n")
  cat("  Individual Predictors:\n")
  for (pred in x$predictors) {
    pr <- x$predictor_results[[pred]]
    cat(sprintf("\n  %s\n", pred))
    cat(sprintf("    Coefficient  : %.3f  (SE = %.3f)\n", pr$b, pr$se))
    cat(sprintf("    z-statistic  : %.3f\n", pr$z_val))
    cat(sprintf("    p-value      : %.4f  [%s]\n", pr$p_val, pr$sig))
    cat(sprintf("    Odds Ratio   : %.3f\n", pr$odds))
    cat(sprintf("    %d%% CI (OR) : [%.3f, %.3f]\n",
                as.integer(x$conf.level * 100), pr$ci_low, pr$ci_high))
    cat(sprintf("    Interpretation: %s.\n", pr$or_interpret))
  }
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The model is %s.\n", x$model_sig))
  cat(sprintf("  Nagelkerke R2 = %.4f suggests a %s amount of\n",
              x$nagelkerke, x$r_sq_label))
  cat(sprintf("  variance in %s is explained by the predictors.\n",
              x$outcome))

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
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
