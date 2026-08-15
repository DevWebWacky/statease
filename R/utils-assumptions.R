# Internal shared regression diagnostic helpers.
# Used by reg_interpret(), mlr_interpret(), and check_assumptions()
# so all three compute homoscedasticity, independence, and VIF the
# same way, from one source, rather than three separate implementations.

.se_check_homoscedasticity <- function(model) {
  bp <- tryCatch(car::ncvTest(model), error = function(e) NULL)
  if (is.null(bp)) return(NULL)
  list(
    p      = bp$p,
    status = if (bp$p >= 0.05) "PASSED" else "WARNING"
  )
}

.se_check_independence <- function(model) {
  dw <- tryCatch(car::durbinWatsonTest(model), error = function(e) NULL)
  if (is.null(dw)) return(NULL)
  list(
    dw     = dw$dw,
    p      = dw$p,
    status = if (dw$p >= 0.05) "PASSED" else "WARNING"
  )
}

.se_check_vif <- function(model) {
  vif_vals <- tryCatch(car::vif(model), error = function(e) NULL)
  if (is.null(vif_vals)) return(NULL)
  max_vif <- max(vif_vals)
  list(
    max_vif = max_vif,
    status  = if (max_vif < 5) "PASSED" else "WARNING"
  )
}

.se_check_separation <- function(model) {
  coefs <- suppressWarnings(summary(model)$coefficients)
  se    <- coefs[, "Std. Error"]
  est   <- coefs[, "Estimate"]

  # Signal 1: any coefficient with an implausibly large standard error
  # (a well-known numerical fingerprint of separation, not a formal test)
  huge_se <- any(se > 20, na.rm = TRUE)

  # Signal 2: fitted probabilities pushed to the numerical boundary
  fitted_vals    <- fitted(model)
  extreme_fitted <- any(fitted_vals < 1e-6 | fitted_vals > 1 - 1e-6)

  # Signal 3: glm() itself failed to converge cleanly
  no_converge <- !isTRUE(model$converged)

  flagged <- huge_se || extreme_fitted || no_converge

  list(
    status = if (flagged) "WARNING" else "PASSED",
    detail = if (flagged) {
      "possible separation detected; coefficient estimates may be unreliable"
    } else {
      "no obvious numerical evidence of separation"
    }
  )
}
