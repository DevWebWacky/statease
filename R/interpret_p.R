#' Standalone P-Value Interpreter
#'
#' @param p A numeric p-value between 0 and 1
#' @param alpha Significance level. Default 0.05
#' @param context Optional string describing the test context
#'
#' @return A printed plain-English interpretation of the p-value
#' @export
#'
#' @examples
#' interpret_p(0.03)
#' interpret_p(0.12, alpha = 0.05, context = "comparing treatment vs control")
interpret_p <- function(p, alpha = 0.05, context = NULL) {

  # --- Guard clauses ---
  if (!is.numeric(p) || length(p) != 1) stop("p must be a single numeric value.")
  if (p < 0 || p > 1) stop("p-value must be between 0 and 1.")
  if (alpha <= 0 || alpha >= 1) stop("alpha must be between 0 and 1.")

  # --- Significance decision ---
  significant <- p < alpha

  # --- Strength of evidence label ---
  evidence <- if (p < 0.001) {
    "very strong evidence against the null hypothesis"
  } else if (p < 0.01) {
    "strong evidence against the null hypothesis"
  } else if (p < 0.05) {
    "moderate evidence against the null hypothesis"
  } else if (p < 0.10) {
    "weak evidence against the null hypothesis (borderline)"
  } else {
    "little to no evidence against the null hypothesis"
  }

  # --- Decision label ---
  decision <- if (significant) {
    sprintf("REJECT the null hypothesis at alpha = %.2f", alpha)
  } else {
    sprintf("FAIL TO REJECT the null hypothesis at alpha = %.2f", alpha)
  }

  # --- Plain English verdict ---
  verdict <- if (significant) {
    "The result is statistically significant. The observed data is unlikely
  to have occurred by chance alone if the null hypothesis were true."
  } else {
    "The result is not statistically significant. The observed data does
  not provide sufficient evidence to reject the null hypothesis."
  }

  # --- Important reminder ---
  reminder <- if (significant) {
    "Remember: statistical significance does not imply practical importance.
  Always consider effect size alongside the p-value."
  } else {
    "Remember: a non-significant result does not prove the null hypothesis
  is true. It may reflect insufficient power or a small sample size."
  }

  # --- Print report ---
  cat("\n")
  cat("- statease P-Value Interpretation -\n")
  if (!is.null(context)) {
    cat(sprintf("  Context      : %s\n", context))
  }
  cat(sprintf("  P-value      : %.4f\n", p))
  cat(sprintf("  Alpha        : %.2f\n", alpha))
  cat("-\n")
  cat(sprintf("  Decision     : %s\n", decision))
  cat(sprintf("  Evidence     : There is %s.\n", evidence))
  cat("-\n")
  cat("  Interpretation:\n")
  cat(sprintf("  %s\n", verdict))
  cat("\n")
  cat(sprintf("  WARNING: %s\n", reminder))
  cat("-\n\n")
}
