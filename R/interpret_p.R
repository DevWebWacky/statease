#' Standalone P-Value Interpreter
#'
#' @param p A numeric p-value between 0 and 1
#' @param alpha Significance level. Default 0.05
#' @param context Optional string describing the test context
#'
#' @return An object of class \code{statease_pvalue} containing the
#'   p-value interpretation. Use \code{print()} to display the report.
#' @export
#'
#' @examples
#' result <- interpret_p(0.03)
#' print(result)
#'
#' result2 <- interpret_p(0.12, alpha = 0.05, context = "treatment vs control")
#' print(result2)
interpret_p <- function(p, alpha = 0.05, context = NULL) {

  if (!is.numeric(p) || length(p) != 1) stop("p must be a single numeric value.")
  if (p < 0 || p > 1) stop("p-value must be between 0 and 1.")
  if (alpha <= 0 || alpha >= 1) stop("alpha must be between 0 and 1.")

  significant <- p < alpha

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

  decision <- if (significant) {
    sprintf("REJECT the null hypothesis at alpha = %.2f", alpha)
  } else {
    sprintf("FAIL TO REJECT the null hypothesis at alpha = %.2f", alpha)
  }

  verdict <- if (significant) {
    paste("The result is statistically significant. The observed data is",
          "unlikely to have occurred by chance if the null hypothesis were true.")
  } else {
    paste("The result is not statistically significant. The observed data",
          "does not provide sufficient evidence to reject the null hypothesis.")
  }

  reminder <- if (significant) {
    paste("Note: Statistical significance does not imply practical importance.",
          "Always consider effect size alongside the p-value.")
  } else {
    paste("Note: A non-significant result does not prove the null hypothesis.",
          "It may reflect insufficient power or a small sample size.")
  }

  output <- list(
    p           = p,
    alpha       = alpha,
    context     = context,
    significant = significant,
    evidence    = evidence,
    decision    = decision,
    verdict     = verdict,
    reminder    = reminder
  )

  class(output) <- "statease_pvalue"
  output
}

#' @export
print.statease_pvalue <- function(x, ...) {
  cat("\n")
  cat("-- statease P-Value Interpretation ------------------------------\n")
  if (!is.null(x$context)) {
    cat(sprintf("  Context      : %s\n", x$context))
  }
  cat(sprintf("  P-value      : %.4f\n", x$p))
  cat(sprintf("  Alpha        : %.2f\n", x$alpha))
  cat("-----------------------------------------------------------------\n")
  cat(sprintf("  Decision     : %s\n", x$decision))
  cat(sprintf("  Evidence     : There is %s.\n", x$evidence))
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  %s\n", x$verdict))
  cat("\n")
  cat(sprintf("  %s\n", x$reminder))
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
