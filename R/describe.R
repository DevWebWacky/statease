#' Descriptive Statistics with Interpretation
#'
#' @param x A numeric vector
#' @param var_name Optional name for the variable (used in the report)
#'
#' @return An object of class \code{statease_describe} containing
#'   descriptive statistics and interpretation. Use \code{print()} to
#'   display the formatted report.
#' @export
#'
#' @examples
#' result <- describe(c(23, 45, 12, 67, 34, 89, 56))
#' print(result)
describe <- function(x, var_name = "Variable") {

  if (!is.numeric(x)) stop("Input must be a numeric vector.")
  if (length(x) < 2) stop("At least 2 observations are required.")
  if (all(is.na(x))) stop("All values are missing. Please check your data.")

  x_clean <- na.omit(x)
  if (length(x_clean) < 2) stop("At least 2 non-missing observations are required.")
  if (length(x_clean) < 10) {
    warning("Sample size is small (n < 10). Interpret descriptive statistics with caution.")
  }

  n       <- length(x)
  missing <- sum(is.na(x))
  mn      <- mean(x_clean)
  med     <- median(x_clean)
  sd_val  <- sd(x_clean)
  min_val <- min(x_clean)
  max_val <- max(x_clean)
  q1      <- quantile(x_clean, 0.25)
  q3      <- quantile(x_clean, 0.75)
  iqr_val <- IQR(x_clean)
  skew    <- (3 * (mn - med)) / sd_val
  cv      <- (sd_val / mn) * 100

  skew_label <- if (abs(skew) < 0.5) {
    "approximately symmetric"
  } else if (skew > 0.5) {
    "positively skewed (tail to the right)"
  } else {
    "negatively skewed (tail to the left)"
  }

  spread_label <- if (cv < 15) {
    "low variability"
  } else if (cv < 35) {
    "moderate variability"
  } else {
    "high variability"
  }

  normality_note <- NULL
  if (length(x_clean) >= 3 && length(x_clean) <= 5000) {
    sw <- shapiro.test(x_clean)
    normality_note <- if (sw$p.value < 0.05) {
      sprintf("Shapiro-Wilk test suggests non-normality (W = %.3f, p = %.4f).",
              sw$statistic, sw$p.value)
    } else {
      sprintf("Shapiro-Wilk test suggests normality is reasonable (W = %.3f, p = %.4f).",
              sw$statistic, sw$p.value)
    }
  }

  result <- list(
    var_name       = var_name,
    n              = n,
    missing        = missing,
    mean           = mn,
    median         = med,
    sd             = sd_val,
    min            = min_val,
    max            = max_val,
    q1             = q1,
    q3             = q3,
    iqr            = iqr_val,
    skew           = skew,
    skew_label     = skew_label,
    cv             = cv,
    spread_label   = spread_label,
    normality_note = normality_note
  )

  class(result) <- "statease_describe"
  result
}

#' @export
print.statease_describe <- function(x, ...) {
  cat("\n")
  cat("-- statease Descriptive Report ----------------------------------\n")
  cat(sprintf("  Variable     : %s\n", x$var_name))
  cat(sprintf("  N            : %d  |  Missing: %d\n", x$n, x$missing))
  cat("-----------------------------------------------------------------\n")
  cat(sprintf("  Mean         : %.2f\n", x$mean))
  cat(sprintf("  Median       : %.2f\n", x$median))
  cat(sprintf("  Std Dev      : %.2f\n", x$sd))
  cat(sprintf("  Min          : %.2f  |  Max: %.2f\n", x$min, x$max))
  cat(sprintf("  Q1           : %.2f  |  Q3: %.2f\n", x$q1, x$q3))
  cat(sprintf("  IQR          : %.2f\n", x$iqr))
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The distribution is %s.\n", x$skew_label))
  cat(sprintf("  Spread shows %s (CV = %.1f%%).\n", x$spread_label, x$cv))
  if (!is.null(x$normality_note)) {
    cat(sprintf("  %s\n", x$normality_note))
  }
  if (x$missing > 0) {
    cat(sprintf("  WARNING: %d missing value(s) were excluded.\n", x$missing))
  }
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
