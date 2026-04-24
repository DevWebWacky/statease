#' Descriptive Statistics with Interpretation
#'
#' @param x A numeric vector
#' @param var_name Optional name for the variable (used in the report)
#'
#' @return A printed summary report
#' @export
#'
#' @examples
#' describe(c(23, 45, 12, 67, 34, 89, 56))
describe <- function(x, var_name = "Variable") {

  # --- Guard clauses ---
  if (!is.numeric(x)) stop("Input must be a numeric vector.")
  if (length(x) < 2) stop("At least 2 observations are required.")
  if (all(is.na(x))) stop("All values are missing. Please check your data.")

  x_clean <- na.omit(x)

  if (length(x_clean) < 2) stop("At least 2 non-missing observations are required.")

  # --- Warn if small sample ---
  if (length(x_clean) < 10) {
    warning("Sample size is small (n < 10). Interpret descriptive statistics with caution.")
  }

  # --- Core calculations ---
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

  # --- Skewness ---
  skew <- (3 * (mn - med)) / sd_val

  skew_label <- if (abs(skew) < 0.5) {
    "approximately symmetric"
  } else if (skew > 0.5) {
    "positively skewed (tail to the right)"
  } else {
    "negatively skewed (tail to the left)"
  }

  # --- Spread ---
  cv <- (sd_val / mn) * 100

  spread_label <- if (cv < 15) {
    "low variability"
  } else if (cv < 35) {
    "moderate variability"
  } else {
    "high variability"
  }

  # --- Normality check (Shapiro-Wilk, only if n >= 3 and n <= 5000) ---
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

  # --- Print report ---
  cat("\n")
  cat("- statease Descriptive Report - \n")
  cat(sprintf("  Variable     : %s\n", var_name))
  cat(sprintf("  N            : %d  |  Missing: %d\n", n, missing))
  cat("-\n")
  cat(sprintf("  Mean         : %.2f\n", mn))
  cat(sprintf("  Median       : %.2f\n", med))
  cat(sprintf("  Std Dev      : %.2f\n", sd_val))
  cat(sprintf("  Min          : %.2f  |  Max: %.2f\n", min_val, max_val))
  cat(sprintf("  Q1           : %.2f  |  Q3: %.2f\n", q1, q3))
  cat(sprintf("  IQR          : %.2f\n", iqr_val))
  cat("-\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The distribution is %s.\n", skew_label))
  cat(sprintf("  Spread shows %s (CV = %.1f%%).\n", spread_label, cv))
  if (!is.null(normality_note)) {
    cat(sprintf("  %s\n", normality_note))
  }
  if (missing > 0) {
    cat(sprintf("  WARNING: %d missing value(s) were excluded from analysis.\n", missing))
  }
  cat("-\n\n")
}
