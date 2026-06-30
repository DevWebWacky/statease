#' Check Statistical Assumptions Before Running a Test
#'
#' @param test The test you plan to run. One of "ttest", "anova",
#'   "anova2", "correlation", "regression".
#' @param x A numeric vector (required for most tests)
#' @param y A numeric vector, factor, or character group variable
#'   (optional depending on test)
#' @param data A data frame (required for anova, anova2, regression)
#' @param formula A formula (required for anova, anova2, regression)
#'
#' @return An object of class \code{statease_assumptions} containing
#'   assumption check results. Use \code{print()} to display the
#'   formatted report.
#' @export
#'
#' @examples
#' x <- c(23, 45, 12, 67, 34, 89, 56, 43, 78, 90)
#' y <- c(19, 38, 22, 51, 29, 74, 44, 38, 65, 80)
#' result <- check_assumptions("ttest", x = x, y = y)
#' print(result)
check_assumptions <- function(test, x = NULL, y = NULL,
                              data = NULL, formula = NULL) {

  valid_tests <- c("ttest", "anova", "anova2",
                   "correlation", "regression")
  if (!test %in% valid_tests) {
    stop(sprintf("test must be one of: %s",
                 paste(valid_tests, collapse = ", ")))
  }

  results <- list()

  # --- Helper: normality test selector ---
  check_normality <- function(values, label) {
    values <- na.omit(values)
    n      <- length(values)

    if (n < 3) {
      return(list(
        assumption = sprintf("Normality (%s)", label),
        status     = "WARNING",
        detail     = "Sample size too small to test normality (n < 3)."
      ))
    }

    if (n <= 5000) {
      test_result <- shapiro.test(values)
      test_name   <- "Shapiro-Wilk"
      stat        <- test_result$statistic
      p           <- test_result$p.value
    } else {
      test_result <- ks.test(values, "pnorm",
                             mean(values), sd(values))
      test_name   <- "Kolmogorov-Smirnov"
      stat        <- test_result$statistic
      p           <- test_result$p.value
    }

    status <- if (p >= 0.05) "PASSED" else "WARNING"
    detail <- sprintf(
      "%s test: statistic = %.3f, p = %.4f. %s",
      test_name, stat, p,
      if (status == "PASSED") {
        "Normality assumption appears satisfied."
      } else {
        "Evidence suggests that the normality assumption may be violated."
      }
    )

    list(
      assumption = sprintf("Normality (%s)", label),
      status     = status,
      detail     = detail
    )
  }

  # --- Helper: sample size guidance ---
  sample_size_note <- function(n, label = "") {
    list(
      assumption = sprintf("Sample size guidance%s",
                           if (nchar(label) > 0) {
                             paste0(" (", label, ")")
                           } else ""),
      status     = if (n < 10) "WARNING" else "PASSED",
      detail     = sprintf(
        "n = %d. %s", n,
        if (n < 10) {
          "Small sample size. There is no formal assumption of sample size adequacy, but results should be interpreted with caution."
        } else {
          "Sample size appears reasonable."
        }
      )
    )
  }

  # T-TEST
  if (test == "ttest") {
    if (is.null(x)) stop("x is required for ttest assumption checks.")

    results[[length(results) + 1]] <- check_normality(x, "x")
    results[[length(results) + 1]] <- sample_size_note(length(na.omit(x)), "x")

    if (!is.null(y) && is.numeric(y)) {
      results[[length(results) + 1]] <- check_normality(y, "y")
      results[[length(results) + 1]] <- sample_size_note(length(na.omit(y)), "y")

      lev <- tryCatch({
        car::leveneTest(c(x, y) ~ as.factor(c(rep("x", length(x)),
                                              rep("y", length(y)))))
      }, error = function(e) NULL)

      if (!is.null(lev)) {
        p <- lev[1, "Pr(>F)"]
        status <- if (p >= 0.05) "PASSED" else "WARNING"
        results[[length(results) + 1]] <- list(
          assumption = "Homogeneity of variance",
          status     = status,
          detail     = sprintf(
            "Levene's Test: p = %.4f. %s", p,
            if (status == "PASSED") {
              "Variances appear approximately equal."
            } else {
              "Evidence suggests unequal variances. Consider Welch correction (used automatically in ttest_interpret())."
            }
          )
        )
      }
    }
  }

  # ONE-WAY ANOVA
  if (test == "anova") {
    if (is.null(formula) || is.null(data)) {
      stop("formula and data are required for anova assumption checks.")
    }

    vars      <- all.vars(formula)
    outcome   <- vars[1]
    group_var <- vars[2]
    data[[group_var]] <- as.factor(data[[group_var]])
    groups    <- levels(data[[group_var]])

    for (g in groups) {
      vals <- data[[outcome]][data[[group_var]] == g]
      results[[length(results) + 1]] <- check_normality(vals, paste("group:", g))
      results[[length(results) + 1]] <- sample_size_note(length(na.omit(vals)), g)
    }

    bart <- tryCatch(
      bartlett.test(formula, data = data),
      error = function(e) NULL
    )
    if (!is.null(bart)) {
      status <- if (bart$p.value >= 0.05) "PASSED" else "WARNING"
      results[[length(results) + 1]] <- list(
        assumption = "Homogeneity of variance",
        status     = status,
        detail     = sprintf(
          "Bartlett's Test: p = %.4f. %s", bart$p.value,
          if (status == "PASSED") {
            "Variances appear approximately equal across groups."
          } else {
            "Evidence suggests unequal variances across groups. Consider kruskal_interpret() as a non-parametric alternative."
          }
        )
      )
    }
  }

  # TWO-WAY ANOVA
  if (test == "anova2") {
    if (is.null(formula) || is.null(data)) {
      stop("formula and data are required for anova2 assumption checks.")
    }

    vars    <- all.vars(formula)
    outcome <- vars[1]
    group1  <- vars[2]
    group2  <- vars[3]

    data[[group1]] <- as.factor(data[[group1]])
    data[[group2]] <- as.factor(data[[group2]])

    model <- lm(formula, data = data)
    res   <- residuals(model)

    results[[length(results) + 1]] <- check_normality(res, "residuals")
    results[[length(results) + 1]] <- sample_size_note(
      nrow(na.omit(data[, vars])), "overall"
    )

    bart <- tryCatch(
      bartlett.test(data[[outcome]] ~
                      interaction(data[[group1]], data[[group2]])),
      error = function(e) NULL
    )
    if (!is.null(bart)) {
      status <- if (bart$p.value >= 0.05) "PASSED" else "WARNING"
      results[[length(results) + 1]] <- list(
        assumption = "Homogeneity of variance",
        status     = status,
        detail     = sprintf(
          "Bartlett's Test: p = %.4f. %s", bart$p.value,
          if (status == "PASSED") {
            "Variances appear approximately equal across cells."
          } else {
            "Evidence suggests unequal variances across cells."
          }
        )
      )
    }
  }

  # CORRELATION
  if (test == "correlation") {
    if (is.null(x) || is.null(y)) {
      stop("x and y are required for correlation assumption checks.")
    }

    results[[length(results) + 1]] <- check_normality(x, "x (for Pearson only)")
    results[[length(results) + 1]] <- check_normality(y, "y (for Pearson only)")
    results[[length(results) + 1]] <- sample_size_note(length(na.omit(x)))

    results[[length(results) + 1]] <- list(
      assumption = "Linearity",
      status     = "INFO",
      detail     = paste("Linearity assessment (recommended visual",
                         "inspection). Create a scatterplot of x",
                         "against y to visually assess linearity",
                         "before interpreting Pearson correlation.")
    )
  }

  # REGRESSION
  if (test == "regression") {
    if (is.null(formula) || is.null(data)) {
      stop("formula and data are required for regression assumption checks.")
    }

    model <- lm(formula, data = data)
    res   <- residuals(model)

    results[[length(results) + 1]] <- check_normality(res, "residuals")
    results[[length(results) + 1]] <- sample_size_note(nrow(model$model))

    # Homoscedasticity
    bp <- tryCatch(
      car::ncvTest(model),
      error = function(e) NULL
    )
    if (!is.null(bp)) {
      status <- if (bp$p >= 0.05) "PASSED" else "WARNING"
      results[[length(results) + 1]] <- list(
        assumption = "Homoscedasticity",
        status     = status,
        detail     = sprintf(
          "Non-constant variance test: p = %.4f. %s", bp$p,
          if (status == "PASSED") {
            "Homoscedasticity assumption appears satisfied."
          } else {
            "Evidence suggests heteroscedasticity (non-constant variance)."
          }
        )
      )
    }

    # Independence of residuals
    dw <- tryCatch(
      car::durbinWatsonTest(model),
      error = function(e) NULL
    )
    if (!is.null(dw)) {
      status <- if (dw$p >= 0.05) "PASSED" else "WARNING"
      results[[length(results) + 1]] <- list(
        assumption = "Independence of residuals",
        status     = status,
        detail     = sprintf(
          "Durbin-Watson test: DW = %.3f, p = %.4f. %s",
          dw$dw, dw$p,
          if (status == "PASSED") {
            "No strong evidence of autocorrelation in residuals."
          } else {
            "Evidence suggests autocorrelation in residuals."
          }
        )
      )
    }

    # Multicollinearity (VIF) for multiple regression
    n_predictors <- length(all.vars(formula)) - 1
    if (n_predictors > 1) {
      vif_vals <- tryCatch(car::vif(model), error = function(e) NULL)
      if (!is.null(vif_vals)) {
        max_vif <- max(vif_vals)
        status  <- if (max_vif < 5) "PASSED" else "WARNING"
        results[[length(results) + 1]] <- list(
          assumption = "Multicollinearity (VIF)",
          status     = status,
          detail     = sprintf(
            "Maximum VIF = %.2f. %s", max_vif,
            if (status == "PASSED") {
              "No strong evidence of multicollinearity."
            } else {
              "Evidence of multicollinearity (VIF >= 5). Consider removing or combining correlated predictors."
            }
          )
        )
      }
    }
  }

  output <- list(
    test    = test,
    results = results
  )

  class(output) <- "statease_assumptions"
  output
}

#' @export
print.statease_assumptions <- function(x, ...) {
  cat("\n")
  cat("-- statease Assumption Check Report -------------------------------\n")
  cat(sprintf("  Test         : %s\n", x$test))
  cat("---------------------------------------------------------------------\n")

  for (r in x$results) {
    status_label <- switch(r$status,
                           "PASSED"  = "[PASSED]",
                           "WARNING" = "[WARNING]",
                           "FAILED"  = "[FAILED]",
                           "INFO"    = "[INFO]"
    )
    cat(sprintf("\n  %s %s\n", status_label, r$assumption))
    cat(sprintf("    %s\n", r$detail))
  }

  cat("\n---------------------------------------------------------------------\n")
  cat("  NOTE: Assumption checks are based on statistical tests and\n")
  cat("  heuristics. They provide guidance but should not be\n")
  cat("  interpreted as definitive proof that assumptions are met\n")
  cat("  or violated.\n\n")
  cat("  NOTE: Failure to reject an assumption test does not prove\n")
  cat("  that the assumption has been satisfied.\n\n")
  cat("  NOTE: Visual inspection of residual plots is always\n")
  cat("  recommended alongside formal assumption tests.\n")
  cat("---------------------------------------------------------------------\n\n")
  invisible(x)
}
