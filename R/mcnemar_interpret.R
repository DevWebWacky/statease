#' McNemar's Test with Plain-English Interpretation
#'
#' @param x A factor or character vector (first measurement)
#' @param y A factor or character vector (second measurement)
#' @param conf.level Confidence level. Default 0.95.
#'
#' @return An object of class \code{statease_mcnemar} containing test
#'   results and interpretation. Use \code{print()} to display the
#'   formatted report.
#' @export
#'
#' @examples
#' x <- c("Yes","No","Yes","Yes","No","Yes","No","No","Yes","Yes")
#' y <- c("No","No","Yes","Yes","No","Yes","Yes","No","Yes","No")
#' result <- mcnemar_interpret(x, y)
#' print(result)
mcnemar_interpret <- function(x, y, conf.level = 0.95) {

  #Guard clauses
  if (!is.vector(x) && !is.factor(x)) {
    stop("x must be a vector or factor.")
  }
  if (!is.vector(y) && !is.factor(y)) {
    stop("y must be a vector or factor.")
  }
  if (length(x) != length(y)) {
    stop("x and y must have the same length.")
  }
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1.")
  }

  alpha <- 1 - conf.level

  #Convert to factor with same levels
  all_levels <- union(levels(as.factor(x)), levels(as.factor(y)))
  x <- factor(x, levels = all_levels)
  y <- factor(y, levels = all_levels)

  #Contingency table
  cont_table <- table(x, y)
  n          <- sum(cont_table)
  n_rows     <- nrow(cont_table)
  n_cols     <- ncol(cont_table)

  #Check for 2x2 table
  if (n_rows != 2 || n_cols != 2) {
    warning(paste("McNemar's Test is designed for 2x2 tables.",
                  "For larger tables consider using the",
                  "Stuart-Maxwell test instead."))
  }

  #Discordant pairs
  if (n_rows == 2 && n_cols == 2) {
    b <- cont_table[1, 2]  # x=1, y=2
    c <- cont_table[2, 1]  # x=2, y=1
    discordant <- b + c
  } else {
    discordant <- NA
  }

  #Run McNemar's Test
  result <- mcnemar.test(cont_table)
  p_val  <- result$statistic
  p_val  <- result$p.value

  #Matched Odds Ratio
  or    <- if (n_rows == 2 && n_cols == 2 && c != 0) b / c else NA
  or_ci <- if (!is.na(or) && c != 0) {
    se_log_or <- sqrt(1/b + 1/c)
    z         <- qnorm(1 - alpha/2)
    c(exp(log(or) - z * se_log_or),
      exp(log(or) + z * se_log_or))
  } else {
    NULL
  }

  #  Significance
  sig_label <- if (p_val < alpha) {
    sprintf("statistically significant (p = %.4f < alpha %.2f)",
            p_val, alpha)
  } else {
    sprintf("not statistically significant (p = %.4f > alpha %.2f)",
            p_val, alpha)
  }

  # Result interpretation
  result_label <- if (p_val < alpha) {
    "There is evidence of a significant difference in paired proportions between the two measurements."
  } else {
    "There is insufficient evidence of a significant difference in paired proportions between the two measurements."
  }

  # OR interpretation
  or_label <- if (!is.na(or)) {
    if (or > 1) {
      sprintf(paste("Matched OR = %.3f: More subjects changed",
                    "from the first category to the second",
                    "category than vice versa."), or)
    } else if (or < 1) {
      sprintf(paste("Matched OR = %.3f: More subjects changed",
                    "from the second category to the first",
                    "category than vice versa."), or)
    } else {
      sprintf(paste("Matched OR = %.3f: No evidence of",
                    "asymmetry in the discordant pairs."), or)
    }
  } else {
    "Matched OR could not be computed."
  }

  # CI interpretation
  ci_label <- if (!is.null(or_ci)) {
    if (or_ci[1] > 1 || or_ci[2] < 1) {
      sprintf(paste("%.0f%% CI [%.3f, %.3f] excludes 1:",
                    "Evidence of a significant difference",
                    "in paired proportions between measurements."),
              conf.level * 100, or_ci[1], or_ci[2])
    } else {
      sprintf(paste("%.0f%% CI [%.3f, %.3f] includes 1:",
                    "No statistically significant evidence",
                    "of a difference in paired proportions."),
              conf.level * 100, or_ci[1], or_ci[2])
    }
  } else {
    NULL
  }

  # Warnings and Notes
  warnings_list <- c()
  notes_list    <- c()

  # Small discordant pairs
  if (!is.na(discordant) && discordant < 10) {
    warnings_list <- c(warnings_list,
                       paste("The number of discordant pairs is very small",
                             "(less than 10). Results may be unreliable.",
                             "Interpret with caution."))
  }

  # Zero cells
  if (any(cont_table == 0)) {
    warnings_list <- c(warnings_list,
                       paste("One or more cells contain zero counts.",
                             "Matched odds ratio may be unstable,",
                             "undefined, or infinite.",
                             "Interpret results with caution."))
  }

  # Independence warning
  warnings_list <- c(warnings_list,
                     paste("McNemar's Test assumes that observations",
                           "are paired and independent across pairs.",
                           "Violation of this assumption may affect",
                           "the validity of the results."))

  # Paired data note
  notes_list <- c(notes_list,
                  paste("NOTE: McNemar's Test requires paired or",
                        "matched data. Ensure that each row in your",
                        "data represents the same subject measured",
                        "twice or a matched pair."))

  # Large table note
  if (n_rows > 2 || n_cols > 2) {
    notes_list <- c(notes_list,
                    paste("NOTE: For tables larger than 2x2 consider",
                          "using the Stuart-Maxwell test instead,",
                          "which is a generalization of McNemar's",
                          "Test for larger tables."))
  }

  output <- list(
    cont_table    = cont_table,
    n             = n,
    n_rows        = n_rows,
    n_cols        = n_cols,
    discordant    = discordant,
    p_val         = p_val,
    or            = or,
    or_ci         = or_ci,
    sig_label     = sig_label,
    result_label  = result_label,
    or_label      = or_label,
    ci_label      = ci_label,
    warnings_list = warnings_list,
    notes_list    = notes_list,
    conf.level    = conf.level,
    alpha         = alpha
  )

  class(output) <- "statease_mcnemar"
  output
}

#' @export
print.statease_mcnemar <- function(x, ...) {
  cat("\n")
  cat("statease McNemar's Test Report --------------------------------\n")
  cat(sprintf("  N            : %d\n", x$n))
  cat(sprintf("  Table size   : %d x %d\n", x$n_rows, x$n_cols))
  if (!is.na(x$discordant)) {
    cat(sprintf("  Discordant   : %d pairs\n", x$discordant))
  }
  cat("-----------------------------------------------------------------\n")
  cat("  Contingency Table:\n")
  print(x$cont_table)
  cat("\n")
  cat("-----------------------------------------------------------------\n")
  cat(sprintf("  p-value      : %.4f\n", x$p_val))
  if (!is.na(x$or)) {
    cat(sprintf("  Matched OR   : %.3f\n", x$or))
  }
  if (!is.null(x$or_ci)) {
    cat(sprintf("  %d%% CI      : [%.3f, %.3f]\n",
                as.integer(x$conf.level * 100),
                x$or_ci[1], x$or_ci[2]))
  }
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The result is %s.\n", x$sig_label))
  cat(sprintf("  %s\n", x$result_label))
  if (!is.null(x$or_label)) {
    cat(sprintf("  %s\n", x$or_label))
  }
  if (!is.null(x$ci_label)) {
    cat(sprintf("  %s\n", x$ci_label))
  }
  if (length(x$warnings_list) > 0) {
    cat("\n")
    for (w in x$warnings_list) {
      cat(sprintf("  WARNING: %s\n", w))
    }
  }
  if (length(x$notes_list) > 0) {
    cat("\n")
    for (n in x$notes_list) {
      cat(sprintf("  %s\n", n))
    }
  }
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
