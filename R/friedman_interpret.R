#' Friedman Test with Plain-English Interpretation
#'
#' @param formula A formula of the form outcome ~ time | subject
#' @param data A data frame containing the variables
#' @param conf.level Confidence level. Default 0.95.
#'
#' @return An object of class \code{statease_friedman} containing test
#'   results and interpretation. Use \code{print()} to display the
#'   formatted report.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   score   = c(23,45,12,67,34,89,56,43,78,90,11,34),
#'   time    = rep(c("T1","T2","T3"), each = 4),
#'   subject = rep(1:4, times = 3)
#' )
#' result <- friedman_interpret(score ~ time | subject, data = df)
#' print(result)
friedman_interpret <- function(formula, data, conf.level = 0.95) {

  #Guard clauses
  if (!inherits(formula, "formula")) {
    stop("Please provide a valid formula e.g. score ~ time | subject")
  }
  if (!is.data.frame(data)) stop("data must be a data frame.")
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1.")
  }

  alpha <- 1 - conf.level

  #Extract variables from formula
  formula_str <- deparse(formula)
  if (!grepl("\\|", formula_str)) {
    stop("Formula must include subject variable e.g. score ~ time | subject")
  }

  parts     <- strsplit(formula_str, "\\|")[[1]]
  left_part <- trimws(parts[1])
  subject   <- trimws(parts[2])
  left_vars <- all.vars(as.formula(left_part))
  outcome   <- left_vars[1]
  time_var  <- left_vars[2]

  #Check variables exist
  for (v in c(outcome, time_var, subject)) {
    if (!v %in% names(data)) {
      stop(sprintf("Variable '%s' not found in data.", v))
    }
  }

  #Check outcome is numeric
  if (!is.numeric(data[[outcome]])) {
    stop(sprintf("Outcome variable '%s' must be numeric.", outcome))
  }

  #Convert time and subject to factor
  data[[time_var]] <- as.factor(data[[time_var]])
  data[[subject]]  <- as.factor(data[[subject]])

  groups   <- levels(data[[time_var]])
  n_groups <- length(groups)
  n_subj   <- length(levels(data[[subject]]))

  if (n_groups < 3) {
    stop("Friedman Test requires at least 3 time points or conditions.")
  }

  # Sample size check
  if (n_subj < 10) {
    warning(paste("The Friedman Test may have low statistical power",
                  "with very small sample sizes.",
                  "Interpret non-significant results with caution."))
  }

  # Group medians
  group_medians <- tapply(data[[outcome]],
                          data[[time_var]],
                          median, na.rm = TRUE)

  # Run Friedman Test
  result  <- friedman.test(formula, data = data)
  chi_val <- result$statistic
  df      <- result$parameter
  p_val   <- result$p.value

  # Kendall's W (effect size)
  kendall_w <- chi_val / (n_subj * (n_groups - 1))

  w_label <- if (kendall_w < 0.1) {
    "negligible"
  } else if (kendall_w < 0.3) {
    "weak"
  } else if (kendall_w < 0.5) {
    "moderate"
  } else {
    "strong"
  }

  # Significance
  sig_label <- if (p_val < alpha) {
    sprintf("statistically significant (p = %.4f < alpha %.2f)",
            p_val, alpha)
  } else {
    sprintf("not statistically significant (p = %.4f > alpha %.2f)",
            p_val, alpha)
  }

  # Result interpretation
  result_label <- if (p_val < alpha) {
    "There is evidence of a significant difference in ranks across the related groups or repeated measurements."
  } else {
    "There is insufficient evidence of a significant difference in ranks across the related groups or repeated measurements."
  }

  # Post-hoc pairwise Wilcoxon
  posthoc <- NULL
  if (p_val < alpha) {
    pairs     <- combn(groups, 2, simplify = FALSE)
    posthoc   <- list()
    p_vals_ph <- c()

    for (pair in pairs) {
      g1   <- pair[1]
      g2   <- pair[2]
      x1   <- data[[outcome]][data[[time_var]] == g1]
      x2   <- data[[outcome]][data[[time_var]] == g2]
      wt   <- suppressWarnings(
        wilcox.test(x1, x2, paired = TRUE, exact = FALSE)
      )
      comp           <- paste(g1, "vs", g2)
      p_vals_ph[comp] <- wt$p.value
    }

    # Bonferroni correction
    p_adj <- p.adjust(p_vals_ph, method = "bonferroni")

    for (comp in names(p_adj)) {
      posthoc[[comp]] <- list(
        comparison = comp,
        p_val      = p_vals_ph[comp],
        p_adj      = p_adj[comp],
        sig        = if (p_adj[comp] < alpha) {
          "[significant]"
        } else {
          "[not significant]"
        }
      )
    }
  }

  #Normality check
  normality_note <- NULL
  vals <- na.omit(data[[outcome]])
  if (length(vals) >= 3 && length(vals) <= 5000) {
    sw <- shapiro.test(vals)
    if (sw$p.value >= 0.05) {
      normality_note <- paste(
        "NOTE: Normality assumption appears reasonable.",
        "If the assumptions of repeated measures ANOVA",
        "are met, consider using repeated measures ANOVA",
        "for greater statistical power.")
    }
  }

  # Independence warning
  independence_warning <- paste(
    "The Friedman Test assumes that the blocks",
    "(subjects) are independent of each other.",
    "Violation of this assumption may affect",
    "the validity of the results.")

  output <- list(
    outcome              = outcome,
    time_var             = time_var,
    subject              = subject,
    n_groups             = n_groups,
    n_subj               = n_subj,
    groups               = groups,
    group_medians        = group_medians,
    chi_val              = chi_val,
    df                   = df,
    p_val                = p_val,
    kendall_w            = kendall_w,
    w_label              = w_label,
    sig_label            = sig_label,
    result_label         = result_label,
    posthoc              = posthoc,
    normality_note       = normality_note,
    independence_warning = independence_warning,
    conf.level           = conf.level,
    alpha                = alpha
  )

  class(output) <- "statease_friedman"
  output
}

#' @export
print.statease_friedman <- function(x, ...) {
  cat("\n")
  cat("-- statease Friedman Test Report \n")
  cat(sprintf("  Outcome      : %s\n", x$outcome))
  cat(sprintf("  Time/Group   : %s (%d levels)\n",
              x$time_var, x$n_groups))
  cat(sprintf("  Subjects     : %s (n = %d)\n",
              x$subject, x$n_subj))
  cat("-----------------------------------------------------------------\n")
  cat("  Group Medians (descriptive only):\n")
  for (g in x$groups) {
    cat(sprintf("    %-12s : %.2f\n", g, x$group_medians[g]))
  }
  cat("-----------------------------------------------------------------\n")
  cat(sprintf("  Chi-square   : %.3f\n", x$chi_val))
  cat(sprintf("  df           : %d\n", x$df))
  cat(sprintf("  p-value      : %.4f\n", x$p_val))
  cat(sprintf("  Kendall's W  : %.4f (%s effect)\n",
              x$kendall_w, x$w_label))
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The result is %s.\n", x$sig_label))
  cat(sprintf("  %s\n", x$result_label))
  cat("\n")
  cat("  NOTE: Medians are reported for descriptive purposes only.\n")
  cat("  The Friedman Test assesses whether rank distributions\n")
  cat("  differ across groups and does not directly test for\n")
  cat("  differences in medians.\n")

  if (!is.null(x$posthoc)) {
    cat("\n")
    cat("-- Post-Hoc Pairwise Wilcoxon Signed-Rank Tests -----------------\n")
    cat("  (Bonferroni correction applied)\n\n")
    for (comp in names(x$posthoc)) {
      ph <- x$posthoc[[comp]]
      cat(sprintf("  %s\n", ph$comparison))
      cat(sprintf("    p = %.4f  |  p adj = %.4f  |  %s\n",
                  ph$p_val, ph$p_adj, ph$sig))
    }
  } else {
    cat("\n  Post-hoc tests not run (overall result not significant).\n")
  }

  cat("\n")
  cat(sprintf("  WARNING: %s\n", x$independence_warning))

  if (!is.null(x$normality_note)) {
    cat(sprintf("\n  %s\n", x$normality_note))
  }
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
