#' One-Way ANOVA with Post-Hoc Tukey and Plain-English Interpretation
#'
#' @param formula A formula of the form outcome ~ group
#' @param data A data frame containing the variables
#' @param conf.level Confidence level. Default 0.95
#'
#' @return An object of class \code{statease_anova} containing ANOVA
#'   results, effect size, and post-hoc comparisons. Use \code{print()}
#'   to display the formatted report.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   score = c(23,45,12,67,34,89,56,43,78,90,11,34),
#'   group = rep(c("A","B","C"), each = 4)
#' )
#' result <- anova_interpret(score ~ group, data = df)
#' print(result)
anova_interpret <- function(formula, data, conf.level = 0.95) {

  if (!inherits(formula, "formula")) {
    stop("Please provide a valid formula e.g. score ~ group")
  }
  if (!is.data.frame(data)) stop("data must be a data frame.")
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1.")
  }

  alpha     <- 1 - conf.level
  vars      <- all.vars(formula)
  outcome   <- vars[1]
  group_var <- vars[2]

  if (!outcome %in% names(data)) {
    stop(sprintf("Outcome variable '%s' not found in data.", outcome))
  }
  if (!group_var %in% names(data)) {
    stop(sprintf("Group variable '%s' not found in data.", group_var))
  }
  if (!is.numeric(data[[outcome]])) {
    stop(sprintf("Outcome variable '%s' must be numeric.", outcome))
  }

  data[[group_var]] <- as.factor(data[[group_var]])
  groups            <- levels(data[[group_var]])
  n_groups          <- length(groups)

  if (n_groups < 2) stop("Group variable must have at least 2 levels.")
  if (n_groups < 3) {
    warning("Only 2 groups detected. Consider using ttest_interpret() instead.")
  }

  group_ns <- tapply(data[[outcome]], data[[group_var]],
                     function(x) sum(!is.na(x)))
  if (any(group_ns < 2)) {
    stop("Each group must have at least 2 non-missing observations.")
  }
  if (any(group_ns < 10)) {
    warning("One or more groups have small sample sizes (n < 10). Interpret with caution.")
  }

  normality_notes <- c()
  for (g in groups) {
    grp_data <- na.omit(data[[outcome]][data[[group_var]] == g])
    if (length(grp_data) >= 3 && length(grp_data) <= 5000) {
      sw <- shapiro.test(grp_data)
      if (sw$p.value < 0.05) {
        normality_notes <- c(normality_notes,
                             sprintf("WARNING: Group '%s' may not be normally distributed (p = %.4f).",
                                     g, sw$p.value))
      }
    }
  }

  variance_note <- NULL
  bart <- tryCatch(
    bartlett.test(formula, data = data),
    error = function(e) NULL
  )
  if (!is.null(bart) && bart$p.value < 0.05) {
    variance_note <- sprintf(
      "WARNING: Bartlett's test suggests unequal variances (p = %.4f).",
      bart$p.value)
  }

  aov_model   <- aov(formula, data = data)
  aov_sum     <- summary(aov_model)[[1]]
  f_val       <- aov_sum[["F value"]][1]
  p_val       <- aov_sum[["Pr(>F)"]][1]
  df1         <- aov_sum[["Df"]][1]
  df2         <- aov_sum[["Df"]][2]
  group_means <- tapply(data[[outcome]], data[[group_var]], mean, na.rm = TRUE)
  ss_between  <- aov_sum[["Sum Sq"]][1]
  ss_total    <- sum(aov_sum[["Sum Sq"]])
  eta_sq      <- ss_between / ss_total

  eta_label <- if (eta_sq < 0.01) "negligible" else if
  (eta_sq < 0.06) "small" else if
  (eta_sq < 0.14) "moderate" else "large"

  sig_label <- if (p_val < alpha) {
    sprintf("statistically significant (p = %.4f < alpha %.2f)", p_val, alpha)
  } else {
    sprintf("not statistically significant (p = %.4f > alpha %.2f)", p_val, alpha)
  }

  tukey_df <- NULL
  if (p_val < alpha) {
    tukey    <- TukeyHSD(aov_model, conf.level = conf.level)
    tukey_df <- as.data.frame(tukey[[group_var]])
    tukey_df$comparison <- rownames(tukey_df)
  }

  output <- list(
    outcome         = outcome,
    group_var       = group_var,
    n_groups        = n_groups,
    groups          = groups,
    group_means     = group_means,
    group_ns        = group_ns,
    f_val           = f_val,
    df1             = df1,
    df2             = df2,
    p_val           = p_val,
    eta_sq          = eta_sq,
    eta_label       = eta_label,
    sig_label       = sig_label,
    alpha           = alpha,
    conf.level      = conf.level,
    tukey_df        = tukey_df,
    normality_notes = normality_notes,
    variance_note   = variance_note
  )

  class(output) <- "statease_anova"
  output
}

#' @export
print.statease_anova <- function(x, ...) {
  cat("\n")
  cat("-- statease ANOVA Report -----------------------------------------\n")
  cat(sprintf("  Outcome      : %s\n", x$outcome))
  cat(sprintf("  Group        : %s  (%d levels)\n", x$group_var, x$n_groups))
  cat("-----------------------------------------------------------------\n")
  cat("  Group Means:\n")
  for (g in x$groups) {
    cat(sprintf("    %-12s : Mean = %.2f  (n = %d)\n",
                g, x$group_means[g], x$group_ns[g]))
  }
  cat("-----------------------------------------------------------------\n")
  cat(sprintf("  F-statistic  : %.3f\n", x$f_val))
  cat(sprintf("  df           : %d, %d\n", x$df1, x$df2))
  cat(sprintf("  p-value      : %.4f\n", x$p_val))
  cat(sprintf("  Eta squared  : %.4f (%s effect)\n", x$eta_sq, x$eta_label))
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The overall ANOVA result is %s.\n", x$sig_label))
  cat(sprintf("  Group differences explain %.1f%% of variance\n", x$eta_sq * 100))
  cat(sprintf("  (eta^2 = %.4f, %s effect).\n", x$eta_sq, x$eta_label))

  if (length(x$normality_notes) > 0) {
    cat("\n")
    for (note in x$normality_notes) cat(sprintf("  %s\n", note))
  }
  if (!is.null(x$variance_note)) cat(sprintf("  %s\n", x$variance_note))

  if (!is.null(x$tukey_df)) {
    cat("\n")
    cat("-- Post-Hoc Tukey HSD --------------------------------------------\n")
    for (i in seq_len(nrow(x$tukey_df))) {
      comp    <- x$tukey_df$comparison[i]
      diff    <- x$tukey_df$diff[i]
      p_tukey <- x$tukey_df$`p adj`[i]
      sig     <- if (p_tukey < x$alpha) "[significant]" else "[not significant]"
      cat(sprintf("  %s\n", comp))
      cat(sprintf("    Mean diff = %.3f  |  p adj = %.4f  |  %s\n",
                  diff, p_tukey, sig))
    }
    cat("-----------------------------------------------------------------\n")
    cat("  Note: Tukey HSD controls for family-wise error rate.\n")
  } else {
    cat("\n  Post-hoc tests not run (overall result not significant).\n")
  }
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
