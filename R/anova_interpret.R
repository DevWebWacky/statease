#' One-Way ANOVA with Post-Hoc Tukey and Plain-English Interpretation
#'
#' @param formula A formula of the form outcome ~ group
#' @param data A data frame containing the variables
#' @param conf.level Confidence level. Default 0.95
#'
#' @return A printed ANOVA report with interpretation and post-hoc results
#' @export
#'
#' @examples
#' df <- data.frame(
#'   score = c(23,45,12,67,34,89,56,43,78,90,11,34),
#'   group = rep(c("A","B","C"), each = 4)
#' )
#' anova_interpret(score ~ group, data = df)
anova_interpret <- function(formula, data, conf.level = 0.95) {

  # --- Guard clauses ---
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

  # --- Check variables exist in data ---
  if (!outcome %in% names(data)) {
    stop(sprintf("Outcome variable '%s' not found in data.", outcome))
  }
  if (!group_var %in% names(data)) {
    stop(sprintf("Group variable '%s' not found in data.", group_var))
  }

  # --- Check outcome is numeric ---
  if (!is.numeric(data[[outcome]])) {
    stop(sprintf("Outcome variable '%s' must be numeric.", outcome))
  }

  # --- Ensure group is a factor ---
  data[[group_var]] <- as.factor(data[[group_var]])
  groups            <- levels(data[[group_var]])
  n_groups          <- length(groups)

  if (n_groups < 2) stop("Group variable must have at least 2 levels.")
  if (n_groups < 3) {
    warning("Only 2 groups detected. Consider using ttest_interpret() instead.")
  }

  # --- Check sample size per group ---
  group_ns <- tapply(data[[outcome]], data[[group_var]],
                     function(x) sum(!is.na(x)))
  if (any(group_ns < 2)) {
    stop("Each group must have at least 2 non-missing observations.")
  }
  if (any(group_ns < 10)) {
    warning("One or more groups have small sample sizes (n < 10). Interpret with caution.")
  }

  # --- Normality check per group ---
  normality_notes <- c()
  for (g in groups) {
    grp_data <- na.omit(data[[outcome]][data[[group_var]] == g])
    if (length(grp_data) >= 3 && length(grp_data) <= 5000) {
      sw <- shapiro.test(grp_data)
      if (sw$p.value < 0.05) {
        normality_notes <- c(normality_notes,
                             sprintf("WARNING: Group '%s' may not be normally distributed (Shapiro-Wilk p = %.4f).",
                                     g, sw$p.value))
      }
    }
  }

  # --- Homogeneity of variance (Bartlett's test) ---
  variance_note <- NULL
  bart <- tryCatch(
    bartlett.test(formula, data = data),
    error = function(e) NULL
  )
  if (!is.null(bart) && bart$p.value < 0.05) {
    variance_note <- sprintf(
      "WARNING: Bartlett's test suggests unequal variances across groups (p = %.4f).",
      bart$p.value)
  }

  # --- Run ANOVA ---
  aov_model <- aov(formula, data = data)
  aov_sum   <- summary(aov_model)[[1]]

  f_val <- aov_sum[["F value"]][1]
  p_val <- aov_sum[["Pr(>F)"]][1]
  df1   <- aov_sum[["Df"]][1]
  df2   <- aov_sum[["Df"]][2]

  # --- Group means ---
  group_means <- tapply(data[[outcome]], data[[group_var]], mean, na.rm = TRUE)

  # --- Eta squared ---
  ss_between <- aov_sum[["Sum Sq"]][1]
  ss_total   <- sum(aov_sum[["Sum Sq"]])
  eta_sq     <- ss_between / ss_total

  eta_label <- if (eta_sq < 0.01) "negligible" else if
  (eta_sq < 0.06) "small" else if
  (eta_sq < 0.14) "moderate" else "large"

  sig_label <- if (p_val < alpha) {
    sprintf("statistically significant (p = %.4f < alpha %.2f)", p_val, alpha)
  } else {
    sprintf("not statistically significant (p = %.4f > alpha %.2f)", p_val, alpha)
  }

  # --- Print report ---
  cat("\n")
  cat("- statease ANOVA Report -\n")
  cat(sprintf("  Outcome      : %s\n", outcome))
  cat(sprintf("  Group        : %s  (%d levels)\n", group_var, n_groups))
  cat("-\n")
  cat("  Group Means:\n")
  for (g in groups) {
    cat(sprintf("    %-12s : Mean = %.2f  (n = %d)\n",
                g, group_means[g], group_ns[g]))
  }
  cat("-\n")
  cat(sprintf("  F-statistic  : %.3f\n", f_val))
  cat(sprintf("  df           : %d, %d\n", df1, df2))
  cat(sprintf("  p-value      : %.4f\n", p_val))
  cat(sprintf("  Eta squared  : %.4f (%s effect)\n", eta_sq, eta_label))
  cat("-\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The overall ANOVA result is %s.\n", sig_label))
  cat(sprintf("  Group differences explain %.1f%% of the variance\n", eta_sq * 100))
  cat(sprintf("  in %s (eta^2 = %.4f, %s effect).\n", outcome, eta_sq, eta_label))

  # --- Assumption warnings ---
  if (length(normality_notes) > 0) {
    cat("\n")
    for (note in normality_notes) cat(sprintf("  %s\n", note))
  }
  if (!is.null(variance_note)) {
    cat(sprintf("  %s\n", variance_note))
  }

  # --- Post-hoc Tukey ---
  if (p_val < alpha) {
    cat("\n")
    cat("- Post-Hoc Tukey HSD -\n")
    tukey    <- TukeyHSD(aov_model, conf.level = conf.level)
    tukey_df <- as.data.frame(tukey[[group_var]])
    tukey_df$comparison <- rownames(tukey_df)

    for (i in 1:nrow(tukey_df)) {
      comp    <- tukey_df$comparison[i]
      diff    <- tukey_df$diff[i]
      p_tukey <- tukey_df$`p adj`[i]
      sig     <- if (p_tukey < alpha) "Significant" else "Not significant"
      cat(sprintf("  %s\n", comp))
      cat(sprintf("    Mean diff = %.3f  |  p adj = %.4f  |  %s\n",
                  diff, p_tukey, sig))
    }
    cat("-\n")
    cat("  Note: Tukey HSD controls for family-wise error rate.\n")
  } else {
    cat("\n  Post-hoc tests not run (overall result not significant).\n")
  }

  cat("-\n\n")
}
