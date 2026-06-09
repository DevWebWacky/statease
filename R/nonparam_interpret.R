#' Mann-Whitney U Test with Plain-English Interpretation
#'
#' @param x A numeric vector (group 1)
#' @param y A numeric vector (group 2)
#' @param conf.level Confidence level. Default 0.95.
#' @param var_name Optional label for the report. Default "Variable"
#'
#' @return An object of class \code{statease_mannwhitney} containing
#'   test results and interpretation. Use \code{print()} to display
#'   the formatted report.
#' @export
#'
#' @examples
#' x <- c(23, 45, 12, 67, 34, 89, 56)
#' y <- c(19, 38, 22, 51, 29, 74, 44)
#' result <- mannwhitney_interpret(x, y)
#' print(result)
mannwhitney_interpret <- function(x, y, conf.level = 0.95,
                                  var_name = "Variable") {

  # --- Guard clauses ---
  if (!is.numeric(x)) stop("x must be a numeric vector.")
  if (!is.numeric(y)) stop("y must be a numeric vector.")
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1.")
  }
  if (length(na.omit(x)) < 2) stop("x must have at least 2 non-missing values.")
  if (length(na.omit(y)) < 2) stop("y must have at least 2 non-missing values.")

  alpha   <- 1 - conf.level
  x_clean <- na.omit(x)
  y_clean <- na.omit(y)

  if (length(x_clean) < 10 || length(y_clean) < 10) {
    warning("Sample size is small (n < 10). Interpret results with caution.")
  }

  # --- Run test ---
  result <- wilcox.test(x, y, paired = FALSE,
                        conf.int = TRUE, conf.level = conf.level)
  w_val  <- result$statistic
  p_val  <- result$p.value
  ci     <- result$conf.int

  # --- Effect size r = Z / sqrt(N) ---
  n_total <- length(x_clean) + length(y_clean)
  z_val   <- qnorm(p_val / 2)
  r_effect <- abs(z_val) / sqrt(n_total)

  effect_label <- if (r_effect < 0.1) {
    "negligible"
  } else if (r_effect < 0.3) {
    "small"
  } else if (r_effect < 0.5) {
    "moderate"
  } else {
    "large"
  }

  # --- Medians ---
  med_x <- median(x_clean)
  med_y <- median(y_clean)

  direction <- if (med_x > med_y) {
    sprintf(paste("Values in Group 1 appear stochastically greater",
                  "than values in Group 2.",
                  "(Reported medians: Group 1 = %.2f, Group 2 = %.2f)"),
            med_x, med_y)
  } else {
    sprintf(paste("Values in Group 2 appear stochastically greater",
                  "than values in Group 1.",
                  "(Reported medians: Group 1 = %.2f, Group 2 = %.2f)"),
            med_x, med_y)
  }

  sig_label <- if (p_val < alpha) {
    sprintf("statistically significant (p = %.4f < alpha %.2f)", p_val, alpha)
  } else {
    sprintf("not statistically significant (p = %.4f > alpha %.2f)", p_val, alpha)
  }

  output <- list(
    var_name     = var_name,
    n_x          = length(x_clean),
    n_y          = length(y_clean),
    med_x        = med_x,
    med_y        = med_y,
    w_val        = w_val,
    p_val        = p_val,
    ci           = ci,
    r_effect     = r_effect,
    effect_label = effect_label,
    sig_label    = sig_label,
    direction    = direction,
    conf.level   = conf.level,
    alpha        = alpha
  )

  class(output) <- "statease_mannwhitney"
  output
}

#' @export
print.statease_mannwhitney <- function(x, ...) {
  cat("\n")
  cat("-- statease Mann-Whitney U Test Report --------------------------\n")
  cat(sprintf("  Variable     : %s\n", x$var_name))
  cat(sprintf("  Group 1      : n = %d  |  Median = %.2f\n", x$n_x, x$med_x))
  cat(sprintf("  Group 2      : n = %d  |  Median = %.2f\n", x$n_y, x$med_y))
  cat("-----------------------------------------------------------------\n")
  cat(sprintf("  W statistic  : %.3f\n", x$w_val))
  cat(sprintf("  p-value      : %.4f\n", x$p_val))
  cat(sprintf("  %d%% CI      : [%.3f, %.3f]\n",
              as.integer(x$conf.level * 100), x$ci[1], x$ci[2]))
  cat(sprintf("  Effect size  : %.3f (%s)\n", x$r_effect, x$effect_label))
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The result is %s.\n", x$sig_label))
  cat(sprintf("  %s\n", x$direction))
  cat(sprintf("  Effect size is %s (r = %.3f).\n",
              x$effect_label, x$r_effect))
  cat("  Note: Mann-Whitney tests stochastic superiority,\n")
  cat("  not differences in medians.\n")
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}


#' Wilcoxon Signed Rank Test with Plain-English Interpretation
#'
#' @param x A numeric vector (first measurement)
#' @param y A numeric vector (second measurement)
#' @param conf.level Confidence level. Default 0.95.
#' @param var_name Optional label for the report. Default "Variable"
#'
#' @return An object of class \code{statease_wilcoxon} containing
#'   test results and interpretation. Use \code{print()} to display
#'   the formatted report.
#' @export
#'
#' @examples
#' x <- c(23, 45, 12, 67, 34, 89, 56)
#' y <- c(19, 38, 22, 51, 29, 74, 44)
#' result <- wilcoxon_interpret(x, y)
#' print(result)
wilcoxon_interpret <- function(x, y, conf.level = 0.95,
                               var_name = "Variable") {

  # --- Guard clauses ---
  if (!is.numeric(x)) stop("x must be a numeric vector.")
  if (!is.numeric(y)) stop("y must be a numeric vector.")
  if (length(x) != length(y)) {
    stop("x and y must have the same length for a paired test.")
  }
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1.")
  }
  if (length(na.omit(x)) < 2) stop("x must have at least 2 non-missing values.")

  alpha   <- 1 - conf.level
  x_clean <- na.omit(x)
  y_clean <- na.omit(y)

  if (length(x_clean) < 10) {
    warning("Sample size is small (n < 10). Interpret results with caution.")
  }

  # --- Run test ---
  result <- wilcox.test(x, y, paired = TRUE,
                        conf.int = TRUE, conf.level = conf.level)
  v_val  <- result$statistic
  p_val  <- result$p.value
  ci     <- result$conf.int

  # --- Effect size ---
  n_total  <- length(x_clean)
  z_val    <- qnorm(p_val / 2)
  r_effect <- abs(z_val) / sqrt(n_total)

  effect_label <- if (r_effect < 0.1) {
    "negligible"
  } else if (r_effect < 0.3) {
    "small"
  } else if (r_effect < 0.5) {
    "moderate"
  } else {
    "large"
  }

  # --- Medians ---
  med_x <- median(x_clean)
  med_y <- median(y_clean)

  direction <- if (med_x > med_y) {
    sprintf(paste("Post-measurement values appear stochastically",
                  "greater than pre-measurement values.",
                  "(Reported medians: Post = %.2f, Pre = %.2f)"),
            med_x, med_y)
  } else {
    sprintf(paste("Pre-measurement values appear stochastically",
                  "greater than post-measurement values.",
                  "(Reported medians: Post = %.2f, Pre = %.2f)"),
            med_x, med_y)
  }

  sig_label <- if (p_val < alpha) {
    sprintf("statistically significant (p = %.4f < alpha %.2f)", p_val, alpha)
  } else {
    sprintf("not statistically significant (p = %.4f > alpha %.2f)", p_val, alpha)
  }

  output <- list(
    var_name     = var_name,
    n            = length(x_clean),
    med_x        = med_x,
    med_y        = med_y,
    v_val        = v_val,
    p_val        = p_val,
    ci           = ci,
    r_effect     = r_effect,
    effect_label = effect_label,
    sig_label    = sig_label,
    direction    = direction,
    conf.level   = conf.level,
    alpha        = alpha
  )

  class(output) <- "statease_wilcoxon"
  output
}

#' @export
print.statease_wilcoxon <- function(x, ...) {
  cat("\n")
  cat("-- statease Wilcoxon Signed Rank Test Report --------------------\n")
  cat(sprintf("  Variable     : %s\n", x$var_name))
  cat(sprintf("  N (pairs)    : %d\n", x$n))
  cat(sprintf("  Pre Median   : %.2f\n", x$med_y))
  cat(sprintf("  Post Median  : %.2f\n", x$med_x))
  cat("-----------------------------------------------------------------\n")
  cat(sprintf("  V statistic  : %.3f\n", x$v_val))
  cat(sprintf("  p-value      : %.4f\n", x$p_val))
  cat(sprintf("  %d%% CI      : [%.3f, %.3f]\n",
              as.integer(x$conf.level * 100), x$ci[1], x$ci[2]))
  cat(sprintf("  Effect size  : %.3f (%s)\n", x$r_effect, x$effect_label))
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The result is %s.\n", x$sig_label))
  cat(sprintf("  %s\n", x$direction))
  cat(sprintf("  Effect size is %s (r = %.3f).\n",
              x$effect_label, x$r_effect))
  cat("  Note: Wilcoxon test compares groups using ranked values.\n")
  cat("  A significant result sugegsts one group tends to have larger or smaller observation than the other.\n")
  cat("  This can be interpreted as evidence of stochastic superiority, but only under typical distribution assumptions.")
  cat("  It does not specifically test differences in medians.\n")
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}


#' Kruskal-Wallis Test with Plain-English Interpretation
#'
#' @param formula A formula of the form outcome ~ group
#' @param data A data frame containing the variables
#' @param conf.level Confidence level. Default 0.95.
#'
#' @return An object of class \code{statease_kruskal} containing
#'   test results and interpretation. Use \code{print()} to display
#'   the formatted report.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   score = c(23,45,12,67,34,89,56,43,78,90,11,34),
#'   group = rep(c("A","B","C"), each = 4)
#' )
#' result <- kruskal_interpret(score ~ group, data = df)
#' print(result)
kruskal_interpret <- function(formula, data, conf.level = 0.95) {

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

  # --- Check variables exist ---
  if (!outcome %in% names(data)) {
    stop(sprintf("Outcome variable '%s' not found in data.", outcome))
  }
  if (!group_var %in% names(data)) {
    stop(sprintf("Group variable '%s' not found in data.", group_var))
  }
  if (!is.numeric(data[[outcome]])) {
    stop(sprintf("Outcome variable '%s' must be numeric.", outcome))
  }

  # --- Convert group to factor ---
  data[[group_var]] <- as.factor(data[[group_var]])
  groups            <- levels(data[[group_var]])
  n_groups          <- length(groups)

  if (n_groups < 2) stop("Group variable must have at least 2 levels.")

  # --- Sample size check ---
  group_ns <- tapply(data[[outcome]], data[[group_var]],
                     function(x) sum(!is.na(x)))
  if (any(group_ns < 5)) {
    warning("One or more groups have very small sample sizes (n < 5).")
  }

  # --- Group medians ---
  group_medians <- tapply(data[[outcome]], data[[group_var]],
                          median, na.rm = TRUE)

  # --- Run Kruskal-Wallis ---
  result  <- kruskal.test(formula, data = data)
  h_val   <- result$statistic
  df      <- result$parameter
  p_val   <- result$p.value
  n_total <- sum(group_ns)

  # --- Effect size eta squared ---
  eta_sq <- (h_val - n_groups + 1) / (n_total - n_groups)
  eta_sq <- max(0, eta_sq)

  effect_label <- if (eta_sq < 0.01) {
    "negligible"
  } else if (eta_sq < 0.06) {
    "small"
  } else if (eta_sq < 0.14) {
    "moderate"
  } else {
    "large"
  }

  sig_label <- if (p_val < alpha) {
    sprintf("statistically significant (p = %.4f < alpha %.2f)", p_val, alpha)
  } else {
    sprintf("not statistically significant (p = %.4f > alpha %.2f)", p_val, alpha)
  }

  # --- Post-hoc Dunn test (manual) if significant ---
  dunn_results <- NULL
  if (p_val < alpha && n_groups > 2) {
    pairs     <- combn(groups, 2, simplify = FALSE)
    dunn_list <- list()
    for (pair in pairs) {
      g1   <- pair[1]
      g2   <- pair[2]
      x1   <- data[[outcome]][data[[group_var]] == g1]
      x2   <- data[[outcome]][data[[group_var]] == g2]
      wt   <- wilcox.test(x1, x2, exact = FALSE)
      comp <- paste(g1, "vs", g2)
      dunn_list[[comp]] <- list(
        comparison = comp,
        p_val      = wt$p.value,
        sig        = if (wt$p.value < alpha) "[significant]" else "[not significant]"
      )
    }
    dunn_results <- dunn_list
  }

  output <- list(
    outcome       = outcome,
    group_var     = group_var,
    groups        = groups,
    n_groups      = n_groups,
    n_total       = n_total,
    group_ns      = group_ns,
    group_medians = group_medians,
    h_val         = h_val,
    df            = df,
    p_val         = p_val,
    eta_sq        = eta_sq,
    effect_label  = effect_label,
    sig_label     = sig_label,
    dunn_results  = dunn_results,
    alpha         = alpha,
    conf.level    = conf.level
  )

  class(output) <- "statease_kruskal"
  output
}

#' @export
print.statease_kruskal <- function(x, ...) {
  cat("\n")
  cat("-- statease Kruskal-Wallis Test Report --------------------------\n")
  cat(sprintf("  Outcome      : %s\n", x$outcome))
  cat(sprintf("  Group        : %s  (%d levels)\n", x$group_var, x$n_groups))
  cat(sprintf("  N            : %d\n", x$n_total))
  cat("-----------------------------------------------------------------\n")
  cat("  Group Medians:\n")
  for (g in x$groups) {
    cat(sprintf("    %-12s : Median = %.2f  (n = %d)\n",
                g, x$group_medians[g], x$group_ns[g]))
  }
  cat("-----------------------------------------------------------------\n")
  cat(sprintf("  H statistic  : %.3f\n", x$h_val))
  cat(sprintf("  df           : %d\n", x$df))
  cat(sprintf("  p-value      : %.4f\n", x$p_val))
  cat(sprintf("  Eta squared  : %.4f (%s effect)\n",
              x$eta_sq, x$effect_label))
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The result is %s.\n", x$sig_label))
  cat(sprintf("  The result is %s.\n", x$sig_label))
  cat(sprintf("  Effect size is %s (eta^2 = %.4f).\n",
              x$effect_label, x$eta_sq))
  cat("  Note: Kruskal-Wallis test compares multiple groups using ranked values\n")
  cat("  not differences in medians.\n")
  cat("  Medians are reported for descriptive purposes only.\n")
  if (!is.null(x$dunn_results)) {
    cat("\n")
    cat("-- Post-Hoc Pairwise Comparisons (Wilcoxon) ---------------------\n")
    for (comp in names(x$dunn_results)) {
      dr <- x$dunn_results[[comp]]
      cat(sprintf("  %s\n", dr$comparison))
      cat(sprintf("    p = %.4f  %s\n", dr$p_val, dr$sig))
    }
    cat("\n")
    cat("  Note: Pairwise Wilcoxon tests used for post-hoc comparisons.\n")
  } else if (x$p_val >= x$alpha) {
    cat("\n  Post-hoc tests not run (overall result not significant).\n")
  }

  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
