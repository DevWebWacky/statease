#' MANOVA with Plain-English Interpretation
#'
#' @param formula A formula of the form cbind(outcome1, outcome2, ...) ~ group
#' @param data A data frame containing the variables
#' @param conf.level Confidence level. Default 0.95.
#'
#' @return An object of class \code{statease_manova} containing MANOVA
#'   results and interpretation. Use \code{print()} to display the
#'   formatted report.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   math    = c(23,45,12,67,34,89,56,43,78,90,11,34),
#'   english = c(34,56,23,78,45,90,67,54,89,95,22,45),
#'   group   = rep(c("A","B","C"), each = 4)
#' )
#' result <- manova_interpret(cbind(math, english) ~ group, data = df)
#' print(result)
manova_interpret <- function(formula, data, conf.level = 0.95) {

  # --- Guard clauses ---
  if (!inherits(formula, "formula")) {
    stop("Please provide a valid formula e.g. cbind(y1, y2) ~ group")
  }
  if (!is.data.frame(data)) stop("data must be a data frame.")
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1.")
  }

  alpha <- 1 - conf.level

  # --- Extract variable names ---
  vars      <- all.vars(formula)
  group_var <- vars[length(vars)]
  outcomes  <- vars[-length(vars)]

  # --- Check variables exist ---
  for (v in vars) {
    if (!v %in% names(data)) {
      stop(sprintf("Variable '%s' not found in data.", v))
    }
  }

  # --- Check outcomes are numeric ---
  for (o in outcomes) {
    if (!is.numeric(data[[o]])) {
      stop(sprintf("Outcome variable '%s' must be numeric.", o))
    }
  }

  # --- Check at least two outcomes ---
  if (length(outcomes) < 2) {
    stop("MANOVA requires at least two outcome variables e.g. cbind(y1, y2) ~ group")
  }

  # --- Convert group to factor ---
  data[[group_var]] <- as.factor(data[[group_var]])
  groups            <- levels(data[[group_var]])
  n_groups          <- length(groups)

  if (n_groups < 2) stop("Group variable must have at least 2 levels.")

  # --- Sample size check ---
  n <- nrow(na.omit(data[, vars]))
  if (n < 20) {
    warning("Sample size is small (n < 20). Interpret results with caution.")
  }

  # --- Group means for each outcome ---
  group_means <- list()
  for (o in outcomes) {
    group_means[[o]] <- tapply(data[[o]], data[[group_var]],
                               mean, na.rm = TRUE)
  }

  # --- Run MANOVA ---
  model     <- manova(formula, data = data)
  model_sum <- summary(model, test = "Pillai")
  model_sum_wilks <- summary(model, test = "Wilks")

  # --- Extract Pillai's trace ---
  pillai    <- model_sum$stats[1, 2]
  f_val     <- model_sum$stats[1, 3]
  df1       <- model_sum$stats[1, 4]
  df2       <- model_sum$stats[1, 5]
  p_val     <- model_sum$stats[1, 6]

  # --- Extract Wilks lambda ---
  wilks     <- model_sum_wilks$stats[1, 2]

  # --- Effect size (Pillai's trace is itself an effect size) ---
  effect_label <- if (pillai < 0.1) {
    "negligible"
  } else if (pillai < 0.3) {
    "small"
  } else if (pillai < 0.5) {
    "moderate"
  } else {
    "large"
  }

  # --- Significance ---
  sig_label <- if (p_val < alpha) {
    sprintf("statistically significant (p = %.4f < alpha %.2f)", p_val, alpha)
  } else {
    sprintf("not statistically significant (p = %.4f > alpha %.2f)", p_val, alpha)
  }

  # --- Follow-up univariate ANOVAs ---
  univariate  <- summary.aov(model)
  uni_results <- list()
  for (i in seq_along(outcomes)) {
    o       <- outcomes[i]
    uni_sum <- tryCatch({
      as.data.frame(univariate[[i]])
    }, error = function(e) NULL)

    if (!is.null(uni_sum) && nrow(uni_sum) >= 2) {
      f_uni   <- uni_sum[1, "F value"]
      p_uni   <- uni_sum[1, "Pr(>F)"]
      df1_uni <- uni_sum[1, "Df"]
      df2_uni <- uni_sum[2, "Df"]
      sig_uni <- if (!is.na(p_uni) && p_uni < alpha) {
        "significant"
      } else {
        "not significant"
      }
    } else {
      f_uni   <- NA
      p_uni   <- NA
      df1_uni <- NA
      df2_uni <- NA
      sig_uni <- "could not be computed"
    }

    uni_results[[o]] <- list(
      outcome = o,
      f_val   = f_uni,
      df1     = df1_uni,
      df2     = df2_uni,
      p_val   = p_uni,
      sig     = sig_uni
    )
  }

  # --- Normality check ---
  normality_notes <- c()
  for (o in outcomes) {
    vals <- na.omit(data[[o]])
    if (length(vals) >= 3 && length(vals) <= 5000) {
      sw <- shapiro.test(vals)
      if (sw$p.value < 0.05) {
        normality_notes <- c(normality_notes,
                             sprintf("WARNING: '%s' may not be normally distributed (Shapiro-Wilk p = %.4f).",
                                     o, sw$p.value))
      }
    }
  }

  output <- list(
    outcomes        = outcomes,
    group_var       = group_var,
    groups          = groups,
    n_groups        = n_groups,
    n               = n,
    group_means     = group_means,
    pillai          = pillai,
    wilks           = wilks,
    f_val           = f_val,
    df1             = df1,
    df2             = df2,
    p_val           = p_val,
    effect_label    = effect_label,
    sig_label       = sig_label,
    uni_results     = uni_results,
    normality_notes = normality_notes,
    alpha           = alpha,
    conf.level      = conf.level
  )

  class(output) <- "statease_manova"
  output
}

#' @export
print.statease_manova <- function(x, ...) {
  cat("\n")
  cat("-- statease MANOVA Report ----------------------------------------\n")
  cat(sprintf("  Outcomes     : %s\n", paste(x$outcomes, collapse = ", ")))
  cat(sprintf("  Group        : %s  (%d levels)\n", x$group_var, x$n_groups))
  cat(sprintf("  N            : %d\n", x$n))
  cat("-----------------------------------------------------------------\n")
  cat("  Group Means:\n")
  for (o in x$outcomes) {
    cat(sprintf("\n  %s:\n", o))
    for (g in x$groups) {
      cat(sprintf("    %-12s : %.2f\n", g, x$group_means[[o]][g]))
    }
  }
  cat("-----------------------------------------------------------------\n")
  cat("  Multivariate Test Results:\n")
  cat(sprintf("  Pillai's Trace : %.4f\n", x$pillai))
  cat(sprintf("  Wilks' Lambda  : %.4f\n", x$wilks))
  cat(sprintf("  F-statistic    : %.3f  (df = %.0f, %.0f)\n",
              x$f_val, x$df1, x$df2))
  cat(sprintf("  p-value        : %.4f\n", x$p_val))
  cat(sprintf("  Effect size    : %s (Pillai = %.4f)\n",
              x$effect_label, x$pillai))
  cat("-----------------------------------------------------------------\n")
  cat("  Interpretation:\n")
  cat(sprintf("  The overall MANOVA result is %s.\n", x$sig_label))
  cat(sprintf("  Pillai's Trace = %.4f indicates a %s effect.\n",
              x$pillai, x$effect_label))
  cat("-----------------------------------------------------------------\n")
  cat("  Follow-Up Univariate ANOVAs:\n")
  for (o in x$outcomes) {
    ur <- x$uni_results[[o]]
    cat(sprintf("\n  %s\n", o))
    cat(sprintf("    F = %.3f  (df = %d, %d)  p = %.4f  [%s]\n",
                ur$f_val, ur$df1, ur$df2, ur$p_val, ur$sig))
  }
  cat("\n")
  cat("  Note: Follow-up ANOVAs identify which outcomes\n")
  cat("  differ significantly across groups.\n")
  if (length(x$normality_notes) > 0) {
    cat("\n")
    for (note in x$normality_notes) cat(sprintf("  %s\n", note))
  }
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
