#' Two-Way ANOVA with Plain-English Interpretation
#'
#' Uses Type-2 SS by default (safe for unbalanced designs). Automatically
#' switches to Type-3 SS when an interaction term is detected and sets
#' the correct contrasts. Users are warned when interpreting main effects
#' in the presence of a significant interaction.
#'
#' @param formula A formula of the form outcome ~ group1 * group2
#' @param data A data frame containing the variables
#' @param type ANOVA type: 2 or 3. Default is 2. Type 3 is automatically
#'   used when an interaction term is detected in the formula.
#' @param conf.level Confidence level. Default 0.95.
#'
#' @return An object of class \code{statease_anova2} containing two-way
#'   ANOVA results and interpretation. Use \code{print()} to display
#'   the formatted report.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   score  = c(23,45,12,67,34,89,56,43,78,90,11,34),
#'   method = rep(c("Online","Traditional"), each = 6),
#'   gender = rep(c("Male","Female"), times = 6)
#' )
#' result <- anova2_interpret(score ~ method * gender, data = df)
#' print(result)
anova2_interpret <- function(formula, data, type = 2, conf.level = 0.95) {

  # --- Guard clauses ---
  if (!inherits(formula, "formula")) {
    stop("Please provide a valid formula e.g. outcome ~ group1 * group2")
  }
  if (!is.data.frame(data)) stop("data must be a data frame.")
  # Standardize type
  type <- as.character(type)
  type <- toupper(type)
  type <- if (type %in% c("II", "2")) "II" else if
  (type %in% c("III", "3")) "III" else
    stop("type must be 'II' or 'III' (or numeric 2 or 3).")
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1.")
  }

  alpha        <- 1 - conf.level
  formula_str  <- deparse(formula)
  vars         <- all.vars(formula)
  has_interaction <- grepl("\\*", formula_str)

  outcome   <- vars[1]
  group1    <- vars[2]
  group2    <- vars[3]

  # --- Auto switch to Type-3 if interaction detected ---
  if (has_interaction && type == 2) {
    type <- 3
    message("[statease] Interaction term detected -> Automatically using Type-3 SS.")
  }

  # --- Check variables exist ---
  if (!outcome %in% names(data)) {
    stop(sprintf("Outcome variable '%s' not found in data.", outcome))
  }
  if (!group1 %in% names(data)) {
    stop(sprintf("Group variable '%s' not found in data.", group1))
  }
  if (!group2 %in% names(data)) {
    stop(sprintf("Group variable '%s' not found in data.", group2))
  }

  # --- Check outcome is numeric ---
  if (!is.numeric(data[[outcome]])) {
    stop(sprintf("Outcome variable '%s' must be numeric.", outcome))
  }

  # --- Convert groups to factors ---
  data[[group1]] <- as.factor(data[[group1]])
  data[[group2]] <- as.factor(data[[group2]])

  # --- Sample size check ---
  n <- nrow(na.omit(data[, c(outcome, group1, group2)]))
  if (n < 4) stop("At least 4 complete observations are required.")
  if (n < 20) {
    warning("Sample size is small (n < 20). Interpret results with caution.")
  }

  # --- Group means ---
  means_g1 <- tapply(data[[outcome]], data[[group1]], mean, na.rm = TRUE)
  means_g2 <- tapply(data[[outcome]], data[[group2]], mean, na.rm = TRUE)
  means_interaction <- tapply(
    data[[outcome]],
    list(data[[group1]], data[[group2]]),
    mean, na.rm = TRUE
  )

  # --- Run linear model ---
  # For Type III, set contrasts to contr.sum for proper interpretation
  if (type == "III") {
    contrast_list <- list(
      contr.sum,
      contr.sum
    )
    names(contrast_list) <- c(group1, group2)
    model <- lm(formula, data = data, contrasts = contrast_list)
  } else {
    model <- lm(formula, data = data)
  }

  # --- Run car::Anova ---
  aov_result <- car::Anova(model, type = type)
  aov_df     <- as.data.frame(aov_result)

  # --- Extract main effects and interaction ---
  f_g1   <- aov_df[group1, "F value"]
  f_g2   <- aov_df[group2, "F value"]
  p_g1   <- aov_df[group1, "Pr(>F)"]
  p_g2   <- aov_df[group2, "Pr(>F)"]
  df_g1  <- aov_df[group1, "Df"]
  df_g2  <- aov_df[group2, "Df"]
  df_res <- aov_df["Residuals", "Df"]

  # --- Interaction (if present) ---
  int_label <- paste0(group1, ":", group2)
  has_int_row <- int_label %in% rownames(aov_df)

  f_int  <- if (has_int_row) aov_df[int_label, "F value"] else NA
  p_int  <- if (has_int_row) aov_df[int_label, "Pr(>F)"] else NA
  df_int <- if (has_int_row) aov_df[int_label, "Df"] else NA

  # --- Eta squared ---
  ss_g1  <- aov_df[group1, "Sum Sq"]
  ss_g2  <- aov_df[group2, "Sum Sq"]
  ss_int <- if (has_int_row) aov_df[int_label, "Sum Sq"] else 0
  ss_res <- aov_df["Residuals", "Sum Sq"]
  ss_tot <- ss_g1 + ss_g2 + ss_int + ss_res

  eta_g1  <- ss_g1 / ss_tot
  eta_g2  <- ss_g2 / ss_tot
  eta_int <- if (has_int_row) ss_int / ss_tot else NA

  # --- Effect size labels ---
  eta_label <- function(e) {
    if (is.na(e)) return("NA")
    if (e < 0.01) "negligible" else if
    (e < 0.06) "small" else if
    (e < 0.14) "moderate" else "large"
  }

  # --- Significance labels ---
  sig <- function(p) {
    if (is.na(p)) return("NA")
    if (p < alpha) {
      sprintf("significant (p = %.4f)", p)
    } else {
      sprintf("not significant (p = %.4f)", p)
    }
  }

  # --- Interaction warning ---
  interaction_warning <- NULL
  if (has_int_row && !is.na(p_int) && p_int < alpha) {
    interaction_warning <- paste(
      "WARNING: A significant interaction was detected.",
      "Interpret main effects with caution - the effect of",
      group1, "depends on the level of", group2,
      "(and vice versa). Consider examining interaction plots",
      "before drawing conclusions about main effects."
    )
  }

  # --- Normality check ---
  normality_note <- NULL
  res <- residuals(model)
  if (length(res) >= 3 && length(res) <= 5000) {
    sw <- shapiro.test(res)
    if (sw$p.value < 0.05) {
      normality_note <- sprintf(
        "WARNING: Residuals may not be normally distributed (Shapiro-Wilk p = %.4f).",
        sw$p.value)
    }
  }

  # --- Homogeneity of variance ---
  variance_note <- NULL
  bart <- tryCatch(
    bartlett.test(data[[outcome]] ~
                    interaction(data[[group1]], data[[group2]])),
    error = function(e) NULL
  )
  if (!is.null(bart) && bart$p.value < 0.05) {
    variance_note <- sprintf(
      "WARNING: Bartlett's test suggests unequal variances (p = %.4f).",
      bart$p.value)
  }

  # --- Post-hoc Tukey ---
  aov_model <- aov(formula, data = data)
  tukey_g1  <- NULL
  tukey_g2  <- NULL

  if (!is.na(p_g1) && p_g1 < alpha &&
      length(levels(data[[group1]])) > 2) {
    tukey    <- TukeyHSD(aov_model, conf.level = conf.level)
    tukey_g1 <- as.data.frame(tukey[[group1]])
    tukey_g1$comparison <- rownames(tukey_g1)
  }
  if (!is.na(p_g2) && p_g2 < alpha &&
      length(levels(data[[group2]])) > 2) {
    tukey    <- TukeyHSD(aov_model, conf.level = conf.level)
    tukey_g2 <- as.data.frame(tukey[[group2]])
    tukey_g2$comparison <- rownames(tukey_g2)
  }

  output <- list(
    outcome              = outcome,
    group1               = group1,
    group2               = group2,
    n                    = n,
    type                 = type,
    has_interaction      = has_interaction,
    means_g1             = means_g1,
    means_g2             = means_g2,
    means_interaction    = means_interaction,
    f_g1                 = f_g1,
    f_g2                 = f_g2,
    f_int                = f_int,
    p_g1                 = p_g1,
    p_g2                 = p_g2,
    p_int                = p_int,
    df_g1                = df_g1,
    df_g2                = df_g2,
    df_int               = df_int,
    df_res               = df_res,
    eta_g1               = eta_g1,
    eta_g2               = eta_g2,
    eta_int              = eta_int,
    eta_label            = eta_label,
    sig                  = sig,
    tukey_g1             = tukey_g1,
    tukey_g2             = tukey_g2,
    interaction_warning  = interaction_warning,
    normality_note       = normality_note,
    variance_note        = variance_note,
    alpha                = alpha,
    conf.level           = conf.level
  )

  class(output) <- "statease_anova2"
  output
}

#' @export
print.statease_anova2 <- function(x, ...) {
  cat("\n")
  cat(" statease Two-Way ANOVA Report \n")
  cat(sprintf("  Outcome      : %s\n", x$outcome))
  cat(sprintf("  Factor 1     : %s\n", x$group1))
  cat(sprintf("  Factor 2     : %s\n", x$group2))
  cat(sprintf("  N            : %d\n", x$n))
  cat(sprintf("  SS Type      : Type-%s\n", x$ss_type))
  cat("\n")
  cat(sprintf("  Means by %s:\n", x$group1))
  for (g in names(x$means_g1)) {
    cat(sprintf("    %-15s : %.2f\n", g, x$means_g1[g]))
  }
  cat(sprintf("\n  Means by %s:\n", x$group2))
  for (g in names(x$means_g2)) {
    cat(sprintf("    %-15s : %.2f\n", g, x$means_g2[g]))
  }
  cat("\n  Interaction Means:\n")
  print(round(x$means_interaction, 2))
  cat("\n")
  cat("  ANOVA Results:\n")
  cat(sprintf("  %-20s : F = %.3f  df = %.0f,%.0f  %s  eta^2 = %.4f (%s)\n",
              x$group1, x$f_g1, x$df_g1, x$df_res,
              x$sig(x$p_g1), x$eta_g1, x$eta_label(x$eta_g1)))
  cat(sprintf("  %-20s : F = %.3f  df = %.0f,%.0f  %s  eta^2 = %.4f (%s)\n",
              x$group2, x$f_g2, x$df_g2, x$df_res,
              x$sig(x$p_g2), x$eta_g2, x$eta_label(x$eta_g2)))
  if (x$has_interaction && !is.na(x$f_int)) {
    cat(sprintf("  %-20s : F = %.3f  df = %.0f,%.0f  %s  eta^2 = %.4f (%s)\n",
                "Interaction", x$f_int, x$df_int, x$df_res,
                x$sig(x$p_int), x$eta_int, x$eta_label(x$eta_int)))
  }
  cat("\n")
  cat("  Interpretation:\n")
  cat(sprintf("  Main effect of %s is %s.\n", x$group1, x$sig(x$p_g1)))
  cat(sprintf("  Main effect of %s is %s.\n", x$group2, x$sig(x$p_g2)))
  if (x$has_interaction && !is.na(x$p_int)) {
    cat(sprintf("  Interaction (%s x %s) is %s.\n",
                x$group1, x$group2, x$sig(x$p_int)))
  }

  # --- Interaction warning ---
  if (!is.null(x$interaction_warning)) {
    cat("\n")
    cat(sprintf("  %s\n", x$interaction_warning))
  }

  # --- Post-hoc ---
  if (!is.null(x$tukey_g1)) {
    cat("\n")
    cat(sprintf("-- Post-Hoc Tukey HSD (%s) ------------------------------\n",
                x$group1))
    for (i in seq_len(nrow(x$tukey_g1))) {
      sig <- if (x$tukey_g1$`p adj`[i] < x$alpha) {
        "[significant]"
      } else {
        "[not significant]"
      }
      cat(sprintf("  %s : diff = %.3f  p adj = %.4f  %s\n",
                  x$tukey_g1$comparison[i],
                  x$tukey_g1$diff[i],
                  x$tukey_g1$`p adj`[i], sig))
    }
  }
  if (!is.null(x$tukey_g2)) {
    cat("\n")
    cat(sprintf("-- Post-Hoc Tukey HSD (%s) ------------------------------\n",
                x$group2))
    for (i in seq_len(nrow(x$tukey_g2))) {
      sig <- if (x$tukey_g2$`p adj`[i] < x$alpha) {
        "[significant]"
      } else {
        "[not significant]"
      }
      cat(sprintf("  %s : diff = %.3f  p adj = %.4f  %s\n",
                  x$tukey_g2$comparison[i],
                  x$tukey_g2$diff[i],
                  x$tukey_g2$`p adj`[i], sig))
    }
  }

  if (!is.null(x$normality_note)) cat(sprintf("\n  %s\n", x$normality_note))
  if (!is.null(x$variance_note)) cat(sprintf("  %s\n", x$variance_note))
  cat("-----------------------------------------------------------------\n\n")
  invisible(x)
}
