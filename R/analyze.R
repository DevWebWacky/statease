#' Master Analysis Function - Auto-detects and runs the right test
#'
#' @param x A numeric vector (required always)
#' @param y A numeric vector, factor, or character group variable (optional)
#' @param data A data frame (required if using a formula)
#' @param formula A formula of the form outcome ~ predictor or
#'   outcome ~ group1 * group2 or cbind(y1, y2) ~ group (optional)
#' @param mu Hypothesised mean for one-sample t-test. Default 0.
#' @param paired Logical. TRUE for paired t-test. Default FALSE.
#' @param nonparam Logical. TRUE to use non-parametric tests. Default FALSE.
#' @param conf.level Confidence level. Default 0.95.
#' @param var_name Optional label for the report.
#' @param var1_name Optional name for first variable in correlation.
#' @param var2_name Optional name for second variable in correlation.
#' @param method Correlation method: "pearson", "spearman", or "kendall".
#'   Default "pearson".
#'
#' @return A printed analysis report from the appropriate test
#' @export
#'
#' @examples
#' # Descriptive only
#' analyze(x = c(23, 45, 12, 67, 34))
#'
#' # Auto t-test
#' analyze(x = c(23,45,12,67,34), y = c(19,38,22,51,29))
#'
#' # Auto Mann-Whitney (non-parametric)
#' analyze(x = c(23,45,12,67,34), y = c(19,38,22,51,29),
#'         nonparam = TRUE)
#'
#' # Auto correlation
#' analyze(x = c(23,45,12,67,34), y = c(19,38,22,51,29),
#'         var1_name = "Scores", var2_name = "Hours")
#'
#' # Auto One-Way ANOVA
#' df <- data.frame(
#'   score = c(23,45,12,67,34,89,56,43,78,90,11,34),
#'   group = rep(c("A","B","C"), each = 4)
#' )
#' analyze(formula = score ~ group, data = df)
#'
#' # Auto Kruskal-Wallis (non-parametric)
#' analyze(formula = score ~ group, data = df, nonparam = TRUE)
#'
#' # Auto Two-Way ANOVA
#' df2 <- data.frame(
#'   score  = c(23,45,12,67,34,89,56,43,78,90,11,34),
#'   method = rep(c("Online","Traditional"), each = 6),
#'   gender = rep(c("Male","Female"), times = 6)
#' )
#' analyze(formula = score ~ method * gender, data = df2)
#'
#' # Auto Regression
#' df3 <- data.frame(
#'   exam_score  = c(23,45,12,67,34,89,56,43,78,90),
#'   study_hours = c(2,5,1,7,3,9,6,4,8,10)
#' )
#' analyze(formula = exam_score ~ study_hours, data = df3)
#'
#' # Auto Multiple Regression
#' df4 <- data.frame(
#'   exam_score  = c(23,45,12,67,34,89,56,43,78,90),
#'   study_hours = c(2,5,1,7,3,9,6,4,8,10),
#'   attendance  = c(60,80,50,90,70,95,85,75,88,92)
#' )
#' analyze(formula = exam_score ~ study_hours + attendance, data = df4)
#'
#' # Auto MANOVA
#' df5 <- data.frame(
#'   math    = c(23,45,12,67,34,89,56,43,78,90,11,34),
#'   english = c(34,56,23,78,45,90,67,54,89,95,22,45),
#'   group   = rep(c("A","B","C"), each = 4)
#' )
#' analyze(formula = cbind(math, english) ~ group, data = df5)
#'
#' # Chi-square
#' analyze(
#'   x = c("Yes","No","Yes","Yes","No"),
#'   y = c("Male","Female","Male","Female","Male")
#' )
analyze <- function(x = NULL, y = NULL, data = NULL, formula = NULL,
                    mu = 0, paired = FALSE, nonparam = FALSE,
                    conf.level = 0.95, var_name = "Variable",
                    var1_name = "Variable 1", var2_name = "Variable 2",
                    method = "pearson") {

  # ── ROUTE 1: Formula provided ───────────────────────────────
  if (!is.null(formula)) {
    if (is.null(data)) stop("Please provide a data frame using the 'data' argument.")

    formula_str <- deparse(formula)
    vars        <- all.vars(formula)

    # MANOVA — cbind in formula
    if (grepl("cbind", formula_str)) {
      message("[statease] Multiple outcomes detected -> Running MANOVA")
      result <- manova_interpret(formula, data = data,
                                 conf.level = conf.level)
      print(result)

      # Two-Way ANOVA — two grouping variables
    } else if (length(vars) == 3) {
      group1 <- vars[2]
      group2 <- vars[3]

      if (!is.numeric(data[[group1]]) && !is.numeric(data[[group2]])) {
        message("[statease] Two grouping variables detected -> Running Two-Way ANOVA")
        result <- anova2_interpret(formula, data = data,
                                   conf.level = conf.level)
        print(result)

      } else {
        message("[statease] Multiple numeric predictors -> Running Multiple Linear Regression")
        result <- mlr_interpret(formula, data = data,
                                conf.level = conf.level)
        print(result)
      }

      # One variable in formula
    } else if (length(vars) == 2) {
      group_var <- vars[2]

      # Regression — predictor is numeric
      if (is.numeric(data[[group_var]])) {
        message("[statease] Numeric predictor detected -> Running Simple Linear Regression")
        result <- reg_interpret(formula, data = data,
                                conf.level = conf.level)
        print(result)

        # Categorical predictor
      } else {
        n_groups <- length(unique(data[[group_var]]))

        if (n_groups == 2) {
          if (nonparam) {
            message("[statease] Two groups + nonparam = TRUE -> Running Mann-Whitney U Test")
            grp_levels <- unique(data[[group_var]])
            x_grp <- data[[vars[1]]][data[[group_var]] == grp_levels[1]]
            y_grp <- data[[vars[1]]][data[[group_var]] == grp_levels[2]]
            result <- mannwhitney_interpret(x_grp, y_grp,
                                            conf.level = conf.level,
                                            var_name = vars[1])
            print(result)
          } else {
            message("[statease] Two groups detected -> Running Independent T-Test")
            grp_levels <- unique(data[[group_var]])
            x_grp <- data[[vars[1]]][data[[group_var]] == grp_levels[1]]
            y_grp <- data[[vars[1]]][data[[group_var]] == grp_levels[2]]
            result <- ttest_interpret(x_grp, y_grp,
                                      conf.level = conf.level,
                                      var_name = vars[1])
            print(result)
          }

        } else {
          if (nonparam) {
            message("[statease] 3+ groups + nonparam = TRUE -> Running Kruskal-Wallis Test")
            result <- kruskal_interpret(formula, data = data,
                                        conf.level = conf.level)
            print(result)
          } else {
            message("[statease] 3+ groups detected -> Running One-Way ANOVA")
            result <- anova_interpret(formula, data = data,
                                      conf.level = conf.level)
            print(result)
          }
        }
      }
    } else {
      stop("Could not determine the right analysis. Check your formula.")
    }

    # ── ROUTE 2: Two categorical vectors → Chi-square ──────────
  } else if (!is.null(x) && !is.null(y) &&
             (is.character(x) || is.factor(x)) &&
             (is.character(y) || is.factor(y))) {
    message("[statease] Two categorical vectors detected -> Running Chi-Square Test")
    result <- chisq_interpret(x, y, conf.level = conf.level)
    print(result)

    # ── ROUTE 3: Two numeric vectors ───────────────────────────
  } else if (!is.null(x) && !is.null(y) &&
             is.numeric(x) && is.numeric(y)) {

    # Correlation — var names provided
    if (var1_name != "Variable 1" || var2_name != "Variable 2") {
      message("[statease] Two numeric vectors with variable names -> Running Correlation")
      result <- cor_interpret(x, y, method = method,
                              conf.level = conf.level,
                              var1_name = var1_name,
                              var2_name = var2_name)
      print(result)

      # Paired non-parametric
    } else if (nonparam && paired) {
      message("[statease] Paired + nonparam = TRUE -> Running Wilcoxon Signed Rank Test")
      result <- wilcoxon_interpret(x, y, conf.level = conf.level,
                                   var_name = var_name)
      print(result)

      # Independent non-parametric
    } else if (nonparam) {
      message("[statease] nonparam = TRUE -> Running Mann-Whitney U Test")
      result <- mannwhitney_interpret(x, y, conf.level = conf.level,
                                      var_name = var_name)
      print(result)

      # Default → T-Test
    } else {
      message("[statease] Two numeric vectors detected -> Running T-Test")
      result <- ttest_interpret(x, y, mu = mu, paired = paired,
                                conf.level = conf.level,
                                var_name = var_name)
      print(result)
    }

    # ── ROUTE 4: x numeric + y categorical ─────────────────────
  } else if (!is.null(x) && !is.null(y) &&
             is.numeric(x) &&
             (is.factor(y) || is.character(y))) {
    n_groups <- length(unique(y))

    if (n_groups == 2) {
      if (nonparam) {
        message("[statease] Numeric + 2 groups + nonparam = TRUE -> Running Mann-Whitney U")
        grp_levels <- unique(y)
        x1 <- x[y == grp_levels[1]]
        x2 <- x[y == grp_levels[2]]
        result <- mannwhitney_interpret(x1, x2,
                                        conf.level = conf.level,
                                        var_name = var_name)
        print(result)
      } else {
        message("[statease] Numeric + 2-level group detected -> Running T-Test")
        grp_levels <- unique(y)
        x1 <- x[y == grp_levels[1]]
        x2 <- x[y == grp_levels[2]]
        result <- ttest_interpret(x1, x2, conf.level = conf.level,
                                  var_name = var_name)
        print(result)
      }

    } else if (n_groups > 2) {
      if (nonparam) {
        message("[statease] Numeric + 3+ groups + nonparam = TRUE -> Running Kruskal-Wallis")
        temp_df <- data.frame(outcome = x, group = as.factor(y))
        result  <- kruskal_interpret(outcome ~ group, data = temp_df,
                                     conf.level = conf.level)
        print(result)
      } else {
        message("[statease] Numeric + 3+ level group detected -> Running One-Way ANOVA")
        temp_df <- data.frame(outcome = x, group = as.factor(y))
        result  <- anova_interpret(outcome ~ group, data = temp_df,
                                   conf.level = conf.level)
        print(result)
      }
    } else {
      stop("Group variable must have at least 2 levels.")
    }

    # ── ROUTE 5: x only → Descriptive or One-Sample T-Test ────
  } else if (!is.null(x) && is.numeric(x)) {
    if (mu != 0) {
      message("[statease] Single vector + mu detected -> Running One-Sample T-Test")
      result <- ttest_interpret(x, mu = mu, conf.level = conf.level,
                                var_name = var_name)
      print(result)
    } else {
      message("[statease] Single numeric vector -> Running Descriptive Statistics")
      result <- describe(x, var_name = var_name)
      print(result)
    }

  } else {
    stop("Could not determine the right analysis. Check your inputs.")
  }

  invisible(result)
}
