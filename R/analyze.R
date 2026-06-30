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
#' @param test_type For power analysis: one of "ttest.one", "ttest.two",
#'   "ttest.paired", "anova", "correlation", "chisq", "regression".
#' @param effect_size For power analysis: the expected effect size.
#' @param power For power analysis: desired power level. Default 0.80.
#' @param n_groups For power analysis ANOVA: number of groups. Default 2.
#' @param n_predictors For power analysis regression: number of predictors.
#'   Default 1.
#' @param check Logical. TRUE to run assumption checks before analysis.
#'   Default FALSE.
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
#' # Auto ANOVA
#' df <- data.frame(
#'   score = c(23,45,12,67,34,89,56,43,78,90,11,34),
#'   group = rep(c("A","B","C"), each = 4)
#' )
#' analyze(formula = score ~ group, data = df)
#'
#' # Power analysis
#' analyze(test_type = "ttest.two", effect_size = 0.5)
#'
#' # Check assumptions
#' analyze(x = c(23,45,12,67,34), y = c(19,38,22,51,29),
#'         check = TRUE)
analyze <- function(x = NULL, y = NULL, data = NULL,
                    formula = NULL, mu = 0, paired = FALSE,
                    nonparam = FALSE, conf.level = 0.95,
                    var_name = "Variable",
                    var1_name = "Variable 1",
                    var2_name = "Variable 2",
                    method = "pearson",
                    test_type = NULL, effect_size = NULL,
                    power = 0.80, n_groups = 2,
                    n_predictors = 1, check = FALSE) {

  #ROUTE 0: Power Analysis
  if (!is.null(test_type) && !is.null(effect_size)) {
    message("[statease] Power analysis requested -> Running Power Analysis")
    result <- power_interpret(
      test        = test_type,
      effect_size = effect_size,
      n           = x,
      alpha       = 1 - conf.level,
      power       = power,
      n_groups    = n_groups,
      n_predictors = n_predictors
    )
    print(result)
    return(invisible(result))
  }

  #ROUTE 0b: Assumption checks
  if (check) {
    if (!is.null(formula) && !is.null(data)) {
      vars      <- all.vars(formula)
      group_var <- vars[2]
      test_name <- if (length(vars) == 3) "anova2" else {
        if (is.numeric(data[[group_var]])) "regression" else "anova"
      }
      message(sprintf("[statease] check = TRUE -> Checking assumptions for %s",
                      test_name))
      result <- check_assumptions(test_name,
                                  formula = formula,
                                  data    = data)
      print(result)
      return(invisible(result))
    } else if (!is.null(x) && !is.null(y) && is.numeric(y)) {
      message("[statease] check = TRUE -> Checking assumptions for ttest")
      result <- check_assumptions("ttest", x = x, y = y)
      print(result)
      return(invisible(result))
    } else if (!is.null(x) && !is.null(y) && is.numeric(x)) {
      message("[statease] check = TRUE -> Checking assumptions for correlation")
      result <- check_assumptions("correlation", x = x, y = y)
      print(result)
      return(invisible(result))
    }
  }

  #ROUTE 1: Formula provided
  if (!is.null(formula)) {
    if (is.null(data)) {
      stop("Please provide a data frame using the 'data' argument.")
    }

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
        n_groups_data <- length(unique(data[[group_var]]))

        if (n_groups_data == 2) {
          if (nonparam) {
            message("[statease] Two groups + nonparam = TRUE -> Running Mann-Whitney U Test")
            grp_levels <- unique(data[[group_var]])
            x_grp <- data[[vars[1]]][data[[group_var]] == grp_levels[1]]
            y_grp <- data[[vars[1]]][data[[group_var]] == grp_levels[2]]
            result <- mannwhitney_interpret(x_grp, y_grp,
                                            conf.level = conf.level,
                                            var_name   = vars[1])
            print(result)
          } else {
            message("[statease] Two groups detected -> Running Independent T-Test")
            grp_levels <- unique(data[[group_var]])
            x_grp <- data[[vars[1]]][data[[group_var]] == grp_levels[1]]
            y_grp <- data[[vars[1]]][data[[group_var]] == grp_levels[2]]
            result <- ttest_interpret(x_grp, y_grp,
                                      conf.level = conf.level,
                                      var_name   = vars[1])
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

    #ROUTE 3: Two numeric vectors
  } else if (!is.null(x) && !is.null(y) &&
             is.numeric(x) && is.numeric(y)) {

    if (var1_name != "Variable 1" || var2_name != "Variable 2") {
      message("[statease] Two numeric vectors with variable names -> Running Correlation")
      result <- cor_interpret(x, y, method = method,
                              conf.level = conf.level,
                              var1_name  = var1_name,
                              var2_name  = var2_name)
      print(result)

    } else if (nonparam && paired) {
      message("[statease] Paired + nonparam = TRUE -> Running Wilcoxon Signed Rank Test")
      result <- wilcoxon_interpret(x, y, conf.level = conf.level,
                                   var_name = var_name)
      print(result)

    } else if (nonparam) {
      message("[statease] nonparam = TRUE -> Running Mann-Whitney U Test")
      result <- mannwhitney_interpret(x, y, conf.level = conf.level,
                                      var_name = var_name)
      print(result)

    } else {
      message("[statease] Two numeric vectors detected -> Running T-Test")
      result <- ttest_interpret(x, y, mu = mu, paired = paired,
                                conf.level = conf.level,
                                var_name   = var_name)
      print(result)
    }

    # ROUTE 4: x numeric + y categorical
  } else if (!is.null(x) && !is.null(y) &&
             is.numeric(x) &&
             (is.factor(y) || is.character(y))) {
    n_groups_data <- length(unique(y))

    if (n_groups_data == 2) {
      if (nonparam) {
        message("[statease] Numeric + 2 groups + nonparam = TRUE -> Running Mann-Whitney U")
        grp_levels <- unique(y)
        x1 <- x[y == grp_levels[1]]
        x2 <- x[y == grp_levels[2]]
        result <- mannwhitney_interpret(x1, x2,
                                        conf.level = conf.level,
                                        var_name   = var_name)
        print(result)
      } else {
        message("[statease] Numeric + 2-level group detected -> Running T-Test")
        grp_levels <- unique(y)
        x1 <- x[y == grp_levels[1]]
        x2 <- x[y == grp_levels[2]]
        result <- ttest_interpret(x1, x2,
                                  conf.level = conf.level,
                                  var_name   = var_name)
        print(result)
      }

    } else if (n_groups_data > 2) {
      if (nonparam) {
        message("[statease] Numeric + 3+ groups + nonparam = TRUE -> Running Kruskal-Wallis")
        temp_df <- data.frame(outcome = x, group = as.factor(y))
        result  <- kruskal_interpret(outcome ~ group,
                                     data       = temp_df,
                                     conf.level = conf.level)
        print(result)
      } else {
        message("[statease] Numeric + 3+ level group detected -> Running One-Way ANOVA")
        temp_df <- data.frame(outcome = x, group = as.factor(y))
        result  <- anova_interpret(outcome ~ group,
                                   data       = temp_df,
                                   conf.level = conf.level)
        print(result)
      }
    } else {
      stop("Group variable must have at least 2 levels.")
    }

    # ROUTE 5: x only → Descriptive or One-Sample T-Test
  } else if (!is.null(x) && is.numeric(x)) {
    if (mu != 0) {
      message("[statease] Single vector + mu detected -> Running One-Sample T-Test")
      result <- ttest_interpret(x, mu = mu,
                                conf.level = conf.level,
                                var_name   = var_name)
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
