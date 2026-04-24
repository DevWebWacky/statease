#' Master Analysis Function - Auto-detects and runs the right test
#'
#' @param x A numeric vector (required always)
#' @param y A numeric vector or factor/character group variable (optional)
#' @param data A data frame (required if using a formula)
#' @param formula A formula of the form outcome ~ group (optional)
#' @param mu Hypothesised mean for one-sample t-test. Default 0.
#' @param paired Logical. TRUE for paired t-test. Default FALSE.
#' @param conf.level Confidence level. Default 0.95.
#' @param var_name Optional label for the report.
#'
#' @return A printed analysis report
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
analyze <- function(x = NULL, y = NULL, data = NULL, formula = NULL,
                    mu = 0, paired = FALSE, conf.level = 0.95,
                    var_name = "Variable") {

  # - ROUTE 1: Formula provided - ANOVA
  if (!is.null(formula)) {
    if (is.null(data)) stop("Please provide a data frame using the 'data' argument.")

    vars      <- all.vars(formula)
    group_var <- vars[2]
    n_groups  <- length(unique(data[[group_var]]))

    if (n_groups < 2) stop("Group variable must have at least 2 levels.")

    if (n_groups == 2) {
      # Two groups via formula - independent t-test
      cat("  [statease] Two groups detected via formula - Running Independent T-Test\n")
      grp_levels <- unique(data[[group_var]])
      x_grp <- data[[vars[1]]][data[[group_var]] == grp_levels[1]]
      y_grp <- data[[vars[1]]][data[[group_var]] == grp_levels[2]]
      ttest_interpret(x_grp, y_grp, conf.level = conf.level, var_name = vars[1])

    } else {
      # Three or more groups -> ANOVA
      cat("  [statease] 3+ groups detected via formula - Running One-Way ANOVA\n")
      anova_interpret(formula, data = data, conf.level = conf.level)
    }

    # - ROUTE 2: x and y both numeric - T-Test -
  } else if (!is.null(x) && !is.null(y) && is.numeric(y)) {
    cat("  [statease] Two numeric vectors detected - Running T-Test\n")
    ttest_interpret(x, y, mu = mu, paired = paired,
                    conf.level = conf.level, var_name = var_name)

    # - ROUTE 3: x numeric, y is a group factor - Auto-route -
  } else if (!is.null(x) && !is.null(y) && (is.factor(y) || is.character(y))) {
    n_groups <- length(unique(y))

    if (n_groups == 2) {
      cat("  [statease] Numeric + 2-level group detected -> Running T-Test\n")
      grp_levels <- unique(y)
      x1 <- x[y == grp_levels[1]]
      x2 <- x[y == grp_levels[2]]
      ttest_interpret(x1, x2, conf.level = conf.level, var_name = var_name)

    } else if (n_groups > 2) {
      cat("  [statease] Numeric + 3+ level group detected -> Running ANOVA\n")
      temp_df   <- data.frame(outcome = x, group = as.factor(y))
      anova_interpret(outcome ~ group, data = temp_df, conf.level = conf.level)

    } else {
      stop("Group variable must have at least 2 levels.")
    }

    # - ROUTE 4: x only → Descriptive stats -
  } else if (!is.null(x) && is.numeric(x)) {
    if (mu != 0) {
      cat("  [statease] Single vector + mu detected -> Running One-Sample T-Test\n")
      ttest_interpret(x, mu = mu, conf.level = conf.level, var_name = var_name)
    } else {
      cat("  [statease] Single numeric vector -> Running Descriptive Statistics\n")
      describe(x, var_name = var_name)
    }

  } else {
    stop("Could not determine the right analysis. Check your inputs.")
  }
}
