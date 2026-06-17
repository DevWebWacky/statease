test_that("anova_interpret returns correct class", {
  df <- data.frame(
    score = c(23,45,12,67,34,89,56,43,78,90,11,34),
    group = rep(c("A","B","C"), each = 4)
  )
  result <- anova_interpret(score ~ group, data = df)
  expect_s3_class(result, "statease_anova")
})

test_that("anova_interpret returns correct values", {
  df <- data.frame(
    score = c(23,45,12,67,34,89,56,43,78,90,11,34),
    group = rep(c("A","B","C"), each = 4)
  )
  result <- anova_interpret(score ~ group, data = df)
  expect_true(is.numeric(result$f_val))
  expect_true(is.numeric(result$p_val))
  expect_true(result$p_val >= 0 && result$p_val <= 1)
  expect_true(is.numeric(result$eta_sq))
})

test_that("anova_interpret errors on missing variable", {
  df <- data.frame(
    score = c(23,45,12,67,34,89,56,43,78,90,11,34),
    group = rep(c("A","B","C"), each = 4)
  )
  expect_error(anova_interpret(marks ~ group, data = df))
})

test_that("anova_interpret errors on non-numeric outcome", {
  df <- data.frame(
    score = rep(c("a","b","c","d"), times = 3),
    group = rep(c("A","B","C"), each = 4)
  )
  expect_error(anova_interpret(score ~ group, data = df))
})

test_that("anova_interpret errors on single group", {
  df <- data.frame(
    score = c(23,45,12,67,34,89),
    group = rep("A", 6)
  )
  expect_error(anova_interpret(score ~ group, data = df))
})
