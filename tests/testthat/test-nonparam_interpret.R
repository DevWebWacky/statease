test_that("mannwhitney_interpret returns correct class", {
  result <- mannwhitney_interpret(
    c(1,2,3,4,5,6,7,8,9,10),
    c(2,3,4,5,6,7,8,9,10,11)
  )
  expect_s3_class(result, "statease_mannwhitney")
})

test_that("mannwhitney_interpret returns correct values", {
  result <- mannwhitney_interpret(
    c(1,2,3,4,5,6,7,8,9,10),
    c(2,3,4,5,6,7,8,9,10,11)
  )
  expect_true(is.numeric(result$p_val))
  expect_true(result$p_val >= 0 && result$p_val <= 1)
  expect_true(is.numeric(result$r_effect))
})

test_that("mannwhitney_interpret errors on non-numeric", {
  expect_error(mannwhitney_interpret("hello", c(1,2,3)))
})

test_that("wilcoxon_interpret returns correct class", {
  result <- wilcoxon_interpret(
    c(1,2,3,4,5,6,7,8,9,10),
    c(2,3,4,5,6,7,8,9,10,11)
  )
  expect_s3_class(result, "statease_wilcoxon")
})

test_that("wilcoxon_interpret errors on unequal lengths", {
  expect_error(wilcoxon_interpret(
    c(1,2,3,4,5),
    c(1,2,3)
  ))
})

test_that("kruskal_interpret returns correct class", {
  df <- data.frame(
    score = c(23,45,12,67,34,89,56,43,78,90,11,34),
    group = rep(c("A","B","C"), each = 4)
  )
  result <- kruskal_interpret(score ~ group, data = df)
  expect_s3_class(result, "statease_kruskal")
})

test_that("kruskal_interpret returns correct values", {
  df <- data.frame(
    score = c(23,45,12,67,34,89,56,43,78,90,11,34),
    group = rep(c("A","B","C"), each = 4)
  )
  result <- kruskal_interpret(score ~ group, data = df)
  expect_true(is.numeric(result$h_val))
  expect_true(is.numeric(result$p_val))
  expect_true(result$p_val >= 0 && result$p_val <= 1)
})
