test_that("ttest_interpret returns correct class", {
  result <- ttest_interpret(
    c(1,2,3,4,5,6,7,8,9,10),
    c(2,3,4,5,6,7,8,9,10,11)
  )
  expect_s3_class(result, "statease_ttest")
})

test_that("ttest_interpret independent samples works", {
  result <- ttest_interpret(
    c(1,2,3,4,5,6,7,8,9,10),
    c(2,3,4,5,6,7,8,9,10,11)
  )
  expect_equal(result$test_type, "Independent Samples T-Test")
  expect_true(is.numeric(result$p_val))
  expect_true(result$p_val >= 0 && result$p_val <= 1)
})

test_that("ttest_interpret one sample works", {
  result <- ttest_interpret(
    c(1,2,3,4,5,6,7,8,9,10),
    mu = 3
  )
  expect_equal(result$test_type, "One-Sample T-Test")
})

test_that("ttest_interpret paired works", {
  result <- ttest_interpret(
    c(23,45,12,67,34,89,56,43,78,90),
    c(19,38,22,51,29,74,44,38,65,80),
    paired = TRUE
  )
  expect_equal(result$test_type, "Paired Samples T-Test")
})

test_that("ttest_interpret errors on non-numeric x", {
  expect_error(ttest_interpret("hello"))
})

test_that("ttest_interpret errors on invalid conf.level", {
  expect_error(ttest_interpret(
    c(1,2,3,4,5), conf.level = 1.5
  ))
})

test_that("ttest_interpret Cohen's d is numeric", {
  result <- ttest_interpret(
    c(1,2,3,4,5,6,7,8,9,10),
    c(2,3,4,5,6,7,8,9,10,11)
  )
  expect_true(is.numeric(result$cohens_d))
})
