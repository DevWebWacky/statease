test_that("interpret_p returns correct class", {
  result <- interpret_p(0.03)
  expect_s3_class(result, "statease_pvalue")
})

test_that("interpret_p significant result", {
  result <- interpret_p(0.03)
  expect_true(result$significant)
})

test_that("interpret_p non-significant result", {
  result <- interpret_p(0.07)
  expect_false(result$significant)
})

test_that("interpret_p very strong evidence", {
  result <- interpret_p(0.0001)
  expect_equal(result$evidence,
               "very strong evidence against the null hypothesis")
})

test_that("interpret_p errors on invalid p-value", {
  expect_error(interpret_p(1.5))
  expect_error(interpret_p(-0.1))
})

test_that("interpret_p errors on invalid alpha", {
  expect_error(interpret_p(0.03, alpha = 1.5))
})

test_that("interpret_p context is stored", {
  result <- interpret_p(0.03, context = "test context")
  expect_equal(result$context, "test context")
})
