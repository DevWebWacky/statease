test_that("describe returns correct class", {
  result <- describe(c(1, 2, 3, 4, 5))
  expect_s3_class(result, "statease_describe")
})

test_that("describe returns correct values", {
  result <- describe(c(1, 2, 3, 4, 5))
  expect_equal(result$mean, 3)
  expect_equal(result$n, 5)
  expect_equal(result$missing, 0)
  expect_equal(result$min, 1)
  expect_equal(result$max, 5)
})

test_that("describe handles missing values", {
  result <- describe(c(1, 2, NA, 4, 5))
  expect_equal(result$missing, 1)
  expect_equal(result$n, 5)
})

test_that("describe errors on non-numeric input", {
  expect_error(describe("hello"))
})

test_that("describe errors on single value", {
  expect_error(describe(1))
})

test_that("describe errors on all missing values", {
  expect_error(describe(c(NA, NA, NA)))
})

test_that("describe var_name is stored correctly", {
  result <- describe(c(1, 2, 3, 4, 5),
                     var_name = "Test Variable")
  expect_equal(result$var_name, "Test Variable")
})
