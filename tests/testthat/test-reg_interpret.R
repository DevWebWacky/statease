test_that("reg_interpret returns correct class", {
  df <- data.frame(
    y = c(1,2,3,4,5,6,7,8,9,10),
    x = c(2,3,4,5,6,7,8,9,10,11)
  )
  result <- reg_interpret(y ~ x, data = df)
  expect_s3_class(result, "statease_reg")
})

test_that("reg_interpret returns correct values", {
  df <- data.frame(
    y = c(1,2,3,4,5,6,7,8,9,10),
    x = c(1,2,3,4,5,6,7,8,9,10)
  )
  result <- reg_interpret(y ~ x, data = df)
  expect_true(is.numeric(result$r_squared))
  expect_true(result$r_squared >= 0 && result$r_squared <= 1)
  expect_true(is.numeric(result$slope))
  expect_true(is.numeric(result$intercept))
})

test_that("reg_interpret perfect fit", {
  df <- data.frame(
    y = c(1,2,3,4,5,6,7,8,9,10),
    x = c(1,2,3,4,5,6,7,8,9,10)
  )
  result <- reg_interpret(y ~ x, data = df)
  expect_equal(round(result$r_squared, 4), 1)
})

test_that("reg_interpret errors on missing variable", {
  df <- data.frame(
    y = c(1,2,3,4,5),
    x = c(1,2,3,4,5)
  )
  expect_error(reg_interpret(y ~ z, data = df))
})

test_that("reg_interpret errors on non-numeric outcome", {
  df <- data.frame(
    y = c("a","b","c","d","e"),
    x = c(1,2,3,4,5)
  )
  expect_error(reg_interpret(y ~ x, data = df))
})
