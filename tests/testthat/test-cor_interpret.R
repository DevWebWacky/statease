test_that("cor_interpret returns correct class", {
  result <- cor_interpret(
    c(1,2,3,4,5,6,7,8,9,10),
    c(2,3,4,5,6,7,8,9,10,11)
  )
  expect_s3_class(result, "statease_cor")
})

test_that("cor_interpret returns correct values", {
  result <- cor_interpret(
    c(1,2,3,4,5,6,7,8,9,10),
    c(2,3,4,5,6,7,8,9,10,11)
  )
  expect_true(is.numeric(result$r))
  expect_true(result$r >= -1 && result$r <= 1)
  expect_true(is.numeric(result$p_val))
})

test_that("cor_interpret perfect correlation", {
  result <- cor_interpret(
    c(1,2,3,4,5,6,7,8,9,10),
    c(1,2,3,4,5,6,7,8,9,10)
  )
  expect_equal(as.numeric(round(result$r, 4)), 1)
})

test_that("cor_interpret errors on unequal lengths", {
  expect_error(cor_interpret(
    c(1,2,3),
    c(1,2)
  ))
})

test_that("cor_interpret errors on invalid method", {
  expect_error(cor_interpret(
    c(1,2,3,4,5),
    c(1,2,3,4,5),
    method = "invalid"
  ))
})

test_that("cor_interpret spearman method works", {
  result <- cor_interpret(
    c(1,2,3,4,5,6,7,8,9,10),
    c(2,3,4,5,6,7,8,9,10,11),
    method = "spearman"
  )
  expect_equal(result$method, "spearman")
})
