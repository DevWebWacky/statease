test_that("chisq_interpret returns correct class", {
  x <- c("Yes","No","Yes","Yes","No","Yes","No","No","Yes","Yes")
  y <- c("Male","Female","Male","Female","Male",
         "Female","Male","Female","Male","Female")
  result <- chisq_interpret(x, y)
  expect_s3_class(result, "statease_chisq")
})

test_that("chisq_interpret returns correct values", {
  x <- c("Yes","No","Yes","Yes","No","Yes","No","No","Yes","Yes")
  y <- c("Male","Female","Male","Female","Male",
         "Female","Male","Female","Male","Female")
  result <- chisq_interpret(x, y)
  expect_true(is.numeric(result$chi_val))
  expect_true(is.numeric(result$p_val))
  expect_true(result$p_val >= 0 && result$p_val <= 1)
  expect_true(is.numeric(result$cramers_v))
})

test_that("chisq_interpret errors on unequal lengths", {
  expect_error(chisq_interpret(
    c("Yes","No","Yes"),
    c("Male","Female")
  ))
})

test_that("chisq_interpret errors on invalid conf.level", {
  x <- c("Yes","No","Yes","Yes","No")
  y <- c("Male","Female","Male","Female","Male")
  expect_error(chisq_interpret(x, y, conf.level = 2))
})
