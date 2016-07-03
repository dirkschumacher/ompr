context("helpers")

test_that("sum_exp returns an ast", {
  result <- sum_exp(x[i], i = 1:3, free_vars = "x")
  expect_equal(deparse(result), "x[1L] + x[2L] + x[3L]")
})

test_that("any_unbounded_indexes detects unbound vars", {
  expect_true(any_unbounded_indexes(substitute(x[2, i])))
})

test_that("sum_exp does not evaluate other variables", {
  weights <- c(1, 2, 3)
  n <- 3
  result <- sum_exp(weights[i] * x[i], i = 1:n, free_vars = "x")
  expect_equal(deparse(result), "weights[1L] * x[1L] + weights[2L] * x[2L] + weights[3L] * x[3L]")
})
