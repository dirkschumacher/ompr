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

test_that("extract_coefficients can extract constant", {
  ast <- substitute(1 + x * 10 + x[1, 2] + 23 + -1 * 3)
  result <- extract_coefficients(ast)
  expect_equivalent(21, result$constant)
})

test_that("extract_coefficients can extract coefficients", {
  ast <- substitute(1 + x * (5 + 5) + x[1, 2] + 23 + -1 * 3)
  result <- extract_coefficients(ast)
  expect_equal(2, length(result$coefficients))
  expect_equivalent(10, result$coefficients[["x"]]$coef)
  expect_equivalent(1, result$coefficients[["x[1, 2]"]]$coef)
})
