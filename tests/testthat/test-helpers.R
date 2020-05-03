context("helpers")

test_that("sum_expr returns an ast", {
  result <- sum_expr(x[i], i = 1:3)
  expect_equal(deparse(result), "x[1L] + x[2L] + x[3L]")
})

test_that("sum_expr does not evaluate other variables", {
  weights <- c(1, 2, 3)
  n <- 3
  result <- sum_expr(weights[i] * x[i], i = 1:n)
  expected <- "weights[1L] * x[1L] + weights[2L] * x[2L] + weights[3L] * x[3L]"
  expect_equal(deparse(result), expected)
})

test_that("sum_expr supports filter expressions", {
  result <- sum_expr(x[i], i = 1:3, i >= 3)
  expect_equal(deparse(result), "x[3L]")
})

test_that("extract_coefficients can extract constant", {
  ast <- substitute(1 + x * 10 + x[1, 2] + 23 + -1 * 3)
  result <- extract_coefficients_internal(ast)
  expect_equivalent(21, result$constant)
})

test_that("extract_coefficients can extract coefficients", {
  ast <- substitute(1 + x * (5 + 5) + x[1, 2] + 23 + -1 * 3)
  result <- extract_coefficients_internal(ast)
  expect_equal(2, length(result$coefficients))
  expect_equivalent(10, result$coefficients[["x"]]$coef)
  expect_equivalent(1, result$coefficients[["x[1, 2]"]]$coef)
})

test_that("check_expression handles special cases", {
  expect_silent(check_expression(add_variable(MIPModel(), x), substitute(x)))
  expect_error(check_expression(add_variable(MIPModel(), x), substitute(y)))
  expect_error(check_expression(
    add_variable(MIPModel(), x[i], i = 1),
    substitute(x)
  ))
})

test_that("extract_coefficients can extract coefficients #2", {
  ast <- substitute(5 * x)
  result <- extract_coefficients_internal(ast)
  expect_equal(1, length(result$coefficients))
  expect_equivalent(5, result$coefficients[["x"]]$coef)
})

test_that("extract_coefficients fails if unkown operator is used", {
  ast <- substitute(5^x)
  expect_error(extract_coefficients_internal(ast))
})

test_that("extract_coefficients can extract coefficients #3", {
  ast <- substitute(x)
  result <- extract_coefficients_internal(ast)
  expect_equal(1, length(result$coefficients))
  expect_equivalent(1, result$coefficients[["x"]]$coef)
})

test_that("bug 20161107 #103: bug in extract coefficient (1)", {
  a <- 12
  m <- MIPModel() %>%
    add_variable(x) %>%
    set_objective(-(a * x))
  result <- extract_coefficients_internal(m$objective$expression[[1]])
  expect_equal(-12, result$coefficients[["x"]]$coef)
  expect_equal(0, result$constant)
})

test_that("bug 20161107 #103: bug in extract coefficient (2)", {
  a <- 47
  m <- MIPModel() %>%
    add_variable(x[i], i = 1:2) %>%
    add_variable(y) %>%
    add_constraint(y - a * x[i] <= 1, i = 1:2)
  result <- extract_coefficients_internal(m$constraints[[1]]$lhs[[1]])
  expect_equal(-47, result$coefficients[["x[1L]"]]$coef)
})

test_that("sum_expr returns 0 if filter expression yields 0 variables", {
  m <- MIPModel() %>%
    add_variable(x[i], i = 1:2) %>%
    add_variable(y) %>%
    add_constraint(sum_expr(x[i], i = 1:2, i == 23) == 1)
  expect_equal(0, m$constraints[[1]]$lhs[[1]])
})
