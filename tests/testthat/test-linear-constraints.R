test_that("LinearTerm constraints are converted correctly", {
  x <- new_linear_term(new_linear_variable(1), coefficient = 1)
  res <- 5 * x <= 10
  expect_s3_class(res, "LinearConstraint")
  expect_equal(res$rhs, 10)
  expect_equal(terms_list(res$lhs), terms_list(5 * x + 0))
  expect_equal(res$lhs$constant, 0)
  expect_equal(res$sense, sense_leq)

  res <- 5 * x >= 10
  expect_s3_class(res, "LinearConstraint")
  expect_equal(res$rhs, 10)
  expect_equal(terms_list(res$lhs), terms_list(5 * x + 0))
  expect_equal(res$lhs$constant, 0)
  expect_equal(res$sense, sense_geq)

  res <- 5 * x == 10
  expect_s3_class(res, "LinearConstraint")
  expect_equal(res$rhs, 10)
  expect_equal(terms_list(res$lhs), terms_list(5 * x + 0))
  expect_equal(res$lhs$constant, 0)
  expect_equal(res$sense, sense_eq)
})

test_that("LinearFunctions constraints are converted correctly", {
  x <- new_linear_term(new_linear_variable(1), coefficient = 1)
  res <- 5 * x + 0 <= 10
  expect_s3_class(res, "LinearConstraint")
  expect_equal(res$rhs, 10)
  expect_equal(terms_list(res$lhs), terms_list(5 * x + 0))
  expect_equal(res$lhs$constant, 0)
  expect_equal(res$sense, sense_leq)

  res <- 5 * x + 0 >= 10
  expect_s3_class(res, "LinearConstraint")
  expect_equal(res$rhs, 10)
  expect_equal(terms_list(res$lhs), terms_list(5 * x + 0))
  expect_equal(res$lhs$constant, 0)
  expect_equal(res$sense, sense_geq)

  res <- 5 * x + 0 == 10
  expect_s3_class(res, "LinearConstraint")
  expect_equal(res$rhs, 10)
  expect_equal(terms_list(res$lhs), terms_list(5 * x + 0))
  expect_equal(res$lhs$constant, 0)
  expect_equal(res$sense, sense_eq)
})

test_that("constraints are always in canonical form", {
  x <- new_linear_term(new_linear_variable(1), coefficient = 1)
  res <- 5 * x + 10 <= -3 * x + 1
  expect_s3_class(res, "LinearConstraint")
  expect_equal(res$rhs, -9)
  expect_equal(res$sense, sense_leq)
})
