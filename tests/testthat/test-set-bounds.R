context("MIP: set-bounds")

test_that("set_bounds checks if single variable exists", {
  m <- add_variable(MIPModel(), x, lb = 10, ub = 12)
  expect_error(set_bounds(m, y, lb = 10))
})

test_that("bounds can be changed of a single variable", {
  m <- add_variable(MIPModel(), x, lb = 10, ub = 12)
  m1 <- set_bounds(m, x, lb = 1, ub = 2)
  expect_equal(m1$variable_bounds_lower, 1)
  expect_equal(m1$variable_bounds_upper, 2)
  expect_equal(m$variable_bounds_lower, 10)
  expect_equal(m$variable_bounds_upper, 12)
})

test_that("set_bounds only bounds if explicitly named", {
  m <- add_variable(MIPModel(), x, lb = 10, ub = 12)
  m1 <- set_bounds(m, x, lb = 1)
  expect_equal(m1$variable_bounds_lower, 1)
  expect_equal(m1$variable_bounds_upper, 12)
})

test_that("set_bounds only bounds if explicitly named", {
  m <- add_variable(MIPModel(), x, lb = 10, ub = 12)
  m1 <- set_bounds(m, x, ub = 1)
  expect_equal(m1$variable_bounds_lower, 10)
  expect_equal(m1$variable_bounds_upper, 1)
})

test_that("set_bounds fails if indexed variable does not exists in model", {
  m <- MIPModel()
  m <- add_variable(MIPModel(), x[i], i = 1:10)
  expect_error(set_bounds(m, y[5], lb = 1, ub = 2))
  expect_error(set_bounds(m, x[23], lb = 1, ub = 2))
})

test_that("bounds can be changed of an indexed variable", {
  m <- add_variable(MIPModel(), x[i], i = 1:10)
  m <- set_bounds(m, x[5], lb = 1, ub = 2)
  expect_equal(m$variable_bounds_lower[5], 1)
  expect_equal(m$variable_bounds_upper[5], 2)
  expect_equal(m$variable_bounds_lower[4], -Inf)
  expect_equal(m$variable_bounds_upper[4], Inf)
})

test_that("bounds can be changed of an indexed variable 1", {
  m <- MIPModel()
  m <- add_variable(MIPModel(), x[i, j], i = 1:2, j = 1:2)
  m <- set_bounds(m, x[1, 2], lb = 1, ub = 2)
  expect_equal(m$variable_bounds_lower[2], 1)
  expect_equal(m$variable_bounds_upper[2], 2)
})

test_that("bounds can be changed of an indexed variable 2", {
  m <- MIPModel()
  m <- add_variable(MIPModel(), x[i, j], i = 1:2, j = 1:2)
  m <- set_bounds(m, x[1, i], lb = 1, ub = 2, i = 2)
  expect_equal(m$variable_bounds_lower[2], 1)
  expect_equal(m$variable_bounds_upper[2], 2)
})

test_that("set_bounds fails if there a non-bound variables in index", {
  m <- add_variable(MIPModel(), x[i, j], i = 1:10, j = 1:10)
  expect_error(set_bounds(m, x[i, j], lb = 1, ub = 2))
})

test_that("bounds can be changed of a two-index variable", {
  m <- MIPModel()
  m <- add_variable(MIPModel(), x[i, j], i = 1:3, j = 1:10)
  m <- set_bounds(m, x[i, j], lb = 1, ub = 2, i = 1:2, j = 1:2)
  lapply(c(1, 2, 11, 12), function(i) {
    expect_equal(m$variable_bounds_lower[i], 1)
    expect_equal(m$variable_bounds_upper[i], 2)
  })
})


test_that("bug 20161007: bound indexes can be reused", {
  m <- MIPModel()
  m <- add_variable(MIPModel(), x[i, j], i = 1:3, j = 1:10)
  m <- set_bounds(m, x[i, i], lb = 1, ub = 2, i = c(1, 2))
  expect_equal(m$variable_bounds_lower[1], 1)
  expect_equal(m$variable_bounds_upper[1], 2)
  expect_equal(m$variable_bounds_lower[12], 1)
  expect_equal(m$variable_bounds_upper[12], 2)
})

test_that("bounds can be changed of a two-index variable with standard eval", {
  m <- add_variable(MIPModel(), x[i, j], i = 1:3, j = 1:10)
  m <- set_bounds_(m, ~x[i, j], lb = 1, ub = 2, i = c(1, 2), j = c(1, 2))
  expect_equal(m$variable_bounds_lower[1], 1)
  expect_equal(m$variable_bounds_upper[1], 2)
  expect_equal(m$variable_bounds_lower[12], 1)
  expect_equal(m$variable_bounds_upper[12], 2)
})

test_that("quantifiers support filter expressions", {
  m <- add_variable(MIPModel(), x[i, j], i = 1:3, j = 1:10)
  m <- set_bounds(m, x[i, j], lb = 1, ub = 2,
                  i = c(1, 2), j = c(1, 2, 3), i == 1, j == 1)
  expect_equal(m$variable_bounds_lower[1], 1)
  expect_equal(m$variable_bounds_upper[1], 2)
})

test_that("quantifiers support filter expressions with SE", {
  m <- add_variable(MIPModel(), x[i, j], i = 1:3, j = 1:10)
  m <- set_bounds_(m, ~x[i, j], lb = 1, ub = 2,
                  i = c(1, 2), j = c(1, 2, 3), .dots = list(~i == 1, ~j == 1))
  expect_equal(m$variable_bounds_lower[1], 1)
  expect_equal(m$variable_bounds_upper[1], 2)
})
