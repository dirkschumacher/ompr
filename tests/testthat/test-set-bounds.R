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
  m <- set_bounds_(m, ~ x[i, j], lb = 1, ub = 2, i = c(1, 2), j = c(1, 2))
  expect_equal(m$variable_bounds_lower[1], 1)
  expect_equal(m$variable_bounds_upper[1], 2)
  expect_equal(m$variable_bounds_lower[12], 1)
  expect_equal(m$variable_bounds_upper[12], 2)
})

test_that("quantifiers support filter expressions", {
  m <- add_variable(MIPModel(), x[i, j], i = 1:3, j = 1:10)
  m <- set_bounds(m, x[i, j],
    lb = 1, ub = 2,
    i = c(1, 2), j = c(1, 2, 3), i == 1, j == 1
  )
  expect_equal(m$variable_bounds_lower[1], 1)
  expect_equal(m$variable_bounds_upper[1], 2)
})

test_that("quantifiers support filter expressions with SE", {
  m <- add_variable(MIPModel(), x[i, j], i = 1:3, j = 1:10)
  m <- set_bounds_(m, ~ x[i, j],
    lb = 1, ub = 2,
    i = c(1, 2), j = c(1, 2, 3), .dots = list(~ i == 1, ~ j == 1)
  )
  expect_equal(m$variable_bounds_lower[1], 1)
  expect_equal(m$variable_bounds_upper[1], 2)
})

test_that("bounds can be added using inequalities", {
  n <- 3
  m <- add_variable(MIPModel(), x[i], i = 1:n)
  m1 <- set_bounds(m, x[i] <= 10 + 1, i = 1:n)
  m1 <- set_bounds(m1, x[i] >= 10 + 1, i = 1:n)
  m2 <- set_bounds(m, 10 + 1 <= x[i], i = 1:n)
  m2 <- set_bounds(m2, 10 + 1 >= x[i], i = 1:n)
  expect_equal(m1$variable_bounds_lower, rep.int(11, n))
  expect_equal(m1$variable_bounds_upper, rep.int(11, n))
  expect_equal(m2$variable_bounds_lower, rep.int(11, n))
  expect_equal(m2$variable_bounds_upper, rep.int(11, n))

  m <- add_variable(MIPModel(), x)
  m1 <- set_bounds(m, x <= 10 + 1)
  m1 <- set_bounds(m1, x >= 10 + 1)
  m2 <- set_bounds(m, 10 + 1 <= x)
  m2 <- set_bounds(m2, 10 + 1 >= x)
  expect_equal(m1$variable_bounds_lower, 11)
  expect_equal(m1$variable_bounds_upper, 11)
  expect_equal(m2$variable_bounds_lower, 11)
  expect_equal(m2$variable_bounds_upper, 11)
})

test_that("bounds can be added using equalities", {
  n <- 3
  m <- add_variable(MIPModel(), x[i], i = 1:n)
  m1 <- set_bounds(m, x[i] == i, i = 1:n)
  expect_equal(m1$variable_bounds_lower, 1:n)
  expect_equal(m1$variable_bounds_upper, 1:n)
  m2 <- set_bounds(m, i == x[i], i = 1:n)
  expect_equal(m2$variable_bounds_lower, 1:n)
  expect_equal(m2$variable_bounds_upper, 1:n)

  m <- add_variable(MIPModel(), x)
  m1 <- set_bounds(m, x == 1)
  expect_equal(m1$variable_bounds_lower, 1)
  expect_equal(m1$variable_bounds_upper, 1)
  m2 <- set_bounds(m, 1 == x)
  expect_equal(m2$variable_bounds_lower, 1)
  expect_equal(m2$variable_bounds_upper, 1)
})

test_that("set_bounds fails if no linear constraint is passed and no bounds either", {
  m <- add_variable(MIPModel(), x)
  expect_error(
    set_bounds(m, x + 10), "Bounds"
  )
  expect_error(
    set_bounds(m, x + 10 < 10)
  )
})

test_that("set_bounds doc example works", {
  model <- add_variable(MIPModel(), x[i], i = 1:5)
  model <- set_bounds(model, x[i] <= i, i = 1:5)
  model <- set_bounds(model, x[i] >= 0, i = 1:5)
  model <- set_bounds(model, x[5] == 45)
  bnds <- variable_bounds(model)
  expect_equal(bnds$lower, c(0, 0, 0, 0, 45))
  expect_equal(bnds$upper, c(1, 2, 3, 4, 45))
})
