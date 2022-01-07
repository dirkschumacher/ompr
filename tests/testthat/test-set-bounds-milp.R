context("MILP: set-bounds")

test_that("set_bounds checks if single variable exists", {
  m <- add_variable(MILPModel(), x, lb = 10, ub = 12)
  expect_error(set_bounds(m, y, lb = 10))
})

test_that("bounds can be changed of a single variable", {
  m <- add_variable(MILPModel(), x, lb = 10, ub = 12)
  m1 <- set_bounds(m, x, lb = 1, ub = 2)
  expect_equal(m1$variables[["x"]]$lb, 1)
  expect_equal(m1$variables[["x"]]$ub, 2)
  expect_equal(m$variables[["x"]]$lb, 10)
  expect_equal(m$variables[["x"]]$ub, 12)
})

test_that("set_bounds only bounds if explicitly named", {
  m <- add_variable(MILPModel(), x, lb = 10, ub = 12)
  m1 <- set_bounds(m, x, lb = 1)
  expect_equal(m1$variables[["x"]]$lb, 1)
  expect_equal(m1$variables[["x"]]$ub, 12)
})

test_that("set_bounds only bounds if explicitly named", {
  m <- add_variable(MILPModel(), x, lb = 10, ub = 12)
  m1 <- set_bounds(m, x, ub = 1)
  expect_equal(m1$variables[["x"]]$lb, 10)
  expect_equal(m1$variables[["x"]]$ub, 1)
})

test_that("set_bounds fails if indexed variable does not exists in model", {
  m <- MILPModel()
  m <- add_variable(MILPModel(), x[i], i = 1:10)
  expect_error(set_bounds(m, y[5], lb = 1, ub = 2))
  expect_error(set_bounds(m, x[23], lb = 1, ub = 2))
})

test_that("bounds can be changed of an indexed variable", {
  m <- add_variable(MILPModel(), x[i], i = 1:10)
  m <- set_bounds(m, x[5], lb = 1, ub = 2)
  expect_equal(m$variables[["x"]]$lb[5], 1)
  expect_equal(m$variables[["x"]]$ub[5], 2)
  expect_equal(m$variables[["x"]]$lb[4], -Inf)
  expect_equal(m$variables[["x"]]$ub[4], Inf)
})

test_that("bounds can be changed of an indexed variable 1", {
  m <- MILPModel()
  m <- add_variable(MILPModel(), x[i, j], i = 1:2, j = 1:2)
  m <- set_bounds(m, x[1, 2], lb = 1, ub = 2)
  expect_equal(m$variables[["x"]]$lb[3], 1)
  expect_equal(m$variables[["x"]]$ub[3], 2)
})

test_that("bounds can be changed of an indexed variable 2", {
  m <- MILPModel()
  m <- add_variable(MILPModel(), x[i, j], i = 1:2, j = 1:2)
  m <- set_bounds(m, x[1, i], lb = 1, ub = 2, i = 2)
  expect_equal(m$variables[["x"]]$lb[3], 1)
  expect_equal(m$variables[["x"]]$ub[3], 2)
})

test_that("set_bounds fails if there a non-bound variables in index", {
  m <- add_variable(MILPModel(), x[i, j], i = 1:10, j = 1:10)
  expect_error(set_bounds(m, x[i, j], lb = 1, ub = 2))
})

test_that("bounds can be changed of a two-index variable", {
  m <- add_variable(MILPModel(), x[i, j], i = 1:3, j = 1:10)
  m <- set_bounds(m, x[i, j], lb = 1, ub = 2, i = 1:2, j = 1:2)
  lapply(c(1, 2, 4, 5), function(i) {
    expect_equal(m$variables[["x"]]$lb[i], 1)
    expect_equal(m$variables[["x"]]$ub[i], 2)
  })
})


test_that("bug 20161007: bound indexes can be reused", {
  m <- MILPModel()
  m <- add_variable(MILPModel(), x[i, j], i = 1:3, j = 1:10)
  m <- set_bounds(m, x[i, i], lb = 1, ub = 2, i = c(1, 2))
  expect_equal(m$variables[["x"]]$lb[1], 1)
  expect_equal(m$variables[["x"]]$ub[1], 2)
  expect_equal(m$variables[["x"]]$lb[5], 1)
  expect_equal(m$variables[["x"]]$ub[5], 2)
})

test_that("bounds can be changed of a two-index variable with standard eval", {
  m <- add_variable(MILPModel(), x[i, j], i = 1:3, j = 1:10)
  m <- set_bounds_(m, ~ x[i, j], lb = 1, ub = 2, i = c(1, 2), j = c(1, 2))
  expect_equal(m$variables[["x"]]$lb[1], 1)
  expect_equal(m$variables[["x"]]$ub[1], 2)
  expect_equal(m$variables[["x"]]$lb[5], 1)
  expect_equal(m$variables[["x"]]$ub[5], 2)
})

test_that("quantifiers support filter expressions", {
  m <- add_variable(MILPModel(), x[i, j], i = 1:3, j = 1:10)
  m <- set_bounds(m, x[i, j],
    lb = 1, ub = 2,
    i = c(1, 2), j = c(1, 2, 3), i == 1, j == 1
  )
  expect_equal(m$variables[["x"]]$lb[1], 1)
  expect_equal(m$variables[["x"]]$ub[1], 2)
})

test_that("quantifiers support filter expressions with SE", {
  m <- add_variable(MILPModel(), x[i, j], i = 1:3, j = 1:10)
  m <- set_bounds_(m, ~ x[i, j],
    lb = 1, ub = 2,
    i = c(1, 2), j = c(1, 2, 3), .dots = list(~ i == 1, ~ j == 1)
  )
  expect_equal(m$variables[["x"]]$lb[1], 1)
  expect_equal(m$variables[["x"]]$ub[1], 2)
})

test_that("fails if expression is not a linear collection", {
  m <- add_variable(MILPModel(), x[i, j], i = 1:3, j = 1:10)
  m <- add_variable(m, y)
  expect_error(set_bounds(m, y[1, 1], lb = 0), "variable")
})
