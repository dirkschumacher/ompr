context("MIP: add-constraint")

test_that("add_constraint only accepts linear inequalities", {
  m <- add_variable(MIPModel(), x, type = "binary")
  expect_error(add_constraint(m, x))
})

test_that("add_constraint_ supports standard eval.", {
  m <- MIPModel()
  m <- add_variable(m, x)
  expect_silent(add_constraint_(m, ~ x <= 10))
})

test_that("add_constraint can handle a for all quantifier", {
  m <- MIPModel() %>%
    add_variable(x[i, j], i = 1:3, j = 1:3) %>%
    add_constraint(sum_expr(x[i, j], j = 1:3) == 1, i = 1:3)
  expect_equal(length(m$constraints), 3)
})

test_that("add_constraint warns about unbouded all quantifier", {
  m <- MIPModel() %>%
    add_variable(x[i, j], i = 1:3, j = 1:3)
  expect_error(add_constraint(sum_expr(x[i, j], j = 1:3) == 1, e = 1:3))
})

test_that("add_constraint throws an error if constraints are non-linear lhs", {
  m <- add_variable(MIPModel(), x[i], i = 1:3, type = "binary")
  expect_error(add_constraint(m, sum_expr(x[i], i = 1:2) * x[3] == 1))
})

test_that("add_constraint throws an error if constraints are non-linear rhs", {
  m <- add_variable(MIPModel(), x[i], i = 1:3, type = "binary")
  expect_error(add_constraint(m, 1 == sum_expr(x[i], i = 1:2) * x[3]))
})

test_that("add_constraint warns about unbouded all quantifier in rhs", {
  m <- MIPModel() %>%
    add_variable(x[i, j], i = 1:3, j = 1:3)
  expect_error(add_constraint(1 == sum_expr(x[i, j], j = 1:3), e = 1:3))
})

test_that("add_constraints throws error if unbounded indexes in lhs", {
  m <- MIPModel() %>%
    add_variable(x[i, j], i = 1:3, j = 1:3)
  expect_error(add_constraint(x[1, j] == 1))
})

test_that("add_constraints throws error if unbounded indexes in rhs", {
  m <- MIPModel() %>%
    add_variable(x[i, j], i = 1:3, j = 1:3)
  expect_error(add_constraint(1 == x[1, j]))
})

test_that("we can add constraints", {
  expect_silent({
    m <- MIPModel()
    m <- add_variable(m, x[i], i = 1:10, type = "binary")
    m <- add_constraint(m, x[3] <= x[6])
  })
})

test_that("add_constraint only allows a fixed set of senses", {
  m <- MIPModel()
  m <- add_variable(m, x[i], i = 1:10, type = "binary")
  add_constraint(m, x[3] <= x[6])
  add_constraint(m, x[3] >= x[6])
  add_constraint(m, x[3] == x[6])
  expect_error(add_constraint(m, x[3] < x[6]))
  expect_error(add_constraint(m, x[3] > x[6]))
  expect_error(add_constraint(m, x[3] + x[6]))
})

test_that("quantifier filter expressions work with add_constraint", {
  m <- add_variable(MIPModel(), x[i], i = 1:10)
  m <- add_constraint(m, x[i] == 1, i = 1:10, i <= 2)
  expect_equal(2, length(m$constraints))
})

test_that("quantifier filter expressions are combined with AND", {
  m <- add_variable(MIPModel(), x[i], i = 1:10)
  m <- add_constraint(m, x[i] == 1, i = 1:10, i <= 2, i >= 2)
  expect_equal(1, length(m$constraints))
})
