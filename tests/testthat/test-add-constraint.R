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

test_that("constraints should be saved with only + operators", {
  m <- add_variable(MIPModel(), x, type = "continuous", lb = 4) %>%
    add_variable(y, type = "continuous", ub = 4) %>%
    add_constraint(x - y <= 10) %>%
    set_objective(5 / (-x + y), sense = "max")
  expect_equal(deparse(m$constraints[[1]]$lhs[[1]]), "x + -1 * y")
})

test_that("constraints can have an unary + operator", {
  m <- add_variable(MIPModel(), x, type = "continuous", lb = 4) %>%
    add_variable(y, type = "continuous", ub = 4) %>%
    add_constraint(+x - y <= 10) %>%
    set_objective(5 / (-x + y), sense = "max")
  expect_equal(deparse(m$constraints[[1]]$lhs[[1]]), "1 * x + -1 * y")
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

test_that("we can add complicated constraints", {
  m <- add_variable(MIPModel(), x[i], i = 1:3, type = "binary")
  m <- add_constraint(m, sum_expr(x[i], i = 1:2) + x[3] == 1)
  expect_equal(length(m$constraints), 1)
  constraint <- m$constraints[[1]]
  expect_equal(constraint$rhs[[1]], 1)
  expect_equal(constraint$sense, "==")
  expect_equal(deparse(constraint$lhs[[1]]), "x[1L] + x[2L] + x[3]")
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

test_that("bounded vars in add_constraints should take precedence", {
  m <- MIPModel() %>%
    add_variable(x[i, j], i = 1:3, j = 1:3)
  j <- 1
  i <- 1
  m <- add_constraint(m, sum_expr(x[i, j], j = 3) == 1)
  expect_equal(m$constraints[[1]]$lhs, expression(x[1, 3]))
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

test_that("quantifier filter expressions work with add_constraint_", {
  m <- add_variable(MIPModel(), x[i], i = 1:10)
  m <- add_constraint_(m, ~ x[i] == 1, i = 1:10, .dots = ~ i <= 2)
  expect_equal(2, length(m$constraints))
})

test_that("indexes in filter expr. for sum_expr are correctly substituted", {
  m <- add_variable(MIPModel(), x[i, j], i = 1:2, j = 1:2)
  m <- add_constraint(m, sum_expr(x[i, j], i = 1:2, j == 1) == 0, j = 1:2)
  expect_equal("x[1, 1] + x[2, 1]", as.character(m$constraints[[1]]$lhs))
  expect_equal("0", as.character(m$constraints[[2]]$lhs))
})

test_that("bug 20170222 #117: you can use c as an index variable", {
  m <- add_variable(MIPModel(), x[m], m = 1:2)
  m <- add_constraint(m, x[c] == 0, c = 1:2)
  m <- set_bounds(m, x[m], lb = 0, m = 1:2)
  expect_equal("x[1]", as.character(m$constraints[[1]]$lhs))
  expect_equal("x[2]", as.character(m$constraints[[2]]$lhs))
})
