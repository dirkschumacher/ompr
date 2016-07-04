context("Model")

test_that("is_defined works with single variables", {
  m <- add_variable(new("Model"), x, type = "binary")
  expect_true(is_defined(m, x))
})

test_that("is_defined works with subscripts", {
  m <- add_variable(new("Model"), x[i], i = 1:10, type = "binary")
  expect_true(is_defined(m, x[3]))
})

test_that("only accept valid models", {
  expect_error(add_variable(mtcars, x[i], i = 1:10))
})

test_that("subscripts must be present in variable expression", {
  expect_error(add_variable(Model(), x[i], j = 1:10))
})
test_that("variable must not be a complex expression", {
  expect_error(add_variable(Model(), x[i] + 4, i = 1:10))
})

test_that("variables with arity 0 can be added", {
  m <- Model()
  m_new <- add_variable(m, x, type = "binary")
  expect_false(is.null(m_new@variables[["x"]]))
  expect_equal(m_new@variables[["x"]]@type, "binary")
  expect_equal(m_new@variables[["x"]]@arity, 0)
})

test_that("variables can be added", {
  m <- add_variable(new("Model"), x[i], i = 1:10, type = "binary")
  expect_false(is.null(m@variables[["x"]]))
  expect_equal(m@variables[["x"]]@type, "binary")
  expect_equal(m@variables[["x"]]@arity, 1)
})

test_that("global variables do not interfere with variable names in expressions", {
  x <- "hi"
  m <- add_variable(new("Model"), x[i], i = 1:10, type = "binary")
  expect_false(is.null(m@variables[["x"]]))
  expect_true(is.null(m@variables[["hi"]]))
})

test_that("variables can be added with bound indexes", {
  j <- 3
  m <- add_variable(new("Model"), x[i, j], i = 1:10, type = "binary")
  expect_false(is.null(m@variables[["x"]]))
  expect_equal(m@variables[["x"]]@arity, 2)
  expect_true(is_defined(m, x[1, 3]))
  expect_false(is_defined(m, x[1, 2]))
})

test_that("one can set an objective function", {
  m <- new("Model")
  m <- add_variable(m, x[i], i = 1:10, type = "binary")
  m <- set_objective(m, x[1] + x[3], direction = "min")
  expect_equal(m@objective@direction, "min")
  expect_equal(deparse(m@objective@expression[[1]]), "x[1] + x[3]")
})

test_that("only max and min are valid directions for an objective function", {
  m <- add_variable(new("Model"), x[i], i = 1:10, type = "binary")
  expect_error(set_objective(m, x[1] + x[3], direction = "wat"))
  set_objective(m, x[1] + x[3], direction = "min")
  set_objective(m, x[1] + x[3], direction = "max")
})

test_that("all symbols in an obj. function need to be variables", {
  m <- add_variable(new("Model"), x[i], i = 1:2, type = "binary")
  expect_error(set_objective(m, x[5], direction = "min"))
})

test_that("obj. function can bind external variables", {
  w <- c(1, 2)
  m <- add_variable(new("Model"), x[i], i = 1:2, type = "binary")
  m <- set_objective(m, w[1] * x[1], direction = "min")
  expect_equal(deparse(m@objective@expression[[1]]), "1 * x[1]")
})

test_that("we can add constraints", {
  m <- new("Model")
  m <- add_variable(m, x[i], i = 1:10, type = "binary")
  m <- add_constraint(m, x[3], "<=", x[6])
})

test_that("add_constraint only allows a fixed set of directions", {
  m <- new("Model")
  m <- add_variable(m, x[i], i = 1:10, type = "binary")
  add_constraint(m, x[3], "<=", x[6])
  add_constraint(m, x[3], ">=", x[6])
  add_constraint(m, x[3], "=", x[6])
  add_constraint(m, x[3], "==", x[6])
  expect_error(add_constraint(m, x[3], "<", x[6]))
  expect_error(add_constraint(m, x[3], ">", x[6]))
  expect_error(add_constraint(m, x[3], "wat", x[6]))
})

test_that("we can add complicated constraints", {
  m <- add_variable(new("Model"), x[i], i = 1:3, type = "binary")
  m <- add_constraint(m, sum_exp(x[i], i = 1:2) + x[3], "==", 1)
  expect_equal(length(m@constraints), 1)
  constraint <- m@constraints[[1]]
  expect_equal(constraint@rhs[[1]], 1)
  expect_equal(constraint@direction, "==")
  expect_equal(deparse(constraint@lhs[[1]]), "x[1L] + x[2L] + x[3]")
})

test_that("add_constraint converts = to ==", {
  m <- add_variable(new("Model"), x[i], i = 1:3, type = "binary")
  m <- add_constraint(m, sum_exp(x[i], i = 1:2) + x[3], "=", 1)
  constraint <- m@constraints[[1]]
  expect_equal(constraint@direction, "==")
})

test_that("add_constraint throws an error if constraints are non-linear", {
  m <- add_variable(new("Model"), x[i], i = 1:3, type = "binary")
  expect_error(add_constraint(m, sum_exp(x[i], i = 1:2) * x[3], "==", 1))
})

test_that("set_objective throws an error if it is non-linear", {
  m <- add_variable(new("Model"), x[i], i = 1:3, type = "binary")
  expect_error(set_objective(m, sum_exp(x[i], i = 1:2) * x[3]))
})

test_that("we can solve a model", {
  m <- add_variable(new("Model"), x[i], i = 1:3, type = "binary")
  m <- add_constraint(m, sum_exp(x[i], i = 1:3), "==", 1)
  m <- set_objective(m, x[1])
  solution <- new("Solution")
  result <- solve_model(m, function(model) {
    expect_identical(model, m)
    solution
  })
  expect_identical(result, solution)
})

test_that("it works with magrittr pipes", {
  m <- add_variable(new("Model"), x[i], i = 1:3, type = "binary") %>%
    add_constraint(sum_exp(x[i], i = 1:3), "==", 1) %>%
    set_objective(x[1])
  expect_equal(length(m@variables), 1)
})

test_that("binding variables works with magrittr", {
  y <- 1
  m <- add_variable(new("Model"), x[i], i = 1:3, type = "binary") %>%
    set_objective(x[1] * y)
  expect_equal(deparse(m@objective@expression[[1]]), "x[1] * 1")
})

test_that("set_object passes external values to sum_exp", {
  max_bins <- 5
  m <- new("Model")
  m <- add_variable(m, y[i], i = 1:max_bins, type = "binary")
  m <- set_objective(m, sum_exp(y[i], i = 1:max_bins), "min")
})


test_that("one can set bounds on variables", {
  max_bins <- 5
  m <- new("Model")
  m1 <- add_variable(m, x, type = "continuous", lb = 40)
  m2 <- add_variable(m, x, type = "continuous", ub = 40)
  expect_equal(m1@variables[["x"]]@lb, 40)
  expect_equal(m2@variables[["x"]]@ub, 40)
})

test_that("throw error if lower bound > upper bound", {
  expect_error(add_variable(new("Model"), x, lb = 5, ub = 4))
})

test_that("add_constraint can handle a for all quantifier", {
  m <- new("Model") %>%
    add_variable(x[i, j], i = 1:3, j = 1:3) %>%
    add_constraint(sum_exp(x[i, j], j = 1:3), "==", 1, i = 1:3)
  expect_equal(length(m@constraints), 3)
})

test_that("add_constraint warns about unbouded all quantifier", {
  m <- new("Model") %>%
    add_variable(x[i, j], i = 1:3, j = 1:3)
  expect_error(add_constraint(sum_exp(x[i, j], j = 1:3), "==", 1, e = 1:3))
})

test_that("bounded vars in add_constraints should take precedence", {
  m <- new("Model") %>%
    add_variable(x[i, j], i = 1:3, j = 1:3)
  j <- 1
  i <- 1
  m <- add_constraint(m, sum_exp(x[i, j], j = 3), "==", 1)
  expect_equal(m@constraints[[1]]@lhs, expression(x[1, 3]))
})

test_that("we can model a tsp", {
  cities <- 3
  distance_matrix <- as.matrix(dist(1:cities, diag = TRUE, upper = TRUE))
  sub_tours <- list(1, 2, 3, c(1, 2), c(1, 3), c(2, 3))
  r <- MIPModel() %>%
    add_variable(x[i, j], i = 1:cities, j = 1:cities, type = "binary") %>%
    set_objective(sum_exp(distance_matrix[i, j] * x[i, j], i = 1:cities, j = 1:cities), direction = "min") %>%
    add_constraint(x[i, i], "==", 0, i = 1:cities) %>%
    add_constraint(x[i, j], "==", x[j, i], i = 1:cities, j = 1:cities) %>%
    add_constraint(sum_exp(x[i, j], i = sub_tours[[s]], j = sub_tours[[s]]), "<=", length(sub_tours[s]) - 1, s = 1:length(sub_tours))
})

test_that("bug 20160701: -x as a formula", {
  add_variable(MIPModel(), x, type = "continuous", lb = 4) %>%
    add_variable(y, type = "continuous", ub = 4) %>%
    add_constraint(x + y, "<=", 10) %>%
    set_objective(-x + y, direction = "max")
})

test_that("model has a nice default output", {
  m <- add_variable(MIPModel(), x, type = "continuous", lb = 4) %>%
    add_variable(y, type = "continuous", ub = 4) %>%
    add_constraint(x + y, "<=", 10) %>%
    set_objective(-x + y, direction = "max")
  expect_output(show(m), "Constraints: 1")
})
