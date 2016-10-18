context("model")

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

test_that("set_objective throws an error if it is non-linear", {
  m <- add_variable(new("Model"), x[i], i = 1:3, type = "binary")
  expect_error(set_objective(m, sum_expr(x[i], i = 1:2) * x[3]))
})

test_that("we can solve a model", {
  m <- add_variable(new("Model"), x[i], i = 1:3, type = "binary")
  m <- add_constraint(m, sum_expr(x[i], i = 1:3) == 1)
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
    add_constraint(sum_expr(x[i], i = 1:3) == 1) %>%
    set_objective(x[1])
  expect_equal(length(m@variables), 1)
})

test_that("set_object passes external values to sum_expr", {
  max_bins <- 5
  m <- new("Model")
  m <- add_variable(m, y[i], i = 1:max_bins, type = "binary")
  m <- set_objective(m, sum_expr(y[i], i = 1:max_bins), "min")
})

test_that("we can model a tsp", {
  cities <- 3
  distance_matrix <- as.matrix(dist(1:cities, diag = TRUE, upper = TRUE))
  sub_tours <- list(1, 2, 3, c(1, 2), c(1, 3), c(2, 3))
  r <- MIPModel() %>%
    add_variable(x[i, j], i = 1:cities, j = 1:cities, type = "binary") %>%
    set_objective(sum_expr(distance_matrix[i, j] * x[i, j],
                          i = 1:cities, j = 1:cities), direction = "min") %>%
    add_constraint(x[i, i] == 0, i = 1:cities) %>%
    add_constraint(x[i, j] == x[j, i], i = 1:cities, j = 1:cities) %>%
    add_constraint(sum_expr(x[i, j], i = sub_tours[[s]], j = sub_tours[[s]]) <=
                     length(sub_tours[s]) - 1, s = 1:length(sub_tours))
})

test_that("bug 20160701: -x as a formula", {
  add_variable(MIPModel(), x, type = "continuous", lb = 4) %>%
    add_variable(y, type = "continuous", ub = 4) %>%
    add_constraint(x + y <= 10) %>%
    set_objective(-x + y, direction = "max")
})

test_that("model has a nice default output", {
  m <- add_variable(MIPModel(), x, type = "continuous", lb = 4) %>%
    add_variable(y, type = "continuous", ub = 4) %>%
    add_constraint(x + y <= 10) %>%
    set_objective(-x + y, direction = "max")
  expect_output(show(m), "Constraints: 1")
})

test_that("bug 20161011 #83: bounds of binary vars are not 0/1", {
  model <- add_variable(MIPModel(), x, type = "binary") %>%
    add_constraint(x <= 10) %>%
    set_objective(-x, direction = "max")
  expect_equal(0, model@variables[[1]]@lb)
  expect_equal(1, model@variables[[1]]@ub)
})

test_that("multiplications in objective fun", {
  m <- add_variable(MIPModel(), x, type = "continuous", lb = 4) %>%
    add_variable(y, type = "continuous", ub = 4) %>%
    add_constraint(x + y <= 10) %>%
    set_objective(5 * (-x + y), direction = "max")
  expect_equal(deparse(m@objective@expression[[1]]), "-5 * x + 5 * y")
})

test_that("model output works without an obj. function", {
  m <- add_variable(MIPModel(), x, type = "continuous", lb = 4)
  expect_output(show(m))
})

test_that("devision in objective fun", {
  m <- add_variable(MIPModel(), x, type = "continuous", lb = 4) %>%
    add_variable(y, type = "continuous", ub = 4) %>%
    add_constraint(x + y <= 10) %>%
    set_objective(5 / (-x + y), direction = "max")
  expect_equal(deparse(m@objective@expression[[1]]), "-0.2 * x + 0.2 * y")
})

test_that("small to mid sized models should work", {
  n <- 400
  result <- MIPModel() %>%
    add_variable(x[i], i = 1:n, type = "binary") %>%
    set_objective(sum_expr(x[i], i = 1:n), "max") %>%
    add_constraint(sum_expr(x[i], i = 1:n) == 1)
})

test_that("bug 20160713 #41: quantifiers in constraints in sum_expr", {
  MIPModel() %>%
    add_variable(x[i], i = 1:9) %>%
    add_constraint(sum_expr(x[i], i = 1:3 + y) == 1, y = c(0, 3, 6))
})

test_that("small to mid sized model should work #2", {
  n <- 40
  coef <- matrix(1:(n ^ 2), ncol = 2)
  MIPModel() %>%
    add_variable(x[i, j], i = 1:n, j = 1:n) %>%
    add_constraint(sum_expr(coef[i, j] * x[i, j], i = 1:n, j = 1:n) == 1)
})

test_that("bug 20160729: two sum_expr on one side", {
  m <- MIPModel() %>%
    add_variable(x[j], j = 1:4) %>%
    add_constraint(sum_expr(x[j], j = 1:2) - sum_expr(x[j], j = 3:4) == 0)
  expect_equal(deparse(m@constraints[[1]]@lhs[[1]]),
               "x[1L] + x[2L] + (-1 * x[3L] + -1 * x[4L])")
})

test_that("bug 20160729_2: external var binding", {
  x <- 1:10
  m <- MIPModel() %>%
    add_variable(x[j], j = 1:2)
  expect_warning(add_constraint(m, sum_expr(x[j], j = 1:2) == 0))
  expect_warning(add_constraint(m, sum_expr(x[j], j = 1:2) == 0),
                 regexp = "x")
})

test_that("solve_model warns about wrong arguments", {
  m <- MIPModel()
  expect_error(solve_model(m, not_a_fun <- 0), regexp = "function")
})

test_that("set_objective_ supports standard eval.", {
  m <- MIPModel()
  m <- add_variable_(m, ~x)
  m <- set_objective_(m, ~x)
})

