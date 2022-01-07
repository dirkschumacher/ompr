context("MILP: model")

test_that("one can set an objective function", {
  m <- MILPModel()
  m <- add_variable(m, x[i], i = 1:10, type = "binary")
  m <- set_objective(m, x[1] + x[3], sense = "min")
  expect_equal(m$objective$sense, "min")
  expect_equal(sum(m$objective$objective@variables$coef), 2)
})

test_that("only max and min are valid directions for an objective function", {
  m <- add_variable(MILPModel(), x[i], i = 1:10, type = "binary")
  expect_error(set_objective(m, x[1] + x[3], sense = "wat"))
  set_objective(m, x[1] + x[3], sense = "min")
  set_objective(m, x[1] + x[3], sense = "max")
})

test_that("all symbols in an obj. function need to be variables", {
  m <- add_variable(MILPModel(), x[i], i = 1:2, type = "binary")
  expect_error(set_objective(m, x[5], sense = "min"))
})

test_that("obj. function can bind external variables", {
  w <- c(1, 2)
  m <- add_variable(MILPModel(), x[i], i = 1:2, type = "binary")
  m <- set_objective(m, w[2] * x[1], sense = "min")
  expect_equal(m$objective$objective@variables$coef, 2)
})

test_that("set_objective throws an error if it is non-linear", {
  m <- add_variable(MILPModel(), x[i], i = 1:3, type = "binary")
  expect_error(set_objective(m, sum_expr(x[i], i = 1:2) * x[3]))
})

test_that("we can solve a model", {
  m <- add_variable(MILPModel(), x[i], i = 1:3, type = "binary")
  m <- add_constraint(m, sum_expr(x[i], i = 1:3) == 1)
  m <- set_objective(m, x[1])
  solution <- new_solution(m, 0, "optimal", solution = c())
  result <- solve_model(m, function(model) {
    expect_identical(model, m)
    solution
  })
  expect_identical(result, solution)
})

test_that("it works with magrittr pipes", {
  m <- add_variable(MILPModel(), x[i], i = 1:3, type = "binary") %>%
    add_constraint(sum_expr(x[i], i = 1:3) == 1) %>%
    set_objective(x[1])
  expect_equal(length(m$variables), 1)
})

test_that("set_object passes external values to sum_expr", {
  max_bins <- 5
  expect_silent({
    m <- MILPModel()
    m <- add_variable(m, y[i], i = 1:max_bins, type = "binary")
    m <- set_objective(m, sum_expr(y[i], i = 1:max_bins), "min")
  })
})

test_that("we can model a tsp", {
  cities <- 3
  distance_matrix <- as.matrix(dist(1:cities, diag = TRUE, upper = TRUE))
  distance <- function(i, j) {
    as.integer(distance_matrix[i, j])
  }
  sub_tours <- list(1, 2, 3, c(1, 2), c(1, 3), c(2, 3))
  expect_silent({
    r <- MILPModel() %>%
      add_variable(x[i, j], i = 1:cities, j = 1:cities, type = "binary") %>%
      set_objective(sum_expr(distance(i, j) * x[i, j],
        i = 1:cities, j = 1:cities
      ), sense = "min") %>%
      add_constraint(x[i, i] == 0, i = 1:cities) %>%
      add_constraint(x[i, j] == x[j, i], i = 1:cities, j = 1:cities) %>%
      add_constraint(sum_expr(x[i, j], i = sub_tours[s], j = sub_tours[s]) <=
        length(sub_tours[s]) - 1, s = 1:length(sub_tours))
  })
})

test_that("bug 20160701: -x as a formula", {
  expect_silent(add_variable(MILPModel(), x, type = "continuous", lb = 4) %>%
    add_variable(y, type = "continuous", ub = 4) %>%
    add_constraint(x + y <= 10) %>%
    set_objective(-x + y, sense = "max"))
})

test_that("model has a nice default output", {
  m <- add_variable(MILPModel(), x, type = "continuous", lb = 4) %>%
    add_variable(y, type = "continuous", ub = 4) %>%
    add_constraint(x + y <= 10) %>%
    set_objective(-x + y, sense = "max")
  expect_output(print(m), "Constraints: 1")
})

test_that("model outputs direction on print", {
  m <- set_objective(add_variable(MILPModel(), x), 0, sense = "max")
  expect_output(print(m), "maximize")
  m <- set_objective(add_variable(MILPModel(), x), 0, sense = "min")
  expect_output(print(m), "minimize")
})


test_that("bug 20161011 #83: bounds of binary vars are not 0/1", {
  model <- add_variable(MILPModel(), x, type = "binary") %>%
    add_constraint(x <= 10) %>%
    set_objective(-x, sense = "max")
  expect_equal(0, model$variables[[1]]$lb)
  expect_equal(1, model$variables[[1]]$ub)
})

test_that("multiplications in objective fun", {
  m <- add_variable(MILPModel(), x, type = "continuous", lb = 4) %>%
    add_variable(y, type = "continuous", ub = 4) %>%
    add_constraint(x + y <= 10) %>%
    set_objective(5 * (-x + y), sense = "max")
  expect_equal(
    m$objective$objective@variables$coef,
    c(-5, 5)
  )
  expect_equal(
    m$objective$objective@variables$variable,
    c("x", "y")
  )
})

test_that("model output works without an obj. function", {
  m <- add_variable(MILPModel(), x, type = "continuous", lb = 4)
  expect_output(show(m))
})

test_that("devision in objective fun", {
  m <- add_variable(MILPModel(), x, type = "continuous", lb = 4) %>%
    add_variable(y, type = "continuous", ub = 4) %>%
    add_constraint(x + y <= 10) %>%
    set_objective((-x + y) / 5, sense = "max")
  expect_equal(
    m$objective$objective@variables$coef,
    c(-0.2, 0.2)
  )
  expect_equal(
    m$objective$objective@variables$variable,
    c("x", "y")
  )
})

test_that("small to mid sized models should work", {
  skip_on_cran()
  n <- 400
  expect_silent(result <- MILPModel() %>%
    add_variable(x[i], i = 1:n, type = "binary") %>%
    set_objective(sum_expr(x[i], i = 1:n), "max") %>%
    add_constraint(sum_expr(x[i], i = 1:n) == 1))
})

test_that("bug 20160713 #41: quantifiers in constraints in sum_expr", {
  expect_silent(MILPModel() %>%
    add_variable(x[i], i = 1:9) %>%
    add_constraint(sum_expr(x[i], i = 1:3 + y) == 1, y = c(0, 3, 6)))
})

test_that("small to mid sized model should work #2", {
  skip_on_cran()
  n <- 40
  # needs to be vectorized
  coef <- function(i, j) {
    rep.int(length(i), 42L)
  }
  expect_silent(MILPModel() %>%
    add_variable(x[i, j], i = 1:n, j = 1:n) %>%
    add_constraint(sum_expr(coef(i, j) * x[i, j], i = 1:n, j = 1:n) == 1))
})

test_that("bug 20160729: two sum_expr on one side", {
  m <- MILPModel() %>%
    add_variable(x[j], j = 1:4) %>%
    add_constraint(sum_expr(x[j], j = 1:2) - sum_expr(x[j], j = 3:4) == 0)
  expect_equal(
    m$constraints[[1]]$lhs@variables$coef,
    c(1, 1, -1, -1)
  )
  expect_equal(
    m$constraints[[1]]$lhs@variables$col,
    c(1, 2, 3, 4)
  )
})

test_that("solve_model warns about wrong arguments", {
  m <- MILPModel()
  expect_error(solve_model(m, not_a_fun <- 0), regexp = "function")
})

test_that("set_objective_ supports standard eval.", {
  m <- MILPModel()
  m <- add_variable_(m, ~x)
  expect_silent(m <- set_objective_(m, ~x))
})

test_that("can expand a term N * (x - y)", {
  m <- add_variable(MILPModel(), x[i], i = 1:2)
  m <- set_objective_(m, ~ -5 * (x[1] - x[2]))
  expect_equal(m$objective$objective@variables$coef, c(-5, 5))
})

test_that("evaluates terms", {
  m <- add_variable(MILPModel(), x[i], i = 1:2)
  m <- set_objective_(m, ~ 5 * 5)
  expect_equal(m$objective$objective, 25)
})

test_that("SE handles sum_expr well", {
  m <- MILPModel() %>%
    add_variable_(~ x[j], j = 1:4) %>%
    add_constraint_(~ sum_expr(x[j], j = 1:2, j == 1) -
      sum_expr(x[j], j = 3:4) == 0)
  expect_equal(m$constraints[[1]]$lhs@variables$coef, c(1, -1, -1))
  expect_equal(m$constraints[[1]]$lhs@variables$col, c(1, 3, 4))
})

test_that("bug 20161110 #106: Error when indices used in sum_expr(...)
           condition already have values in workspace", {
  i <- 2
  j <- 2
  model <- MILPModel()
  model <- add_variable(model, x[i, j], i = 1:2, j = 1:2, i != j)
  expect_silent(result <- set_objective(
    model,
    sum_expr(x[i, j],
      i = 1:2,
      j = 1:2, i != j
    )
  ))
  expect_silent(result <- add_constraint(
    model,
    sum_expr(x[i, j],
      i = 1:2,
      j = 1:2, i != j
    ) <= 10
  ))
  expect_silent(result <- add_constraint(
    model,
    sum_expr(1 + x[i, j] + x[i, j],
      i = 1:2, j = 1:2,
      i != j
    ) <= 10
  ))
})

test_that("variable sum divided by number", {
  model <- MILPModel() %>%
    add_variable(x[i], i = 1:3) %>%
    add_variable(y[i], i = 1:3) %>%
    add_constraint((1 + x[i] + y[i]) / 5 <= 10, i = 1:3)
  constr <- ompr::extract_constraints(model)
  expected_matrix <- matrix(
    c(
      0.2, 0, 0,
      0, 0.2, 0,
      0, 0, 0.2,
      0.2, 0, 0,
      0, 0.2, 0,
      0, 0, 0.2
    ),
    ncol = 6, nrow = 3
  )
  expect_equivalent(as.matrix(constr$matrix), expected_matrix)
  expect_equal(constr$rhs, rep.int(10, 3) - 0.2)
})

test_that("variable sum + numeric", {
  model <- MILPModel() %>%
    add_variable(x) %>%
    add_constraint(1 + (1 + x) <= 3)
  constr <- ompr::extract_constraints(model)
  expect_equal(constr$rhs, 1)
})

test_that("unary plus for variable sum", {
  model <- MILPModel() %>%
    add_variable(x) %>%
    add_constraint(+(1 + x) <= 3)
  constr <- ompr::extract_constraints(model)
  expect_equal(constr$rhs, 2)
})

test_that("unary minus for variable sum", {
  model <- MILPModel() %>%
    add_variable(x) %>%
    add_constraint(-(1 + x) <= 3)
  constr <- ompr::extract_constraints(model)
  expect_equal(constr$rhs, 4)
})

test_that("variabe sum - varaible sum", {
  model <- MILPModel() %>%
    add_variable(x) %>%
    add_variable(y) %>%
    add_constraint((10 + x) - (5 + y) <= 3)
  constr <- ompr::extract_constraints(model)
  expect_equal(constr$rhs, -2)
})

test_that("numeric - varaible sum", {
  model <- MILPModel() %>%
    add_variable(x) %>%
    add_constraint(10 - (10 + x) <= 3)
  constr <- ompr::extract_constraints(model)
  expect_equal(constr$rhs, 3)
})

test_that("nice warning message if sum_expr selected non existent variable", {
  expect_warning(
    MILPModel() %>%
      add_variable(x[i], i = 1:3, i != 2) %>%
      set_objective(sum_expr(x[i], i = 1:3, i != 1)),
    "variable"
  )
})

test_that("bug 20180408: scalar variable and multiple constraints need to be
           handled differently", {
  model <- MILPModel() %>%
    add_variable(y) %>%
    set_objective(10 * y, sense = "min") %>%
    add_constraint(y[rep.int(1, 5)] <= i, i = 1:5)
  constr <- ompr::extract_constraints(model)
  expected_matrix <- matrix(rep.int(1, 5), ncol = 1)
  expect_equivalent(as.matrix(constr$matrix), expected_matrix)
})

test_that("colwise can be used for all coefficients", {
  model <- MILPModel() %>%
    add_variable(x[i, j], i = 1:3, j = 1:2) %>%
    add_variable(z[i, j], i = 1:3, j = 1:2) %>%
    add_variable(y[i, j, k], i = 1:3, j = 1:2, k = 1:2) %>%
    add_constraint(sum_expr(colwise(1:6) * x[i, j], j = 1:2) == 0, i = 1:3) %>%
    add_constraint(sum_expr(colwise(1:6) * x[i, j], j = 1:2, i = 1:3) == 0) %>%
    add_constraint(sum_expr(colwise(1:12) * y[i, j, k], j = 1:2, k = 1:2) == 0,
      i = 1:3
    ) %>%
    add_constraint(sum_expr(colwise(1:12) * y[i, j, k],
      j = 1:2,
      k = 1:2, i = 1:3
    ) == 0) %>%
    add_constraint(sum_expr(colwise(1:12) * y[i, j, k], j = 1:2) == 0,
      i = 1:3, k = 1:2
    ) %>%
    add_constraint(sum_expr(colwise(1:6) * (x[i, j] + z[i, j]), j = 1:2) == 0,
      i = 1:3
    ) %>%
    add_constraint(sum_expr(colwise(1:12) * y[i, j, k], j = 1:2, i = 1:3) == 0,
      k = 1:2
    )

  extract_coef <- function(i) {
    model$constraints[[i]]$lhs@variables[["coef"]]
  }

  expect_equal(extract_coef(1), 1:6)
  expect_equal(extract_coef(2), 1:6)
  expect_equal(extract_coef(3), 1:12)
  expect_equal(extract_coef(4), 1:12)
  expect_equal(extract_coef(5), 1:12)
  expect_equal(extract_coef(6), c(1:6, 1:6))
  expect_equal(extract_coef(7), 1:12)
})

test_that("colwise example from docs works", {
  model <- MILPModel() %>%
    add_variable(x[i, j], i = 1:2, j = 1:3) %>%
    add_constraint(colwise(1:6) * x[1:2, colwise(1:3)] == 0)
  expect_equal(model$constraints[[1]]$lhs@variables[["coef"]], 1:6)
})
