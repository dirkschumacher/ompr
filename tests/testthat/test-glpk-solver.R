context("glpk")

test_that("glpk correctly flags an unbounded problem", {
  result <- add_variable(new("Model"), x, type = "continuous") %>%
    set_objective(x, direction = "max") %>%
    solve_model(configure_glpk_solver())
  expect_equal(result@status, "infeasible")
})

test_that("glpk correctly flags an infeasible problem", {
  result <- add_variable(new("Model"), x, type = "continuous", lb = 10) %>%
    set_objective(x, direction = "max") %>%
    add_constraint(x, "<=", 3) %>%
    solve_model(configure_glpk_solver())
  expect_equal(result@status, "infeasible")
})

test_that("glpk interprets obj. max direction correctly", {
  result <- add_variable(new("Model"), x, type = "continuous", ub = 10) %>%
    set_objective(x, direction = "max") %>%
    add_constraint(x, "<=", 80) %>%
    solve_model(configure_glpk_solver())
  expect_equal(result@objective_value, 10)
  expect_equal(names(result@solution), c("x"))
})

test_that("glpk interprets obj. min direction correctly", {
  result <- add_variable(new("Model"), x, type = "continuous", lb = 10) %>%
    set_objective(x, direction = "min") %>%
    add_constraint(x, ">=", 0) %>%
    solve_model(configure_glpk_solver())
  expect_equal(result@objective_value, 10)
})

test_that("glpk can solve a model", {
  weights <- c(1, 2, 3)
  result <- add_variable(new("Model"), x[i], i = 1:3, type = "binary") %>%
    add_constraint(sum_exp(x[i], i = 1:3), "==", 1) %>%
    set_objective(sum_exp(x[i] * weights[i], i = 1:3) + 5) %>%
    solve_model(configure_glpk_solver())
  expect_equal(result@objective_value, 8)
  expect_equal(names(result@solution), c("x[1]", "x[2]", "x[3]"))
})

test_that("glpk can solve a bin packing problem", {
  max_bins <- 5
  bin_size <- 3
  n <- 5
  weights <- rep.int(1, n)
  m <- new("Model")
  m <- add_variable(m, y[i], i = 1:max_bins, type = "binary")
  m <- add_variable(m, x[i, j], i = 1:max_bins, j = 1:n, type = "binary")
  m <- set_objective(m, sum_exp(y[i], i = 1:max_bins), "min")
  for(i in 1:max_bins) {
    m <- add_constraint(m, sum_exp(weights[j] * x[i, j], j = 1:n), "<=", y[i] * bin_size)
  }
  for(j in 1:n) {
    m <- add_constraint(m, sum_exp(x[i, j], i = 1:max_bins), "==", 1)
  }
  result <- solve_model(m, configure_glpk_solver())
  expect_equal(result@objective_value, 2)
})

test_that("quantified constraints work", {
  max_bins <- 5
  bin_size <- 3
  n <- 5
  weights <- rep.int(1, n)
  m <- new("Model")
  m <- add_variable(m, y[i], i = 1:max_bins, type = "binary")
  m <- add_variable(m, x[i, j], i = 1:max_bins, j = 1:n, type = "binary")
  m <- set_objective(m, sum_exp(y[i], i = 1:max_bins), direction = "min")
  m <- add_constraint(m, sum_exp(weights[j] * x[i, j], j = 1:n), "<=", y[i] * bin_size, i = 1:max_bins)
  m <- add_constraint(m, sum_exp(x[i, j], i = 1:max_bins), "==", 1, j = 1:n)
  result <- solve_model(m, configure_glpk_solver())
  expect_equal(result@objective_value, 2)
})

test_that("glpk has a verbose option", {
  weights <- c(1, 2, 3)
  m <- add_variable(new("Model"), x[i], i = 1:3, type = "binary") %>%
    add_constraint(sum_exp(x[i], i = 1:3), "==", 1) %>%
    set_objective(sum_exp(x[i] * weights[i], i = 1:3) + 5)
  expect_output(solve_model(m, configure_glpk_solver(verbose = TRUE)))
})
