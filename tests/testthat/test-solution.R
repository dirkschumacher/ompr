context("solution")

test_that("export single var to numeric", {
 model <- MIPModel() %>%
   add_variable(x, ub = 1) %>%
   add_variable(y, ub = 1) %>%
   add_constraint(x + y, "<=", 1) %>%
   set_objective(x + y)
 solution <- new("Solution",
                             status = "optimal",
                             model = model,
                             objective_value = 2,
                             solution = setNames(c(1, 1), c("x", "y")))
 result <- get_solution(solution, x)
 expect_equivalent(result, 1)
})

test_that("export solutions to data.frame if var is indexed", {
 model <- MIPModel() %>%
   add_variable(x[i], i = 1:3, ub = 1) %>%
   set_objective(sum_exp(x[i], i = 1:3))
 solution <- new("Solution",
                             status = "optimal",
                             model = model,
                             objective_value = 3,
                             solution = setNames(c(1, 1, 1),
                                                 c("x[1]", "x[3]", "x[3]")))
 expect_error(get_solution(solution, x))
})

test_that("export solutions to data.frame with index", {
 model <- MIPModel() %>%
   add_variable(x[i], i = 1:3, ub = 1) %>%
   set_objective(sum_exp(x[i], i = 1:3))
 solution <- new("Solution",
                 status = "optimal",
                 model = model,
                 objective_value = 3,
                 solution = setNames(c(1, 1, 1), c("x[1]", "x[2]", "x[3]")))
 result <- get_solution(solution, x[i])
 expect_s3_class(result, "data.frame")
 expect_equivalent(as.numeric(result$i), c(1, 2, 3))
})

test_that("export solutions to data.frame with two indexes", {
  model <- MIPModel() %>%
    add_variable(x[i, j], i = 1:2, j = 1:2, ub = 1)
  solution_vars <- setNames(c(1, 1, 1, 1),
                            c("x[1,1]", "x[1,2]", "x[2,1]", "x[2,2]"))
  solution <- new("Solution",
                  status = "optimal",
                  model = model,
                  objective_value = 3,
                  solution = solution_vars)
  result <- get_solution(solution, x[i, j])
  expect_s3_class(result, "data.frame")
  expect_equivalent(result$variable, c("x", "x", "x", "x"))
  expect_equivalent(result$value, c(1, 1, 1, 1))
  expect_equivalent(as.numeric(result$i), c(1, 1, 2, 2))
  expect_equivalent(as.numeric(result$j), c(1, 2, 1, 2))
})

test_that("export infeasible solutions to data.frame", {
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:3, ub = 1) %>%
    set_objective(sum_exp(x[i], i = 1:3))
  solution <- new("Solution",
                  status = "infeasible",
                  model = model,
                  objective_value = 3,
                  solution = setNames(c(1, 1, 1), c("x[1]", "x[3]", "x[3]")))
  result <- get_solution(solution, x[i])
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})


test_that("export solutions to single value if all indexes bound", {
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:3, ub = 1) %>%
    add_variable(y[i], i = 1:3, ub = 1) %>%
    set_objective(sum_exp(x[i], i = 1:3))
  solution <- new("Solution",
                              status = "optimal",
                              model = model,
                              objective_value = 3,
                              solution = setNames(c(2, 2, 2, 1, 1, 1),
                                                  c("y[1]", "y[3]", "y[3]",
                                                    "x[1]", "x[3]", "x[3]")))
  result <- get_solution(solution, y[1])
  expect_equivalent(result, 2)
})

test_that("export solutions to df in a model with more than one variable", {
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:3, ub = 1) %>%
    add_variable(y[i], i = 1:3, ub = 1) %>%
    set_objective(sum_exp(x[i], i = 1:3))
  solution <- new("Solution",
                  status = "optimal",
                  model = model,
                  objective_value = 3,
                  solution = setNames(c(2, 2, 2, 1, 1, 1),
                                      c("y[1]", "y[3]", "y[3]",
                                        "x[1]", "x[3]", "x[3]")))
  result <- get_solution(solution, y[i])
  expect_equivalent(result$value, c(2, 2, 2))
})

test_that("solution has a nice default output", {
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:3, ub = 1) %>%
    add_variable(y[i], i = 1:3, ub = 1) %>%
    set_objective(sum_exp(x[i], i = 1:3))
  solution <- new("Solution",
                  status = "optimal",
                  model = model,
                  objective_value = 3,
                  solution = setNames(c(2, 2, 2, 1, 1, 1),
                                      c("y[1]", "y[3]", "y[3]",
                                        "x[1]", "x[3]", "x[3]")))
  expect_output(show(solution), "Status: optimal\n")
  expect_output(show(solution), "Objective value: 3")
})
