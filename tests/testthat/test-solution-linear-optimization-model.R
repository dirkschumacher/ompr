context("MIP: solution")

test_that("export single var to numeric", {
  model <- MIPModel() %>%
    add_variable(x, ub = 1) %>%
    add_variable(y, ub = 1) %>%
    add_constraint(x + y <= 1) %>%
    set_objective(x + y)
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 2,
    solution = setNames(c(1, 1), c("x", "y"))
  )
  result <- get_solution(solution, x)
  expect_equivalent(result, 1)
})

test_that("export solutions to data.frame if var is indexed", {
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:3, ub = 1) %>%
    set_objective(sum_expr(x[i], i = 1:3))
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = setNames(
      c(1, 1, 1),
      c("x[1]", "x[3]", "x[3]")
    )
  )
  expect_error(get_solution(solution, x))
})

test_that("export solutions to data.frame with index", {
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:3, ub = 1) %>%
    set_objective(sum_expr(x[i], i = 1:3))
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = setNames(c(1, 1, 1), c("x[1]", "x[2]", "x[3]"))
  )
  result <- get_solution(solution, x[i])
  expect_s3_class(result, "data.frame")
  expect_equivalent(as.numeric(result$i), c(1, 2, 3))
})

test_that("export solutions to data.frame with two indexes", {
  model <- MIPModel() %>%
    add_variable(x[i, j], i = 1:2, j = 1:2, ub = 1)
  solution_vars <- setNames(
    c(1, 1, 1, 1),
    c("x[1,1]", "x[1,2]", "x[2,1]", "x[2,2]")
  )
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = solution_vars
  )
  result <- get_solution(solution, x[i, j])
  expect_s3_class(result, "data.frame")
  expect_equivalent(result$variable, c("x", "x", "x", "x"))
  expect_equivalent(result$value, c(1, 1, 1, 1))
  expect_equivalent(as.numeric(result$j), c(1, 1, 2, 2))
  expect_equivalent(as.numeric(result$i), c(1, 2, 1, 2))
})

test_that("export infeasible solutions to data.frame", {
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:3, ub = 1) %>%
    set_objective(sum_expr(x[i], i = 1:3))
  solution <- new_solution(
    status = "infeasible",
    model = model,
    objective_value = 3,
    solution = setNames(c(1, 1, 1), c("x[1]", "x[3]", "x[3]"))
  )
  result <- get_solution(solution, x[i])
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})


test_that("export solutions to single value if all indexes bound", {
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:3, ub = 1) %>%
    add_variable(y[i], i = 1:3, ub = 1) %>%
    set_objective(sum_expr(x[i], i = 1:3))
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = setNames(
      c(2, 2, 2, 1, 1, 1),
      c(
        "y[1]", "y[3]", "y[3]",
        "x[1]", "x[3]", "x[3]"
      )
    )
  )
  result <- get_solution(solution, y[1])
  expect_equivalent(result, 2)
})

test_that("export solutions to df in a model with more than one variable", {
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:3, ub = 1) %>%
    add_variable(y[i], i = 1:3, ub = 1) %>%
    set_objective(sum_expr(x[i], i = 1:3))
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = setNames(
      c(2, 2, 2, 1, 1, 1),
      c(
        "y[1]", "y[3]", "y[3]",
        "x[1]", "x[3]", "x[3]"
      )
    )
  )
  result <- get_solution(solution, y[i])
  expect_equivalent(result$value, c(2, 2, 2))
})

test_that("solution has a nice default output", {
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:3, ub = 1) %>%
    add_variable(y[i], i = 1:3, ub = 1) %>%
    set_objective(sum_expr(x[i], i = 1:3))
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = setNames(
      c(2, 2, 2, 1, 1, 1),
      c(
        "y[1]", "y[3]", "y[3]",
        "x[1]", "x[3]", "x[3]"
      )
    )
  )
  expect_output(show(solution), "Status: optimal\n")
  expect_output(show(solution), "Objective value: 3")
})

test_that("solution indexes should not be factors", {
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:3, ub = 1) %>%
    add_variable(y[i], i = 1:3, ub = 1) %>%
    set_objective(sum_expr(x[i], i = 1:3))
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = setNames(
      c(2, 2, 2, 1, 1, 1),
      c(
        "y[1]", "y[2]", "y[3]",
        "x[1]", "x[2]", "x[3]"
      )
    )
  )
  expect_equal(class(get_solution(solution, y[i])$i), "integer")
})

test_that("bug 20160908: solution indexes mixed up", {
  model <- MIPModel() %>%
    add_variable(x[i, j], i = 10:11, j = 10:12, ub = 1) %>%
    set_objective(sum_expr(x[10, i], i = 10:12))
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = setNames(
      c(2, 2, 2, 1, 1, 1),
      c(
        "x[10,10]", "x[10,11]", "x[10,12]",
        "x[11,10]", "x[11,11]", "x[11,12]"
      )
    )
  )
  sol <- get_solution(solution, x[i, j])
  expect_equal(sol$i, c(10, 11, 10, 11, 10, 11))
})

test_that("objective_value gets the obj. value", {
  model <- MIPModel() %>%
    add_variable(x[i, j], i = 10:11, j = 10:12, ub = 1) %>%
    set_objective(sum_expr(x[10, i], i = 10:12))
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = setNames(
      c(2, 2, 2, 1, 1, 1),
      c(
        "x[10,10]", "x[10,11]", "x[10,12]",
        "x[11,10]", "x[11,11]", "x[11,12]"
      )
    )
  )
  expect_equal(3, objective_value(solution))
})

test_that("solver_status gets the solver_status", {
  model <- MIPModel() %>%
    add_variable(x[i, j], i = 10:11, j = 10:12, ub = 1) %>%
    set_objective(sum_expr(x[10, i], i = 10:12))
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = setNames(
      c(2, 2, 2, 1, 1, 1),
      c(
        "x[10,10]", "x[10,11]", "x[10,12]",
        "x[11,10]", "x[11,11]", "x[11,12]"
      )
    )
  )
  expect_equal("optimal", solver_status(solution))
})

test_that("get_solution_ is supported (though deprecated)", {
  model <- MIPModel() %>%
    add_variable(x[i, j], i = 10:11, j = 10:12, ub = 1) %>%
    set_objective(sum_expr(x[10, i], i = 10:12))
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = setNames(
      c(2, 2, 2, 1, 1, 1),
      c(
        "x[10,10]", "x[10,11]", "x[10,12]",
        "x[11,10]", "x[11,11]", "x[11,12]"
      )
    )
  )
  res <- get_solution_(solution, ~ x[i, j])
  expect_equal(res$value, c(2, 1, 2, 1, 2, 1))
  expect_equal(res$i, c(10, 11, 10, 11, 10, 11))
  expect_equal(res$j, c(10, 10, 11, 11, 12, 12))
})

test_that("get_solution works with character indexes", {
  model <- MIPModel() %>%
    add_variable(x[letter], letter = letters, ub = 1) %>%
    set_objective(sum_expr(x[letter], letter = letters))
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = length(letters),
    solution = setNames(
      rep.int(1, length(letters)),
      paste0("x[", letters, "]")
    )
  )
  res <- get_solution(solution, x[letter])
  res <- res[order(res$letter), ]
  expect_equal(res$value, rep.int(1, length(letters)))
  expect_equal(res$letter, letters)
})

test_that("row and column duals", {
  model <- MIPModel() %>%
    add_variable(x[letter], letter = letters, ub = 1) %>%
    set_objective(sum_expr(x[letter], letter = letters)) %>%
    add_constraint(x[i] == 1, i = letters)
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = length(letters),
    solution = setNames(
      rep.int(1, length(letters)),
      paste0("x[", letters, "]")
    ),
    solution_column_duals <- function() {
      setNames(
        rep.int(42, length(letters)),
        paste0("x[", letters, "]")
      )
    },
    solution_row_duals <- function() rep.int(48, length(letters))
  )
  expect_equal(
    get_column_duals(solution),
    setNames(
      rep.int(42, length(letters)),
      paste0("x[", letters, "]")
    )
  )
  expect_equal(
    get_row_duals(solution),
    rep.int(48, length(letters))
  )
  expect_equal(
    get_solution(solution, x[letter], type = "dual")$value,
    rep.int(42, length(letters))
  )
})

test_that("get_solution errors when variable not found", {
  model <- MIPModel() %>%
    add_variable(y[i], i = 1, ub = 1) %>%
    set_objective(sum_expr(y[i], i = 1))
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = setNames(
      1,
      c("y[1]")
    )
  )
  expect_error(get_solution(solution, z[1]), "Variable")
})

test_that("get_solution errors when solution is not a vector or has NAs", {
  model <- MIPModel() %>%
    add_variable(y[i], i = 1, ub = 1) %>%
    set_objective(sum_expr(y[i], i = 1))
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = NULL
  )
  expect_error(get_solution(solution, y[1]), "solver")
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = setNames(
      NA_real_,
      c("y[1]")
    )
  )
  expect_error(get_solution(solution, y[1]), "solver")
})


test_that("get_solution orders by indexes", {
  model <- MIPModel() %>%
    add_variable(x[i, j], i = 10:11, j = 10:12, ub = 1) %>%
    set_objective(sum_expr(x[10, i], i = 10:12))
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = setNames(
      c(2, 2, 2, 1, 1, 1),
      c(
        "x[11,10]", "x[11,11]", "x[11,12]",
        "x[10,10]", "x[10,11]", "x[10,12]"
      )
    )
  )
  res <- get_solution(solution, x[i, j])
  expect_equal(res$value, c(1, 2, 1, 2, 1, 2))
  expect_equal(get_solution(solution, x[10, j])$value, c(1, 1, 1))
  expect_equal(get_solution(solution, x[i, 11])$value, c(1, 2))
})

test_that("get_solution signals a proper error if fixed indexes are not found", {
  model <- MIPModel() %>%
    add_variable(x[i, j], i = 10:11, j = 10:12, ub = 1) %>%
    set_objective(sum_expr(x[10, i], i = 10:12))
  solution <- new_solution(
    status = "optimal",
    model = model,
    objective_value = 3,
    solution = setNames(
      c(2, 2, 2, 1, 1, 1),
      c(
        "x[11,10]", "x[11,11]", "x[11,12]",
        "x[10,10]", "x[10,11]", "x[10,12]"
      )
    )
  )
  expect_error(
    get_solution(solution, x[i, 20]),
    "found"
  )
})
