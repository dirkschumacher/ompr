context("MIP: model-api")

describe("nvars()", {
  it("returns the number of variables", {
    model <- MIPModel() %>%
      add_variable(x[i], i = 1:10, type = "binary") %>%
      add_variable(y[i], i = 1:5, type = "continuous") %>%
      add_variable(z[i], i = 1:2, type = "integer")
    result <- nvars(model)
    expect_equivalent(10, result$binary)
    expect_equivalent(5, result$continuous)
    expect_equivalent(2, result$integer)
    expect_null(names(result$continuous))
    expect_null(names(result$binary))
    expect_null(names(result$integer))
  })
  it("returns 0 for a model without variables", {
    model <- MIPModel()
    result <- nvars(model)
    expect_equivalent(0, result$binary)
    expect_equivalent(0, result$continuous)
    expect_equivalent(0, result$integer)
  })
})

describe("nconstraints()", {
  it("returns the number of rows (constraints) of a model", {
    m <- MIPModel() %>%
      add_variable(x[i], i = 1:5) %>%
      add_constraint(x[i] <= 10, i = 1:5) %>%
      add_constraint(x[i] >= 5, i = 1:5)
    expect_equal(nconstraints(m), 10)
  })
})

describe("objective_function()", {
  it("returns a list with a vector and a constant by default", {
    model <- MIPModel() %>%
      add_variable(x[i], i = 1:9) %>%
      set_objective(sum_expr(i * x[i], i = 1:9) + 10)
    result <- objective_function(model)
    expect_equal(c(1:9), as.vector(result$solution))
    expect_equal(10, result$constant)
  })
  it("returns handles models without objective function", {
    model <- MIPModel() %>%
      add_variable(x[i], i = 1:10)
    result <- objective_function(model)
    expect_equal(rep.int(0, 10), as.vector(result$solution))
    expect_equal(0, result$constant)
  })
  it("returns a sparse vector", {
    n <- 2
    model <- MIPModel() %>%
      add_variable(x[i, j],
        i = 1:n, j = 1:n,
        type = "integer", lb = 0, ub = 1
      ) %>%
      set_bounds(x[i, j], i = 1:n, j = 1:n, lb = 0, ub = 0) %>%
      set_objective(sum_expr(x[i, j], i = 1:n, j = 1:n)) %>%
      add_constraint(sum_expr(x[i, j], i = 1:n, j = 1:n) <= 10)
    result <- objective_function(model)
    expected <- Matrix::sparseVector(rep.int(1, n^2), seq_len(n^2), n^2)
    expect_s4_class(result$solution, "dsparseVector")
    expect_equal(result$solution@length, expected@length)
    expect_setequal(result$solution@x, expected@x)
    expect_setequal(result$solution@i, expected@i)
  })
})

describe("variable_keys()", {
  it("returns a vector of unique variable keys", {
    model <- MIPModel() %>%
      add_variable(x[i], i = 1:3)
    result <- variable_keys(model)
    expect_equal(c("x[1]", "x[2]", "x[3]"), result)
  })
  it("works with more than 1 index var", {
    model <- MIPModel() %>%
      add_variable(x[i, j], i = 1:2, j = 1:2)
    result <- variable_keys(model)
    expect_setequal(c("x[1,1]", "x[2,1]", "x[1,2]", "x[2,2]"), result)
  })
  it("works with vars without an index", {
    model <- MIPModel() %>%
      add_variable(x[i, j], i = 1, j = 1) %>%
      add_variable(a)
    result <- variable_keys(model)
    expect_setequal(c("a", "x[1,1]"), result)
  })
  it("sorts keys alphabetically", {
    model <- MIPModel() %>%
      add_variable(x[i], i = 1:3) %>%
      add_variable(y[i], i = 1:3)
    result <- variable_keys(model)
    expect_setequal(sort(result), result)
  })
  it("returns an empty character vector if model has no vars", {
    expect_equal(character(0), variable_keys(MIPModel()))
  })
})

describe("extract_constraints()", {
  it("returns a list of named elements", {
    model <- MIPModel() %>%
      add_variable(x[i], i = 1:3) %>%
      add_variable(y[i], i = 1:3) %>%
      add_constraint(x[i] + y[i] <= 1, i = 1:3)
    result <- extract_constraints(model)
    expect_true(is.list(result))
    expect_true(all(c("matrix", "rhs", "sense") %in% names(result)))
  })
  it("returns the constraint matrix as a matrix", {
    model <- MIPModel() %>%
      add_variable(x[i], i = 1:3) %>%
      add_variable(y[i], i = 1:3) %>%
      add_constraint(x[i] + y[i] <= 1, i = 1:3)
    result <- extract_constraints(model)
    exp_matrix <- matrix(c(
      1, 0, 0, 0, 1, 0, 0, 0, 1,
      1, 0, 0, 0, 1, 0, 0, 0, 1
    ), ncol = 6, nrow = 3)
    expect_equivalent(exp_matrix, as.matrix(result$matrix))
  })
  it("returns the constraint right hand side", {
    model <- MIPModel() %>%
      add_variable(x[i], i = 1:3) %>%
      add_variable(y[i], i = 1:3) %>%
      add_constraint(x[i] + y[i] <= 1, i = 1:3)
    result <- extract_constraints(model)
    expect_equal(c(1, 1, 1), result$rhs)
  })
  it("returns the constraint sense", {
    model <- MIPModel() %>%
      add_variable(x[i], i = 1:3) %>%
      add_variable(y[i], i = 1:3) %>%
      add_constraint(x[i] + y[i] <= 1, i = 1:3)
    result <- extract_constraints(model)
    expect_equal(c("<=", "<=", "<="), result$sense)
  })
  it("returns a sparse Matrix with column-oriented encoding", {
    model <- MIPModel() %>%
      add_variable(x[i], i = 1:3) %>%
      add_variable(y[i], i = 1:3) %>%
      add_constraint(x[i] + y[i] <= 1, i = 1:3)
    result <- extract_constraints(model)
    expect_s4_class(result$matrix, "dgCMatrix")
  })
  it("works with non indexed variables", {
    model <- MIPModel() %>%
      add_variable(x) %>%
      add_constraint(x <= 1)
    result <- extract_constraints(model)
    expect_equivalent(matrix(1, nrow = 1, ncol = 1), as.matrix(result$matrix))
  })
  it("supports underscores in variables", {
    # bug #115 20170217
    model <- MIPModel() %>%
      add_variable(x_a[i], i = 1:3) %>%
      set_objective(sum_expr(x_a[i], i = 1:3)) %>%
      add_constraint(x_a[1] == 1)
    expect_equal(1, sum(extract_constraints(model)$matrix))
  })
  it("returns an empy matrix when no constraints are present", {
    model <- MIPModel() %>%
      add_variable(x_a[i], i = 1:3)
    res <- extract_constraints(model)
    expect_equal(ncol(res$matrix), 3)
    expect_equal(nrow(res$matrix), 0)
    expect_equal(res$sense, character(0))
    expect_equal(res$rhs, numeric(0))
  })
})

describe("variable_types()", {
  it("returns the variable types in the correct order", {
    model <- MIPModel() %>%
      add_variable(x, type = "binary") %>%
      add_variable(y, type = "continuous") %>%
      add_variable(z, type = "integer")
    result <- variable_types(model)
    expect_equal(factor(c("binary", "continuous", "integer")), result)
  })
  it("returns the variable types in the correct order for index variables", {
    model <- MIPModel() %>%
      add_variable(x[i], type = "integer", i = 1:2) %>%
      add_variable(a[i], type = "binary", i = 1:2)
    expected <- factor(c("binary", "binary", "integer", "integer"))
    expect_setequal(expected, variable_types(model))
  })
  it("returns an empty vector if model has no variables", {
    expect_equal(
      factor(levels = c("binary", "continuous", "integer")),
      variable_types(MIPModel())
    )
  })
})

describe("variable_bounds()", {
  it("returns a list with the correct variable bounds", {
    model <- MIPModel() %>%
      add_variable(x, type = "binary") %>%
      add_variable(y, type = "continuous", lb = 2) %>%
      add_variable(z, type = "integer", ub = 3)
    result <- variable_bounds(model)
    expected <- list(
      lower = c(0, 2, -Inf),
      upper = c(1, Inf, 3)
    )
    expect_equal(expected, result)
  })
  it("works with indexed variables", {
    model <- MIPModel() %>%
      add_variable(x[i], i = 1:3, lb = 1, ub = 3, type = "integer")
    result <- variable_bounds(model)
    expected <- list(
      lower = c(1, 1, 1),
      upper = c(3, 3, 3)
    )
    expect_equal(expected, result)
  })
  it("returns a list with empty numerics of model has no variables", {
    expect_equal(list(
      lower = numeric(0),
      upper = numeric(0)
    ), variable_bounds(MIPModel()))
  })
  it("returns the bounds in the order of the constraint matrix", {
    n <- 2
    model <- MIPModel() %>%
      add_variable(x[i, j], i = 1:n, j = 1:n, type = "binary") %>%
      add_variable(u[i], i = 1:n, lb = 1, ub = n) %>%
      set_objective(0) %>%
      add_constraint(u[i] + 1 <= u[j] + n * (1 - x[i, j]), i = 1:n, j = 1:n)
    result <- variable_bounds(model)
    expect_equal(c(0, 0, 0, 0, 1, 1), result$lower)
    expect_equal(c(1, 1, 1, 1, 2, 2), result$upper)
  })
})

test_that("bug 20170312: variable_keys has wrong orderning", {
  model <- MIPModel() %>%
    add_variable(x[i, j],
      i = 1:2, j = 1:3, type = "integer",
      lb = 0, ub = 5
    ) %>%
    set_bounds(x[i, i], i = 1:2, lb = 1, ub = 1)
  result <- variable_bounds(model)
  expect_equal(c(1, 0, 0, 0, 1, 0), result$lower)
  expect_equal(c(1, 5, 5, 5, 1, 5), result$upper)
})

test_that("bug github#265: expression builder bug with coefficients after var", {
  a <- b <- 1
  m <- MIPModel() %>%
    add_variable(x) %>%
    set_objective(x * a * b) %>%
    add_constraint(x <= 1)
  expect_equal(as.numeric(objective_function(m)$solution), 1)
})

test_that("bug github#327: Using index 'e' fails in sum_expr", {
  expect_silent({
    MIPModel() %>%
      add_variable(x[e], e = 1:3) %>%
      set_objective(sum_expr(x[e], e = 1:3))
  })
})

test_that("bug github#266: reverting the index names fails", {
  expect_silent({
    m <- MIPModel() %>%
      add_variable(x[i, j], j = 1:3, i = 1:2) %>%
      add_constraint(x[i, j] == 1, i = 1:2, j = 1:3)
  })
  expect_setequal(
    variable_keys(m),
    c(
      "x[1,1]", "x[1,2]", "x[1,3]",
      "x[2,1]", "x[2,2]", "x[2,3]"
    )
  )
})

test_that("extract_constraints does not contain explicit 0 values", {
  m <- MIPModel() %>%
    add_variable(x) %>%
    add_constraint(0 * x <= 1)
  res <- extract_constraints(m)
  expect_equal(
    res$matrix@x,
    numeric(0L)
  )
})
