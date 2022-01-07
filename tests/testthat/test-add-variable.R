context("MIP: add_variable")

test_that("only accept valid models", {
  expect_error(add_variable(mtcars, x[i], i = 1:10))
})

test_that("variable must not be a complex expression", {
  expect_error(add_varia.ble(MIPModel(), x[i] + 4, i = 1:10))
})

test_that("global vars do not interfere with variable names in expressions", {
  x <- "hi"
  m <- add_variable(MIPModel(), x[i], i = 1:10, type = "binary")
  expect_false(is.null(m$variables[["x"]]))
  expect_true(is.null(m$variables[["hi"]]))
})

test_that("variables can be added with bound indexes", {
  j <- 3
  m <- add_variable(MIPModel(), x[i, j], i = 1:10, type = "binary")
  expect_false(is.null(m$variables[["x"]]))
})

test_that("one can set bounds on variables", {
  max_bins <- 5
  m <- MIPModel()
  m1 <- add_variable(m, x, type = "continuous", lb = 40)
  m2 <- add_variable(m, x, type = "continuous", ub = 40)
  expect_equal(m1$variable_bounds_lower, 40)
  expect_equal(m2$variable_bounds_upper, 40)
})

test_that("throw error if lower bound > upper bound", {
  expect_error(add_variable(MIPModel(), x, lb = 5, ub = 4))
})

test_that("add_variable_ supports standard eval.", {
  m <- MIPModel()
  expect_silent(add_variable_(m, ~x))
})

test_that("add_variable throws error when lb or ub is of length > 1", {
  m <- MIPModel()
  expect_error(add_variable_(m, ~x, lb = c(5, 2), ub = 10))
  expect_error(add_variable_(m, ~x, lb = 2, ub = c(10, 10)))
})

test_that("add_variable throws error when lb or ub is not numeric", {
  m <- MIPModel()
  expect_error(add_variable_(m, ~x, lb = "5"))
  expect_error(add_variable_(m, ~x, ub = "5"))
})

test_that("add_variable throws error when type is wrong", {
  m <- MIPModel()
  expect_error(add_variable_(m, ~x, lb = 5, type = "wat"))
  expect_error(add_variable_(m, ~x, lb = 5, type = c("integer", "binary")))
})

test_that("add_variable warns if binary var's bound is not 0/1", {
  m <- MIPModel()
  expect_warning({
    x <- add_variable_(m, ~x, lb = 10, type = "binary")
  })
  expect_warning({
    y <- add_variable_(m, ~x, ub = 110, type = "binary")
  })
  expect_equal(x$variable_bounds_lower, 0)
  expect_equal(y$variable_bounds_upper, 1)
})

test_that("variable quantifiers can have filter expressions", {
  result <- add_variable(MIPModel(), x[i, j],
    i = 1:5,
    j = 1:5,
    i == j + 1
  )
  expect_setequal(
    variable_keys(result),
    c("x[2,1]", "x[3,2]", "x[4,3]", "x[5,4]")
  )
})

test_that("filter epxressions work with SE 2", {
  result <- add_variable_(MIPModel(), ~ x[i, j],
    i = 1:5,
    j = 1:5,
    .dots = ~ i == j + 1
  )
  expect_setequal(
    variable_keys(result),
    c("x[2,1]", "x[3,2]", "x[4,3]", "x[5,4]")
  )
})


test_that("add_variable throws error when lb or ub is of length > 1", {
  m <- MIPModel()
  expect_error(add_variable_(m, ~x, lb = c(5, 2), ub = 10))
  expect_error(add_variable_(m, ~x, lb = 2, ub = c(10, 10)))
})

test_that("add_variable throws error when lb or ub is not numeric", {
  m <- MIPModel()
  expect_error(add_variable_(m, ~x, lb = "5"))
  expect_error(add_variable_(m, ~x, ub = "5"))
})

test_that("add_variable throws error when type is wrong", {
  m <- MIPModel()
  expect_error(add_variable_(m, ~x, lb = 5, type = "wat"))
  expect_error(add_variable_(m, ~x, lb = 5, type = c("integer", "binary")))
})

test_that("add_variable warns if binary var's bound is not 0/1", {
  m <- MIPModel()
  expect_warning({
    x <- add_variable_(m, ~x, lb = 10, type = "binary")
  })
  expect_warning({
    y <- add_variable_(m, ~x, ub = 110, type = "binary")
  })
  expect_equal(x$variable_bounds_lower, 0)
  expect_equal(y$variable_bounds_upper, 1)
})

test_that("bug 20161014: division with number in rhs", {
  m <- MIPModel() %>%
    add_variable(x) %>%
    add_constraint(x / 5 <= 1)
  expect_equal(m$constraints[[1]]@lhs@terms[[1]]@coefficient, 1 / 5)
})
