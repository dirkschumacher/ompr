context("MIP: add_variable")

test_that("only accept valid models", {
  expect_error(add_variable(mtcars, x[i], i = 1:10))
})

test_that("subscripts must be present in variable expression", {
  expect_error(add_variable(MIPModel(), x[i], j = 1:10))
})
test_that("variable must not be a complex expression", {
  expect_error(add_variable(MIPModel(), x[i] + 4, i = 1:10))
})

test_that("variables with arity 0 can be added", {
  m <- MIPModel()
  m_new <- add_variable(m, x, type = "binary")
  expect_false(is.null(m_new$variables[["x"]]))
  expect_equal(m_new$variables[["x"]]$type, "binary")
  expect_equal(m_new$variables[["x"]]$arity, 0)
})

test_that("variables can be added", {
  m <- add_variable(MIPModel(), x[i], i = 1:10, type = "binary")
  expect_false(is.null(m$variables[["x"]]))
  expect_equal(m$variables[["x"]]$type, "binary")
  expect_equal(m$variables[["x"]]$arity, 1)
})

test_that("variables bounds get replicated for var. groups", {
  m <- add_variable(MIPModel(), x[i],
    i = 1:3,
    lb = 0, ub = 1,
    type = "binary"
  )
  expect_false(is.null(m$variables[["x"]]))
  expect_equal(m$variables[["x"]]$lb, c(0, 0, 0))
  expect_equal(m$variables[["x"]]$ub, c(1, 1, 1))
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
  expect_equal(m$variables[["x"]]$arity, 2)
})

test_that("binding variables works with magrittr", {
  y <- 1
  m <- add_variable(MIPModel(), x[i], i = 1:3, type = "binary") %>%
    set_objective(x[1] * y)
  expect_equal(deparse(m$objective$expression[[1]]), "x[1] * 1")
})

test_that("one can set bounds on variables", {
  max_bins <- 5
  m <- MIPModel()
  m1 <- add_variable(m, x, type = "continuous", lb = 40)
  m2 <- add_variable(m, x, type = "continuous", ub = 40)
  expect_equal(m1$variables[["x"]]$lb, 40)
  expect_equal(m2$variables[["x"]]$ub, 40)
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
  expect_equal(0, x$variables[[1]]$lb)
  expect_equal(1, x$variables[[1]]$ub)
})

test_that("variable quantifiers can have filter expressions", {
  result <- add_variable(MIPModel(), x[i, j],
    i = 1:5,
    j = 1:5,
    i == j + 1
  )
  expect_equal(
    c("2_1", "3_2", "4_3", "5_4"),
    result$variables[["x"]]$instances
  )
})

test_that("add_variable fails if filter expressions filter out everything", {
  expect_error(add_variable(MIPModel(), x[i], i = 1:3, i > 10))
})

test_that("filter epxressions work with SE 2", {
  result <- add_variable_(MIPModel(), ~ x[i, j],
    i = 1:5,
    j = 1:5,
    .dots = ~ i == j + 1
  )
  expect_equal(
    c("2_1", "3_2", "4_3", "5_4"),
    result$variables[["x"]]$instances
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
  expect_equal(0, x$variables[[1]]$lb)
  expect_equal(1, x$variables[[1]]$ub)
})

test_that("bug 20161014: division with number in rhs", {
  m <- MIPModel() %>%
    add_variable(x) %>%
    add_constraint(x / 5 <= 1)
  expect_equal("0.2 * x", as.character(m$constraints[[1]]$lhs))
})
