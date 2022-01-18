test_that("multiplying linear terms", {
  x <- new_linear_term(variable = new_linear_variable(1), coefficient = 1)
  x2 <- x * 2
  expect_equal(x2@coefficient, 2)
  x2 <- 2 * x
  expect_equal(x2@coefficient, 2)
})

test_that("dividing linear terms by a numeric", {
  x <- new_linear_term(variable = new_linear_variable(1), coefficient = 1)
  x2 <- x / 2
  expect_equal(x2@coefficient, 1 / 2)
})

test_that("unary +/- for linear terms", {
  x <- new_linear_term(variable = new_linear_variable(1), coefficient = 1)
  expect_equal(+x, x)
  x2 <- -x
  expect_equal(x2@coefficient, -1)
})

test_that("addition creates linear functions", {
  x <- new_linear_term(variable = new_linear_variable(1), coefficient = 1)
  x2 <- x + 2
  expect_equal(x2@constant, 2)
  expect_s4_class(x2, "LinearFunction")
  x2 <- 2 + x
  expect_equal(x2@constant, 2)
  expect_s4_class(x2, "LinearFunction")
})

test_that("adding and substracting two linear terms", {
  x <- new_linear_term(variable = new_linear_variable(1), coefficient = 1)
  y <- new_linear_term(variable = new_linear_variable(2), coefficient = 5)
  res <- x + y
  expect_s4_class(res, "LinearFunction")
  expect_equal(length(res@terms), 2)
  expect_equal(res@constant, 0)
})

test_that("adding a constant to a linear functions", {
  x <- new_linear_term(variable = new_linear_variable(1), coefficient = 1)
  x2 <- (x + 2) + 2
  expect_equal(x2@constant, 4)
  expect_s4_class(x2, "LinearFunction")
  x2 <- 2 + (2 + x)
  expect_equal(x2@constant, 4)
  expect_s4_class(x2, "LinearFunction")
})

test_that("substracting a constant to a linear functions", {
  x <- new_linear_term(variable = new_linear_variable(1), coefficient = 1)
  x2 <- (x + 2) - 2
  expect_equal(x2@constant, 0)
  expect_s4_class(x2, "LinearFunction")
  x2 <- 2 - (2 + x)
  expect_equal(x2@constant, 0)
  expect_s4_class(x2, "LinearFunction")
})

test_that("multiplying a linear function by a function", {
  x <- new_linear_term(variable = new_linear_variable(1), coefficient = 1) + 1
  x2 <- x * 10
  expect_equal(x2@constant, 10)
  expect_equal(x2@terms[[1]]@coefficient, 10)
  x2 <- 10 * x
  expect_equal(x2@constant, 10)
  expect_equal(x2@terms[[1]]@coefficient, 10)
})

test_that("dividing a linear function by a function", {
  x <- new_linear_term(variable = new_linear_variable(1), coefficient = 1) + 1
  x2 <- x / 2
  expect_equal(x2@constant, 1 / 2)
  expect_equal(x2@terms[[1]]@coefficient, 1 / 2)
})

test_that("adding a linear function and linear terms", {
  x <- new_linear_term(variable = new_linear_variable(1), coefficient = 1) + 1
  y <- new_linear_term(variable = new_linear_variable(1), coefficient = 1)
  z <- new_linear_term(variable = new_linear_variable(2), coefficient = 1)
  res <- (x + y) + z
  expect_equal(length(res@terms), 3)
})

test_that("substracting a linear function and linear terms", {
  x <- new_linear_term(variable = new_linear_variable(1), coefficient = 1) + 1
  y <- new_linear_term(variable = new_linear_variable(1), coefficient = 1)
  z <- new_linear_term(variable = new_linear_variable(2), coefficient = 42)
  res <- (x - y) - z
  expect_equal(length(res@terms), 3)
  expect_equal(res@constant, 1)
  expect_setequal(
    vapply(res@terms, function(x) x@coefficient, numeric(1)),
    c(1, -1, -42)
  )
  res <- z - (x - y)
  expect_equal(length(res@terms), 3)
  expect_equal(res@constant, -1)
  expect_setequal(
    vapply(res@terms, function(x) x@coefficient, numeric(1)),
    c(1, -1, 42)
  )
})
