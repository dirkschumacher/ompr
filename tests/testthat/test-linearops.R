context("linear ops")

new_arity2_var <- function(var_name) {
  d <- expand.grid(variable = var_name, V1 = 1:10, V2 = 1:10)
  d[["col"]] <- seq_len(nrow(d))
  index_map <- function(x) {
    d[d$variable == x, ]
  }
  new("LinearVariableCollection", index_mapping = index_map)
}

new_arity3_var <- function(var_name) {
  d <- expand.grid(variable = var_name, V1 = 1:3, V2 = 1:3, V3 = 1:3)
  d[["col"]] <- seq_len(nrow(d))
  index_map <- function(x) {
    d[d$variable == x, ]
  }
  new("LinearVariableCollection", index_mapping = index_map)
}


x <- new_arity2_var("x")
y <- new_arity3_var("y")

test_that("expanding rows", {
  res <- x[1:3, 1:3]
  expect_equal(nrow(res@variables), 3)
  expect_equal(res@variables$coef, rep.int(1, 3))
})

test_that("adding constants", {
  res <- x[1:3, 1:3] + 1
  expect_equal(res@constant$constant, rep.int(1, 3))
  expect_equal(res@constant$row, 1:3)

  res <- x[1:3, 1:3] + 1:3
  expect_equal(res@constant$constant, 1:3)
  expect_equal(res@constant$row, 1:3)

  expect_error(x[1:3, 1:3] + 1:4)

  res <- (x[1:3, 1:3] + 1:3) + 1:3
  expect_equal(res@constant$constant, 1:3 + 1:3)
  expect_equal(res@constant$row, 1:3)
})

test_that("substracting length 1 vectors", {
  res <- x[1:3, 1:3] + x[4, 4]
  expect_equal(res@variables$row, c(1, 1, 2, 2, 3, 3))

  res <- x[4, 4] + x[1:3, 1:3]
  expect_equal(res@variables$row, c(1, 1, 2, 2, 3, 3))

  res <- (x[1:3, 1:3] + 4) + x[4, 4]
  expect_equal(res@variables@variables$row, c(1, 1, 2, 2, 3, 3))
})

test_that("multiplying constants", {
  res <- (x[1:3, 1:3] + 1) * 2
  expect_equal(res@constant$constant, rep.int(2, 3))
  expect_equal(res@constant$row, 1:3)

  res <- (x[1:3, 1:3] + 1:3) * 1:3
  expect_equal(res@constant$constant, 1:3 * 1:3)
  expect_equal(res@constant$row, 1:3)
})

test_that("adding two linear sums", {
  res <- (x[1:3, 1:3] + 1) + (x[1:3, 1:3] + 1:3)
  expect_equal(res@constant$constant, 1:3 + 1)
  expect_equal(res@constant$row, 1:3)
  expect_equal(res@variables@variables$coef, rep.int(2, 3))
  expect_equal(res@variables@variables$row, 1:3)
})

test_that("index colwise", {
  # x[1, 1]
  # x[1, 1] + x[1, 2]
  # x[1, 1] + x[1, 2] + x[1, 3]
  res <- x[1, colwise(1, 1:2, 1:3)]
  expect_equal(nrow(res@variables[res@variables$row == 1, ]), 1)
  expect_equal(nrow(res@variables[res@variables$row == 2, ]), 2)
  expect_equal(nrow(res@variables[res@variables$row == 3, ]), 3)

  res <- x[1, as_colwise(list(1, 1:2, 1:3))]
  expect_equal(nrow(res@variables[res@variables$row == 1, ]), 1)
  expect_equal(nrow(res@variables[res@variables$row == 2, ]), 2)
  expect_equal(nrow(res@variables[res@variables$row == 3, ]), 3)

  res <- x[1:3, as_colwise(1:3)]
  expect_equal(nrow(res@variables[res@variables$row == 1, ]), 3)
  expect_equal(nrow(res@variables[res@variables$row == 2, ]), 3)
  expect_equal(nrow(res@variables[res@variables$row == 3, ]), 3)
})

test_that("index arity 3", {
  # y[1, 1, 1]
  # y[1, 1, 2] + y[1, 2, 2]
  # y[1, 1, 3] + y[1, 2, 3] + y[1, 2, 3]
  res <- y[colwise(1, 1:2, 1:3), colwise(1), 1:3]
  expect_equal(nrow(res@variables[res@variables$row == 1, ]), 1)
  expect_equal(nrow(res@variables[res@variables$row == 2, ]), 2)
  expect_equal(nrow(res@variables[res@variables$row == 3, ]), 3)
})
