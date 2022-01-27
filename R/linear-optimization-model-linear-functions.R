new_linear_variable <- function(column_idx) {
  structure(list(column_idx = column_idx), class = "OmprLinearVariable")
}

new_variable_collection <- function(map = map) {
  structure(
    list(map = map), class = "OmprLinearVariableCollection"
  )
}

#' @export
`+.AbstractLinearFunction` <- function(x, y) {
  if (inherits(x, "LinearTerm")) {
    add.LinearTerm(x, y)
  } else if (inherits(x, "LinearFunction")) {
    add.LinearFunction(x, y)
  } else if (is.numeric(x)) {
    add.numeric(x, y)
  } else {
    not_supported()
  }
}

add.LinearFunction <- function(x, y) {
  if (missing(y)) {
    x
  } else if (inherits(y, "LinearTerm")) {
    x$terms <- c(x$terms, list(y))
    x
  } else if (inherits(y, "LinearFunction")) {
    x$terms <- c(x$terms, y$terms)
    x$constant <- x$constant + y$constant
    x
  } else if (inherits(y, "numeric")) {
    x$constant <- x$constant + y
    x
  } else {
    not_supported()
  }
}

add.LinearTerm <- function(x, y) {
  if (missing(y)) {
    x
  } else if (inherits(y, "LinearTerm")) {
    new_linear_function(list(x, y), 0)
  } else if (inherits(y, "LinearFunction")) {
    y$terms <- c(y$terms, list(x))
    y
  } else if (inherits(y, "numeric")) {
    new_linear_function(list(x), y)
  } else {
    not_supported()
  }
}

add.numeric <- function(x, y) {
  if (missing(y)) {
    x
  } else if (inherits(y, "LinearTerm")) {
    new_linear_function(list(y), x)
  } else if (inherits(y, "LinearFunction")) {
    y$constant <- y$constant + x
    y
  } else if (inherits(y, "numeric")) {
    unreachable() #nocovr
  } else {
    not_supported()
  }
}

#' @export
`-.AbstractLinearFunction` <- function(x, y) {
  if (missing(y)) {
    -1 * x
  } else {
    x + (-1 * y)
  }
}

#' @export
`*.AbstractLinearFunction` <- function(x, y) {
  if (inherits(x, "LinearTerm")) {
    multiply.LinearTerm(x, y)
  } else if (inherits(x, "LinearFunction")) {
    multiply.LinearFunction(x, y)
  } else if (is.numeric(x)) {
    multiply.numeric(x, y)
  } else {
    not_supported()
  }
}

multiply.LinearTerm <- function(x, y) {
  if (inherits(y, "LinearTerm")) {
    abort("Quadratic expression are not supported")
  } else if (inherits(y, "LinearFunction")) {
    abort("Quadratic expression are not supported")
  } else if (is.numeric(y)) {
    x$coefficient <- x$coefficient * y
    x
  } else {
    not_supported()
  }
}

multiply.LinearFunction <- function(x, y) {
  if (inherits(y, "LinearTerm")) {
    abort("Quadratic expression are not supported")
  } else if (inherits(y, "LinearFunction")) {
    abort("Quadratic expression are not supported")
  } else if (is.numeric(y)) {
    x$constant <- x$constant * y
    x$terms <- lapply(x$terms, function(el) el * y)
    x
  } else {
    not_supported()
  }
}

multiply.numeric <- function(x, y) {
  if (inherits(y, "LinearTerm")) {
    y$coefficient <- y$coefficient * x
    y
  } else if (inherits(y, "LinearFunction")) {
    y$constant <- y$constant * x
    y$terms <- lapply(y$terms, function(el) el * x)
    y
  } else {
    not_supported()
  }
}

#' @export
`/.AbstractLinearFunction` <- function(x, y) {
  if (inherits(x, "LinearTerm")) {
    divide.LinearTerm(x, y)
  } else if (inherits(x, "LinearFunction")) {
    divide.LinearFunction(x, y)
  } else {
    not_supported()
  }
}

divide.LinearFunction <- function(x, y) {
  if (is.numeric(y)) {
    x$constant <- x$constant / y
    x$terms <- lapply(x$terms, function(el) el / y)
    x
  } else {
    abort("Operation not supported")
  }
}

divide.LinearTerm <- function(x, y) {
  if (is.numeric(y)) {
    x$coefficient <- x$coefficient / y
    x
  } else {
    abort("Operation not supported")
  }
}

new_linear_term <- function(variable, coefficient) {
  structure(
    list(variable = variable, coefficient = coefficient),
    class = c("LinearTerm", "AbstractLinearFunction")
  )
}

new_linear_function <- function(terms, constant) {
  structure(
    list(terms = terms, constant = constant),
    class = c("LinearFunction", "AbstractLinearFunction")
  )
}

not_supported <- function() {
  abort("Operation not supported")
}
