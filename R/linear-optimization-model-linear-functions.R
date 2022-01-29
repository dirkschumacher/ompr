new_linear_variable <- function(column_idx) {
  structure(list(column_idx = column_idx), class = "OmprLinearVariable")
}

new_variable_collection <- function(map = map) {
  structure(
    list(map = map),
    class = "OmprLinearVariableCollection"
  )
}

new_linear_term <- function(variable, coefficient) {
  structure(
    list(variable = variable, coefficient = coefficient),
    class = c("LinearTerm", "AbstractLinearFunction")
  )
}

new_linear_function <- function(terms, constant) {
  map <- fastmap()
  if (length(terms) > 0) {
    terms <- setNames(
      terms,
      vapply(terms, function(x) {
        as.character(x$variable$column_idx)
      }, character(1))
    )
    map$mset(.list = terms)
  }
  owner <- new_ownership_id()
  map$set("owner", owner)
  structure(
    list(terms = map, constant = constant, owner = owner),
    class = c("LinearFunction", "AbstractLinearFunction")
  )
}


# Re. ownership counter
# LinearFunctions have a reference to a map, which is mutable and by reference
# In the unexpected event that within the `MIPModel` framework two linear
# functions access/modify the same referenced map, a run time error is thrown.
# This is achieved by tracking ownership of each map. In order to implement
# the ownership check, we need a stateful counter of "owners". That counter
# is currently part of the package namespace.
.OwnerShipManager <- new.env(hash = FALSE, size = 1L)
.OwnerShipManager$counter <- 1
new_ownership_id <- function() {
  .OwnerShipManager$counter <- .OwnerShipManager$counter + 1
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
    x + (y + 0)
  } else if (inherits(y, "LinearFunction")) {
    x$constant <- x$constant + y$constant
    new_terms <- merge_terms(x, y)
    x$terms <- new_terms
    update_owner(x)
  } else if (is.numeric(y)) {
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
    (x + 0) + (y + 0)
  } else if (inherits(y, "LinearFunction")) {
    y + x
  } else if (is.numeric(y)) {
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
  } else if (is.numeric(y)) {
    unreachable() # nocovr
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
    update_terms(x, function(value) value * y)
    update_owner(x)
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
    update_terms(y, function(value) value * x)
    update_owner(y)
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
    update_terms(x, function(value) value / y)
    update_owner(x)
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

update_terms <- function(linear_fun, update_fun) {
  terms <- linear_fun$terms
  # we modify linear_fun's terms, so make sure we are the owner
  check_ownership(linear_fun)
  for (key in terms$keys()) {
    if (key == "owner") {
      next
    }
    terms$set(key, update_fun(terms$get(key)))
  }
  invisible(linear_fun)
}

merge_terms <- function(linear_fun1, linear_fun2) {
  terms1 <- linear_fun1$terms
  terms2 <- linear_fun2$terms
  # we modify linear_fun1's terms, so make sure we are the owner
  # also for we check ownership of linear_fun2 as well, why not
  check_ownership(linear_fun1)
  check_ownership(linear_fun2)
  for (key in terms2$keys()) {
    if (key == "owner") {
      next
    }
    if (terms1$has(key)) {
      term1 <- terms1$get(key)
      term2 <- terms2$get(key)
      term1$coefficient <- term1$coefficient + term2$coefficient
      terms1$set(key, term1)
    } else {
      terms1$set(key, terms2$get(key))
    }
  }
  terms1
}

terms_list <- function(linear_function) {
  check_ownership(linear_function)
  x <- linear_function$terms$as_list()
  x[names(x) != "owner"]
}

update_owner <- function(fun) {
  fun$owner <- new_ownership_id()
  fun$terms$set("owner", fun$owner)
  fun
}

check_ownership <- function(linear_function) {
  if (linear_function$owner != linear_function$terms$get("owner")) {
    abort(
      paste(
        "A linear functions is used without being the owner of the",
        "underlying data structure. This is a bug. Please report that",
        "as an issue."
      )
    )
  }
}

not_supported <- function() {
  abort("Operation not supported")
}
