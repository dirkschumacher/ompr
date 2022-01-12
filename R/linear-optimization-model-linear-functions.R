#' Internal linear function classes and methods
#' @rdname linear-functions
#' @keywords internal
setClass("OmprLinearVariable", slots = c(column_idx = "numeric"))

#' @rdname linear-functions
setClass("AbstractLinearFunction")

#' @rdname linear-functions
setClass("LinearTerm", slots = c(
  coefficient = "numeric", variable = "OmprLinearVariable"
), contains = "AbstractLinearFunction")

#' @rdname linear-functions
setClass("LinearFunction", slots = c(
  terms = "list",
  constant = "numeric"
), contains = "AbstractLinearFunction")

new_linear_variable <- function(column_idx) {
  stopifnot(length(column_idx) == 1, !is.na(column_idx), column_idx >= 1)
  new("OmprLinearVariable", column_idx = column_idx)
}

new_linear_term <- function(variable, coefficient) {
  stopifnot(length(coefficient) == 1, !is.na(coefficient))
  stopifnot(inherits(variable, "OmprLinearVariable"))
  new("LinearTerm", variable = variable, coefficient = coefficient)
}

new_linear_function <- function(terms, constant) {
  stopifnot(length(constant) == 1, !is.na(constant))
  stopifnot(
    all(
      vapply(terms, function(term) {
        inherits(term, "LinearTerm")
      }, logical(1L))
    )
  )
  # TODO: make it a typed list, maybe
  new("LinearFunction", terms = terms, constant = constant)
}

# Linear Terms
#' @rdname linear-functions
#' @param e1 a parameter
#' @param e2 a parameter
setMethod(
  "*", signature(e1 = "LinearTerm", e2 = "numeric"),
  function(e1, e2) {
    e1@coefficient <- e1@coefficient * e2
    e1
  }
)

#' @rdname linear-functions
setMethod(
  "*", signature(e1 = "numeric", e2 = "LinearTerm"),
  function(e1, e2) e2 * e1
)

#' @rdname linear-functions
setMethod(
  "/", signature(e1 = "LinearTerm", e2 = "numeric"),
  function(e1, e2) {
    e1@coefficient <- e1@coefficient / e2
    e1
  }
)

#' @rdname linear-functions
setMethod(
  "+", signature(e1 = "LinearTerm", e2 = "numeric"),
  function(e1, e2) {
    new_linear_function(terms = list(e1), constant = e2)
  }
)

#' @rdname linear-functions
setMethod(
  "+", signature(e1 = "numeric", e2 = "LinearTerm"),
  function(e1, e2) e2 + e1
)

#' @rdname linear-functions
setMethod(
  "+", signature(e1 = "LinearTerm", e2 = "missing"),
  function(e1, e2) {
    e1
  }
)

#' @rdname linear-functions
setMethod(
  "+", signature(e1 = "LinearTerm", e2 = "LinearTerm"),
  function(e1, e2) {
    new_linear_function(terms = list(e1, e2), constant = 0)
  }
)

#' @rdname linear-functions
setMethod(
  "-", signature(e1 = "LinearTerm", e2 = "missing"),
  function(e1, e2) {
    e1@coefficient <- -1 * e1@coefficient
    e1
  }
)

# Linear Functions
#' @rdname linear-functions
setMethod(
  "+", signature(e1 = "LinearFunction", e2 = "numeric"),
  function(e1, e2) {
    e1@constant <- e1@constant + e2
    e1
  }
)

#' @rdname linear-functions
setMethod(
  "+", signature(e1 = "numeric", e2 = "LinearFunction"),
  function(e1, e2) {
    e2 + e1
  }
)

#' @rdname linear-functions
setMethod(
  "+", signature(e1 = "LinearFunction", e2 = "LinearTerm"),
  function(e1, e2) {
    e1@terms <- c(e1@terms, list(e2))
    e1
  }
)

#' @rdname linear-functions
setMethod(
  "+", signature(e1 = "LinearTerm", e2 = "LinearFunction"),
  function(e1, e2) {
    e2 + e1
  }
)

#' @rdname linear-functions
setMethod(
  "+", signature(e1 = "LinearFunction", e2 = "LinearFunction"),
  function(e1, e2) {
    e1@terms <- c(e1@terms, e2@terms)
    e1@constant <- e1@constant + e2@constant
    e1
  }
)

#' @rdname linear-functions
setMethod(
  "-", signature(e1 = "LinearFunction", e2 = "numeric"),
  function(e1, e2) {
    e1 + (-1 * e2)
  }
)

#' @rdname linear-functions
setMethod(
  "-", signature(e1 = "numeric", e2 = "LinearFunction"),
  function(e1, e2) {
    (-1 * e2) + e1
  }
)

#' @rdname linear-functions
setMethod(
  "-", signature(e1 = "LinearTerm", e2 = "LinearTerm"),
  function(e1, e2) {
    e1 + (-1 * e2)
  }
)

#' @rdname linear-functions
setMethod(
  "-", signature(e1 = "numeric", e2 = "LinearTerm"),
  function(e1, e2) {
    e1 + (-1 * e2)
  }
)

#' @rdname linear-functions
setMethod(
  "-", signature(e1 = "LinearFunction", e2 = "LinearTerm"),
  function(e1, e2) {
    e1 + (-1 * e2)
  }
)

#' @rdname linear-functions
setMethod(
  "-", signature(e1 = "LinearTerm", e2 = "LinearFunction"),
  function(e1, e2) {
    (-1 * e2) + e1
  }
)

#' @rdname linear-functions
setMethod(
  "-", signature(e1 = "LinearFunction", e2 = "LinearFunction"),
  function(e1, e2) {
    e1 + (-1 * e2)
  }
)

#' @rdname linear-functions
setMethod(
  "-", signature(e1 = "LinearFunction", e2 = "LinearFunction"),
  function(e1, e2) {
    (-1 * e2) + e1
  }
)

#' @rdname linear-functions
setMethod(
  "*", signature(e1 = "LinearFunction", e2 = "numeric"),
  function(e1, e2) {
    e1@constant <- e1@constant * e2
    e1@terms <- lapply(e1@terms, function(x) x * e2)
    e1
  }
)

#' @rdname linear-functions
setMethod(
  "*", signature(e1 = "numeric", e2 = "LinearFunction"),
  function(e1, e2) {
    e2 * e1
  }
)

#' @rdname linear-functions
setMethod(
  "/", signature(e1 = "LinearFunction", e2 = "numeric"),
  function(e1, e2) {
    e1@constant <- e1@constant / e2
    e1@terms <- lapply(e1@terms, function(x) x / e2)
    e1
  }
)
