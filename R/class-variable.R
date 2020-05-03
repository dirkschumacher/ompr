#' An S3 class to represent a Variable.
#'
#' This is class should only directly be used if you develop your own solver.
#'
#' @param arity the number of subscripts the variable has
#' @param type the type of the variable must be one of
#'        (binary, continuous, integer). Default is continuous.
#' @param lb an optional lower bound. No value means unbounded.
#' @param ub an optional upper bound. No value means unbounded.
#' @param instances a character vector with an entry for each
#'        variable instance.
#'
#' @noRd
new_variable <- function(arity, type, instances, lb, ub,
                         variable_expression, variable_quantifiers) {
  stopifnot(length(arity) == 1)
  stopifnot(length(type) == 1)
  stopifnot(arity >= 0)
  stopifnot(type %in% c("binary", "continuous", "integer"))
  structure(list(
    arity = arity,
    type = type, instances = instances,
    lb = lb, ub = ub
  ),
  class = "model_variable"
  )
}
