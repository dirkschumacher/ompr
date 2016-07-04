#' An S4 class to represent a Variable.
#'
#' @slot arity the number of subscripts the variable has
#' @slot type the type of the variable must be one of (binary, continuous, integer). Default is continuous.
#' @slot lb an optional lower bound. No value means unbounded.
#' @slot ub an optional upper bound. No value means unbounded.
#' @export
Variable <- setClass("Variable",
  slots = c(
    arity = "integer",
    type = "character",
    lb = "numeric",
    ub = "numeric",
    instances = "character",
    variable_expression = "expression",
    variable_quantifiers = "list"),
  validity = function(object) {
    length(object@arity) == 1 && length(object@type) == 1 &&
    object@arity >= 0 && object@type %in% c("binary", "continuous", "integer")
  })
