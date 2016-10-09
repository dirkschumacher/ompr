#' An S4 class to represent a Variable.
#'
#' This is class should only be used if you develop your own solver.
#'
#' @slot arity the number of subscripts the variable has
#' @slot type the type of the variable must be one of (binary, continuous, integer). Default is continuous.
#' @slot lb an optional lower bound. No value means unbounded.
#' @slot ub an optional upper bound. No value means unbounded.
#' @slot instances a character vector with an entry for each variable instance.
#' @slot variable_expression the original variable expression as an ast
#' @slot variable_quantifiers the variable quantifiers.
#'
#' @noRd
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
