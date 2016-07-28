#' An S4 class to represent a ObjectiveFunction
#'
#' @slot expression the expression in standard form
#' @slot original_expression the original expression as supplied by the user
#' @slot direction the direction of optimization
#' @export
ObjectiveFunction <- setClass("ObjectiveFunction", # Exclude Linting
                              slots = c(
                                expression = "expression",
                                original_expression = "expression",
                                direction = "character"),
                              validity = function(object) {
                                length(object@direction) == 1 &&
                                  object@direction %in% c("min", "max")
                              })
