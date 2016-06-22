#' @export
Constraint <- setClass("Constraint",
                              slots = c(
                                lhs = "expression",
                                direction = "character",
                                rhs = "expression"),
                              validity = function(object) {
                                object@direction %in% c("<=", "==", ">=")
                              })
