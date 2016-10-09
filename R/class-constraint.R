#' A class representing a constraint
#'
#'
#' @noRd
Constraint <- setClass("Constraint",
                              slots = c(
                                lhs = "expression",
                                direction = "character",
                                rhs = "expression",
                                original_lhs_expr = "expression",
                                original_rhs_expr = "expression",
                                constraint_quantifiers = "list" # e.g. i = 1:n
                              ),
                              validity = function(object) {
                                # TODO: check that (l|r)hs is in the correct
                                # format
                                object@direction %in% c("<=", "==", ">=")
                              })
