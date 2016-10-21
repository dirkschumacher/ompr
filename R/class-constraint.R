#' A class representing a constraint
#'
#'
#' @noRd
new_constraint <- function(lhs,
                           direction,
                           rhs,
                           original_lhs_expr = NULL,
                           original_rhs_expr = NULL,
                           constraint_quantifiers = NULL) {
  stopifnot(direction %in% c("<=", "==", ">="))
  structure(list(lhs = lhs,
                 direction = direction,
                 rhs = rhs,
                 original_lhs_expr = original_lhs_expr,
                 original_rhs_expr = original_rhs_expr,
                 constraint_quantifiers = constraint_quantifiers),
            class = "model_constraint")
}
