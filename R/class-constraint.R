#' A class representing a constraint
#'
#'
#' @noRd
new_constraint <- function(lhs,
                           direction,
                           rhs) {
  stopifnot(direction %in% c("<=", "==", ">="))
  structure(list(lhs = lhs,
                 direction = direction,
                 rhs = rhs),
            class = "model_constraint")
}
