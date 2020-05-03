#' A class representing a constraint
#'
#'
#' @noRd
new_constraint <- function(lhs,
                           sense,
                           rhs) {
  stopifnot(sense %in% c("<=", "==", ">="))
  structure(list(
    lhs = lhs,
    sense = sense,
    rhs = rhs
  ),
  class = "model_constraint"
  )
}
