sense_leq <- structure(
  list(sense = "<="),
  class = c("LinearConstraintSenseLeq", "LinearConstraintSense")
)
sense_eq <- structure(
  list(sense = "=="),
  class = c("LinearConstraintSenseEq", "LinearConstraintSense")
)
sense_geq <- structure(
  list(sense = ">="),
  class = c("LinearConstraintSenseGeq", "LinearConstraintSense")
)

flip_constaint_sense <- function(sense) {
  UseMethod("flip_constaint_sense")
}

#' @export
flip_constaint_sense.LinearConstraintSenseEq <- function(sense) {
  sense
}

#' @export
flip_constaint_sense.LinearConstraintSenseLeq <- function(sense) {
  sense_geq
}

#' @export
flip_constaint_sense.LinearConstraintSenseGeq <- function(sense) {
  sense_leq
}

new_linear_constraint <- function(lhs, rhs, sense) {
  UseMethod("new_linear_constraint", lhs)
}

#' @export
new_linear_constraint.LinearTerm <- function(lhs, rhs, sense) {
  lhs <- lhs + 0 # now it is a Linearfunction
  new_linear_constraint(lhs, rhs, sense)
}

#' @export
new_linear_constraint.LinearFunction <- function(lhs, rhs, sense) {
  lhs <- lhs - rhs
  rhs <- -1 * lhs$constant
  lhs$constant <- 0
  structure(
    list(lhs = lhs, rhs = rhs, sense = sense),
    class = "LinearConstraint"
  )
}

equation_dispatcher <- function(sense) {
  function(e1, e2) {
    if (inherits(e1, "AbstractLinearFunction")) {
      new_linear_constraint(e1, e2, sense)
    } else if (inherits(e2, "AbstractLinearFunction")) {
      new_linear_constraint(e2 * -1, e1 * -1, sense)
    } else {
      unreachable()
    }
  }
}

#' @export
`<=.AbstractLinearFunction` <- equation_dispatcher(sense_leq)

#' @export
`==.AbstractLinearFunction` <- equation_dispatcher(sense_eq)

#' @export
`>=.AbstractLinearFunction` <- equation_dispatcher(sense_geq)

unreachable <- function() {
  abort("Unreachable")
}
