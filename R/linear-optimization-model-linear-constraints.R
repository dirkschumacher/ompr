#' Internal linear constraints classes and methods
#' @rdname linear-constraints
#' @slot sense leq, geq or eq
setClass("LinearConstraintSense", slots = c(
  sense = "character"
))

sense_leq <- new("LinearConstraintSense", sense = "<=")
sense_eq <- new("LinearConstraintSense", sense = "==")
sense_geq <- new("LinearConstraintSense", sense = ">=")


#' @include linear-optimization-model-linear-functions.R
#' @rdname linear-constraints
#' @param lhs a linear function
#' @param rhs a numeric scalar
#' @param sense the constraint sense
setClass("LinearConstraint", slots = c(
  lhs = "LinearFunction",
  rhs = "numeric",
  sense = "LinearConstraintSense"
))

#' @rdname linear-constraints
setGeneric("new_linear_constraint", function(lhs, rhs, sense) {
  stop("not implemented")
})

#' @rdname linear-constraints
setMethod(
  "new_linear_constraint",
  signature(
    lhs = "LinearFunction", rhs = "numeric",
    sense = "LinearConstraintSense"
  ),
  function(lhs, rhs, sense) {
    new("LinearConstraint", lhs = lhs, rhs = rhs, sense = sense)
  }
)

#' @rdname linear-constraints
setMethod(
  "new_linear_constraint",
  signature(
    lhs = "LinearFunction", rhs = "LinearTerm",
    sense = "LinearConstraintSense"
  ),
  function(lhs, rhs, sense) {
    rhs <- rhs + 0
    new_linear_constraint(lhs, rhs, sense)
  }
)

#' @rdname linear-constraints
setMethod(
  "new_linear_constraint",
  signature(
    lhs = "LinearFunction", rhs = "LinearFunction",
    sense = "LinearConstraintSense"
  ),
  function(lhs, rhs, sense) {
    lhs <- lhs - rhs
    rhs <- -1 * lhs@constant
    lhs@constant <- 0
    new_linear_constraint(lhs, rhs, sense)
  }
)

#' @rdname linear-constraints
setMethod(
  "new_linear_constraint",
  signature(
    lhs = "LinearTerm", rhs = "ANY",
    sense = "LinearConstraintSense"
  ),
  function(lhs, rhs, sense) {
    lhs <- lhs + 0
    new_linear_constraint(lhs, rhs, sense)
  }
)

#' @rdname linear-constraints
#' @param e1 a parameter
#' @param e2 a parameter
setMethod(
  "<=", signature(e1 = "AbstractLinearFunction", e2 = "ANY"),
  function(e1, e2) {
    new_linear_constraint(e1, e2, sense_leq)
  }
)

#' @rdname linear-constraints
setMethod(
  "==", signature(e1 = "AbstractLinearFunction", e2 = "ANY"),
  function(e1, e2) {
    new_linear_constraint(e1, e2, sense_eq)
  }
)

#' @rdname linear-constraints
setMethod(
  ">=", signature(e1 = "AbstractLinearFunction", e2 = "ANY"),
  function(e1, e2) {
    new_linear_constraint(e1, e2, sense_geq)
  }
)
