#' Get all unique names of the model variables
#'
#' @param model the model
#' @return a character vector ordered in the same way
#'         as the constraint matrix columns and objective vector
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:3)
#' variable_keys(model)
#' @export
variable_keys <- function(model) UseMethod("variable_keys")

#' Extract the objective function from a model
#'
#' @param model the model
#'
#' @return a list with two named elements, 'solution' and 'constant'.
#' 'solution' is a sparse vector from the Matrix package.
#' 'constant' is a constant that needs to be added to get the final obj. value.
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:5) %>%
#'   set_objective(sum_over(i * x[i], i = 1:5) + 10)
#' objective_function(model)
#' @export
objective_function <- function(model) UseMethod("objective_function")

#' Extract the constraint matrix, the right hand side and the sense from a model
#'
#' @param model the model
#' @return a list with three named elements.
#'         'matrix' the (sparse) constraint matrix from the Matrix package.
#'         'rhs' is the right hand side vector in the order of the matrix.
#'         'sense' is a vector of the constraint senses
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:3) %>%
#'   add_variable(y[i], i = 1:3) %>%
#'   add_constraint(x[i] + y[i] <= 1, i = 1:3)
#' extract_constraints(model)
#' @export
extract_constraints <- function(model) UseMethod("extract_constraints")

#' Number of variables of a model
#'
#' @param model the model
#' @return a list with three named elements.
#'         'binary' => number of binary variables,
#'         'integer' => number of integer variables,
#'         'continuous' => number of continuous variables.
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:10, type = "binary") %>%
#'   add_variable(y[i], i = 1:5, type = "continuous") %>%
#'   add_variable(z[i], i = 1:2, type = "integer")
#' nvars(model)
#' @export
nvars <- function(model) UseMethod("nvars")

#' Variable types of a model
#'
#' One component for each variable in the correct order
#' @param model the model
#' @return a factor with levels binary, continuous, integer
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x, type = "binary") %>%
#'   add_variable(y, type = "continuous") %>%
#'   add_variable(z, type = "integer")
#' variable_types(model)
#' @export
variable_types <- function(model) UseMethod("variable_types")

#' Variable lower and upper bounds of a model
#'
#' @param model the model
#'
#' @return a list with two components 'lower' and 'upper' each
#' having a numeric vector of bounds. One for each variable.
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x, type = "binary") %>%
#'   add_variable(y, type = "continuous", lb = 2) %>%
#'   add_variable(z, type = "integer", ub = 3)
#' variable_bounds(model)
#' @export
variable_bounds <- function(model) UseMethod("variable_bounds")

#' Number of variables (rows) of the model
#'
#' @param model the model
#'
#' @return An integer equal to the number of variables. A
#' variable is here a column in the resulting constraint matrix.
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x) %>%
#'   add_variable(y[i], i = 1:10)
#' nconstraints(model) # 11
#' @export
nconstraints <- function(model) UseMethod("nconstraints")

#' Add a variable to the model
#'
#' A variable can either be a name or an indexed name. See examples.
#'
#' @param .model the model
#' @param .variable the variable name/definition
#' @param ... quantifiers for the indexed variable. Including filters
#' @param type must be either continuous, integer or binary
#' @param lb the lower bound of the variable
#' @param ub the upper bound of the variable
#'
#' @examples
#' library(magrittr)
#' MIPModel() %>%
#'   add_variable(x) %>% # creates 1 variable named x
#'   add_variable(y[i],
#'     i = 1:10, i %% 2 == 0,
#'     type = "binary"
#'   ) # creates 4 variables
#' @export
add_variable <- function(.model, .variable, ..., type = "continuous",
                         lb = -Inf, ub = Inf) {
  UseMethod("add_variable")
}

#' @param .dots Used to work around non-standard evaluation.
#' @rdname add_variable
#' @export
add_variable_ <- function(.model, .variable, ..., type = "continuous",
                          lb = -Inf, ub = Inf, .dots) {
  UseMethod("add_variable_")
}

#' Set the bounds of a variable
#'
#' Change the lower and upper bounds of a named variable,
#' indexed variable or a group of variables.
#'
#' @param .model the model
#' @param .variable the variable name/definition or a linear constraint
#' @param ... quantifiers for the indexed variable
#' @param lb the lower bound of the variable.
#' @param ub the upper bound of the variable
#'
#' For \code{MIPModel} you can also pass (in)equalities to define bounds. Please
#' look at the examples.
#'
#' @examples
#' library(magrittr)
#' MIPModel() %>%
#'   add_variable(x[i], i = 1:5) %>%
#'   add_constraint(x[i] >= 1, i = 1:5) %>% # creates 5 constraints
#'   set_bounds(x[i], lb = 3, i = 1:3) %>%
#'   variable_bounds()
#'
#' MIPModel() %>%
#'   add_variable(x[i], i = 1:5) %>%
#'   set_bounds(x[i] <= i, i = 1:5) %>% # upper bound
#'   set_bounds(x[i] >= 0, i = 1:5) %>% # lower bound
#'   set_bounds(x[5] == 45) %>%
#'   variable_bounds()
#' @export
set_bounds <- function(.model, .variable, ..., lb = NULL, ub = NULL) {
  UseMethod("set_bounds")
}

#' @param .dots Used to work around non-standard evaluation.
#' @rdname set_bounds
#' @export
set_bounds_ <- function(.model, .variable, ...,
                        lb = NULL, ub = NULL, .dots) {
  UseMethod("set_bounds_")
}

#' Set the model objective
#'
#' @param model the model
#' @param expression the linear objective as a sum of variables and constants
#' @param sense the model sense. Must be either "max" or "min".
#'
#' @return a Model with a new objective function definition
#' @examples
#' library(magrittr)
#' MIPModel() %>%
#'   add_variable(x, lb = 2) %>%
#'   add_variable(y, lb = 40) %>%
#'   set_objective(x + y, sense = "min")
#' @export
set_objective <- function(model, expression,
                          sense = c("max", "min")) {
  UseMethod("set_objective")
}

#' @rdname set_objective
#' @export
set_objective_ <- function(model, expression,
                           sense = c("max", "min")) {
  UseMethod("set_objective_")
}

#' Add a constraint
#'
#' Add one or more constraints to the model using quantifiers.
#'
#' @param .model the model
#' @param .constraint_expr the constraint. Must be a linear (in)equality with
#'        operator "<=", "==" or ">=".
#' @param ... quantifiers for the indexed variables. For all combinations of
#'            bound variables a new constraint is created. In addition
#'            you can add filter expressions
#' @param .show_progress_bar displays a progressbar when adding multiple
#'                           constraints
#'
#' @return a Model with new constraints added
#'
#' @examples
#' library(magrittr)
#' MIPModel() %>%
#'   add_variable(x[i], i = 1:5) %>%
#'   # creates 5 constraints
#'   add_constraint(x[i] >= 1, i = 1:5) %>%
#'   # you can also use filter expressions
#'   add_constraint(x[i] >= 1, i = 1:5, i %% 2 == 0) %>%
#'   # and depent on other indexes
#'   add_constraint(x[j] >= 1, i = 1:10, j = 1:i, j <= 5)
#' @export
add_constraint <- function(.model, .constraint_expr, ...,
                           .show_progress_bar = TRUE) {
  UseMethod("add_constraint")
}

#' @param .dots Used to work around non-standard evaluation.
#' @rdname add_constraint
#' @export
add_constraint_ <- function(.model,
                            .constraint_expr,
                            ...,
                            .dots,
                            .show_progress_bar = TRUE) {
  UseMethod("add_constraint_")
}

#' Solve a model
#'
#' @param model the model
#' @param solver a function mapping a model to a solution
#'
#' @return solver(model)
#'
#' @export
solve_model <- function(model, solver) UseMethod("solve_model")
