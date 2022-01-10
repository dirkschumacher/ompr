#' Create a new solution
#'
#' This function/class should only be used if you develop your own solver.
#'
#' @param objective_value a numeric objective value
#' @param model the optimization model that was solved
#' @param status the status of the solution
#' @param solution a named numeric vector containing the primal solution values
#' @param solution_column_duals A function without arguments that returns a numeric vector containing the column dual solution values. `NA_real_`, if no column duals are available/defined.
#' @param solution_row_duals A function without arguments that returns a numeric vector containing the column dual solution values. `NA_real_`, if no column duals are available/defined.
#'
#' @export
new_solution <- function(model,
                         objective_value,
                         status,
                         solution,
                         solution_column_duals = function() NA_real_,
                         solution_row_duals = function() NA_real_) {
  stopifnot(is.numeric(objective_value))
  stopifnot(status %in% c(
    "infeasible",
    "unbounded", "optimal",
    "userlimit", "error"
  ))
  stopifnot(all(nchar(names(solution))))
  stopifnot(is.function(solution_column_duals), is.function(solution_row_duals))
  structure(list(
    model = model,
    objective_value = objective_value,
    status = status,
    solution = solution,
    solution_column_duals = solution_column_duals,
    solution_row_duals = solution_row_duals
  ), class = "solution")
}

#' Get variable values from a solution
#'
#' @param solution the solution object
#' @param expr a variable expression. You can partially bind indexes.
#' @param type optional, either "primal" or "dual". The default value is "primal".
#' If "primal" it returns the primal solution, otherwise the column duals.
#' Especially the dual values depend on the solver. If no duals are calculated,
#' the function stops with an error message.
#'
#' @return a data.frame. One row for each variable instance
#'         and a column for each index.
#'         Unless it is a single variable, then it returns a single number.
#'
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' result <- MIPModel() %>%
#'   add_variable(x[i], i = 1:5) %>%
#'   add_variable(y[i, j], i = 1:5, j = 1:5) %>%
#'   add_constraint(x[i] >= 1, i = 1:5) %>%
#'   set_bounds(x[i], lb = 3, i = 1:3) %>%
#'   set_objective(0) %>%
#'   solve_model(with_ROI("glpk"))
#' solution <- get_solution(result, x[i])
#' solution2 <- get_solution(result, y[i, 1])
#' solution3 <- get_solution(result, y[i, j])
#' duals <- get_solution(result, x[i], type = "dual")
#' }
#'
#' @export
get_solution <- function(solution, expr, type = "primal") {
  get_solution_impl(solution, enquo(expr), type)
}

#' Extract the numerical objective value from a solution
#'
#' @param solution a solution
#' @return numeric single item vector
#'
#' @export
objective_value <- function(solution) UseMethod("objective_value")

#' Get the solver status from a solution
#'
#' @param solution a solution
#' @return character vector being either "infeasible", "optimal", "unbounded", "userlimit" or "error
#'
#' @export
solver_status <- function(solution) UseMethod("solver_status")

#' Gets the column duals of a solution
#'
#' @param solution a solution
#'
#' @return Either a numeric vector with one element per column or `NA_real_`.
#'
#' @examples
#' \dontrun{
#' result <- MIPModel() %>%
#'   add_variable(x[i], i = 1:5) %>%
#'   add_variable(y[i, j], i = 1:5, j = 1:5) %>%
#'   add_constraint(x[i] >= 1, i = 1:5) %>%
#'   set_bounds(x[i], lb = 3, i = 1:3) %>%
#'   set_objective(sum_over(i * x[i], i = 1:5)) %>%
#'   solve_model(with_ROI("glpk"))
#'
#' get_column_duals(result)
#' }
#'
#' @export
get_column_duals <- function(solution) UseMethod("get_column_duals")

#' Gets the row duals of a solution
#'
#' @param solution a solution
#'
#' @return Either a numeric vector with one element per row or `NA_real_`.
#'
#' @examples
#' \dontrun{
#' result <- MIPModel() %>%
#'   add_variable(x[i], i = 1:5) %>%
#'   add_variable(y[i, j], i = 1:5, j = 1:5) %>%
#'   add_constraint(x[i] >= 1, i = 1:5) %>%
#'   set_bounds(x[i], lb = 3, i = 1:3) %>%
#'   set_objective(sum_expr(i * x[i], i = 1:5)) %>%
#'   solve_model(with_ROI("glpk"))
#'
#' get_row_duals(result)
#' }
#' @export
get_row_duals <- function(solution) UseMethod("get_row_duals")