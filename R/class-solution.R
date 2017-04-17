#' Create a new solution
#'
#' This function/class should only be used if you develop your own solver.
#'
#' @param objective_value a numeric objective value
#' @param model the optimization model that was solved
#' @param status the status of the solution
#' @param solution a named numeric vector containing the solution values
#'
#' @export
new_solution <- function(model,
                         objective_value,
                         status,
                         solution) {
  stopifnot(is.numeric(objective_value))
  stopifnot(status %in% c("infeasible",
                         "unbounded", "optimal",
                         "userlimit", "error"))
  stopifnot(all(nchar(names(solution))))
  structure(list(model = model,
                 objective_value = objective_value,
                 status = status,
                 solution = solution), class = "solution")
}

#' Get variable values from a solution
#'
#' @param solution the solution object
#' @param expr a variable expression. You can partially bind indexes.
#'
#' @return a data.frame. One row for each variable instance
#'         and a column for each index.
#'         Unless it is a single variable, then it returns a single number.
#'
#' @usage
#' get_solution(solution, expr)
#' get_solution_(solution, expr)
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' result <- MIPModel() %>%
#'      add_variable(x[i], i = 1:5) %>%
#'      add_variable(y[i, j], i = 1:5, j = 1:5) %>%
#'      add_constraint(x[i] >= 1, i = 1:5) %>%
#'      set_bounds(x[i], lb = 3, i = 1:3) %>%
#'      set_objective(0) %>%
#'      solve_model(with_ROI("glpk"))
#' solution <- get_solution(result, x[i])
#' solution2 <- get_solution(result, y[i, 1])
#' solution3 <- get_solution(result, y[i, j])
#' }
#'
#' @export
get_solution <- function(solution, expr) {
  get_solution_(solution, lazyeval::lazy(expr))
}

#' @inheritParams get_solution
#' @rdname get_solution
#' @export
get_solution_ <- function(solution, expr) {
  UseMethod("get_solution_")
}

#' @export
get_solution_.solution <- function(solution, expr) {
  expr <- lazyeval::as.lazy(expr)
  ast <- expr$expr
  is_indexed_var <- is.call(ast)
  stopifnot(!is_indexed_var || ast[[1]] == "[" && length(ast) >= 3)
  var_name <- as.character(if (is_indexed_var) ast[[2]] else ast)
  if (is.null(solution$model$variables[[var_name]])) {
    stop("Variable not found")
  }
  if (is_indexed_var) {
    free_vars <- c()
    idx_pattern <- c()
    for (i in 3:length(ast)) {
      if (is.symbol(ast[[i]]) || is.name(ast[[i]])) {
        free_vars <- c(free_vars, as.character(ast[[i]]))
        idx_pattern <- c(idx_pattern, "(\\d+)")
      } else {
        idx_pattern <- c(idx_pattern,
                         as.character(as.numeric(ast[[i]])))
      }
    }
    instance_pattern <- paste0(var_name,
                               "\\[",
                               paste0(idx_pattern, collapse = ","),
                               "\\]")
    if (length(free_vars) == 0) {
      return(solution$solution[grepl(x = names(solution$solution),
                                     pattern = instance_pattern)])
    } else {
      # the solution is sorted lexigographically
      solution_names <- names(solution$solution)
      var_index <- do.call(rbind,
                           regmatches(solution_names,
                                      regexec(instance_pattern, solution_names)))
      na_rows <- as.logical(apply(is.na(var_index), 1, all))
      var_index <- var_index[!na_rows, ]
      var_values <- solution$solution[grepl(solution_names,
                                            pattern = instance_pattern)]
      result_df <- as.data.frame(var_index[, seq_len(ncol(var_index))[-1]])
      for (x in colnames(result_df)) {
        result_df[[x]] <- as.integer(as.character(result_df[[x]]))
      }
      result_df$value <- var_values
      result_df$variable <- var_name
      colnames(result_df) <- c(free_vars, "value", "variable")
      result_df <- result_df[, c("variable", free_vars, "value")]
      if (solution$status != "optimal") {
        result_df <- result_df[FALSE, ]
      }
      return(result_df)
    }
  } else {
    if (!var_name %in% names(solution$solution)) {
      stop(paste0("Either variable is not part of the model or you",
                  " have to specify the indexes."))
    }
    return(solution$solution[var_name])
  }
}

#' @inheritParams print
#' @export
print.solution <- function(x, ...) {
            cat("Status:", solver_status(x))
            cat("\n")
            cat("Objective value:", objective_value(x))
          }

#' Extract the numerical objective value from a solution
#'
#' @param solution a solution
#' @return numeric single item vector
#'
#' @export
objective_value <- function(solution) UseMethod("objective_value")

#' @export
objective_value.solution <- function(solution) {
  solution$objective_value
}

#' Get the solver status from a solution
#'
#' @param solution a solution
#' @return character vector being either "infeasible", "optimal", "unbounded", "userlimit" or "error
#'
#' @export
solver_status <- function(solution) UseMethod("solver_status")

#' @export
solver_status.solution <- function(solution) {
  solution$status
}
