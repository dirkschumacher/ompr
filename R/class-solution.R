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
#' duals <- get_solution(result, x[i], type = "duals")
#' }
#'
#' @export
get_solution <- function(solution, expr, type = "primal") {
  get_solution_(solution, lazyeval::lazy(expr), type)
}

#' @inheritParams get_solution
#' @rdname get_solution
#' @export
get_solution_ <- function(solution, expr, type) {
  UseMethod("get_solution_")
}

#' @export
get_solution_.solution <- function(solution, expr, type) {
  type <- match.arg(type, c("primal", "dual"))
  solution_vector <- if (type == "primal") {
    solution$solution
  } else {
    solution$solution_column_duals()
  }
  if (is.null(solution_vector) || anyNA(solution_vector)) {
    stop("The solution from the solver is invalid. It is NULL or contains NAs.",
      " Maybe the solver does not export ", type, "s?",
      call. = FALSE
    )
  }
  extract_solution(solution$model, solution_vector, expr)
}

extract_solution <- function(model, solution_vector, expr) {
  stopifnot(length(solution_vector) == length(names(solution_vector)))
  expr <- lazyeval::as.lazy(expr)
  ast <- expr$expr
  is_indexed_var <- is.call(ast)
  stopifnot(!is_indexed_var || ast[[1]] == "[" && length(ast) >= 3)
  var_name <- as.character(if (is_indexed_var) ast[[2]] else ast)
  if (is.null(model$variables[[var_name]])) {
    stop("Variable '", var_name, "' not found", call. = FALSE)
  }
  if (is_indexed_var) {
    free_vars <- c()
    idx_pattern <- c()
    for (i in 3:length(ast)) {
      if (is.symbol(ast[[i]]) || is.name(ast[[i]])) {
        free_vars <- c(free_vars, as.character(ast[[i]]))
        idx_pattern <- c(idx_pattern, "(\\d+)")
      } else {
        idx_pattern <- c(
          idx_pattern,
          as.character(as.numeric(ast[[i]]))
        )
      }
    }
    instance_pattern <- paste0(
      "^",
      var_name,
      "\\[",
      paste0(idx_pattern, collapse = ","),
      "\\]",
      "$"
    )
    if (length(free_vars) == 0) {
      return(solution_vector[grepl(
        x = names(solution_vector),
        pattern = instance_pattern
      )])
    } else {
      # the solution is sorted lexigographically
      solution_names <- names(solution_vector)
      rexp_c <- regexec(instance_pattern, solution_names)
      var_index <- do.call(rbind, regmatches(solution_names, rexp_c))
      na_rows <- as.logical(apply(is.na(var_index), 1, all))
      var_index <- var_index[!na_rows, , drop = FALSE]
      var_values <- solution_vector[grepl(solution_names,
        pattern = instance_pattern
      )]
      result_df <- as.data.frame(var_index[, seq_len(ncol(var_index))[-1]])
      for (x in colnames(result_df)) {
        result_df[[x]] <- as.integer(as.character(result_df[[x]]))
      }
      result_df$value <- var_values
      result_df$variable <- var_name
      colnames(result_df) <- c(free_vars, "value", "variable")
      result_df <- result_df[, c("variable", free_vars, "value")]
      return(result_df)
    }
  } else {
    if (!var_name %in% names(solution_vector)) {
      stop(paste0(
        "Either variable is not part of the model or you",
        " have to specify the indexes."
      ), call. = FALSE)
    }
    return(solution_vector[var_name])
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
#'   set_objective(sum_expr(i * x[i], i = 1:5)) %>%
#'   solve_model(with_ROI("glpk"))
#'
#' get_column_duals(result)
#' }
#'
#' @export
get_column_duals <- function(solution) UseMethod("get_column_duals")

#' @export
get_column_duals.solution <- function(solution) {
  solution_column_duals <- solution$solution_column_duals()
  stopifnot(
    is.numeric(solution_column_duals),
    (length(solution_column_duals) == 1L && is.na(solution_column_duals)) ||
      (length(solution_column_duals) == length(solution$solution))
  )
  solution_column_duals
}

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

#' @export
get_row_duals.solution <- function(solution) {
  solution_row_duals <- solution$solution_row_duals()
  stopifnot(
    is.numeric(solution_row_duals),
    (!is.na(solution_row_duals) || length(solution_row_duals) == 1L)
  )
  solution_row_duals
}
