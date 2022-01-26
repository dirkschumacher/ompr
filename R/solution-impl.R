#' @export
get_solution.solution <- function(solution, expr, type = "primal") {
  type <- match.arg(type, c("primal", "dual"))
  expr <- enquo(expr)
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

#' @rdname get_solution
#' @export
get_solution_ <- function(solution, expr, type = "primal") {
  # we accept a lazy object for backwards compatibility reasons
  expr <- to_quosure(as.lazy(expr))
  get_solution(solution, !!expr, type)
}

extract_solution <- function(model, solution_vector, expr) {
  stopifnot(length(solution_vector) == length(names(solution_vector)))
  ast <- get_expr(expr)
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
        idx_pattern <- c(idx_pattern, "(.+)")
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
      if (ncol(var_index) == 0) {
        abort("No variable values found given the indexes.")
      }
      na_rows <- as.logical(apply(is.na(var_index), 1, all))
      var_index <- var_index[!na_rows, , drop = FALSE]
      var_values <- solution_vector[grepl(solution_names,
                                          pattern = instance_pattern
      )]
      result_df <- as.data.frame(var_index[, seq_len(ncol(var_index))[-1]])
      for (x in colnames(result_df)) {
        is_all_integer <- all(grepl("\\d+", result_df[[x]]))
        if (!is.na(is_all_integer) && is_all_integer) {
          result_df[[x]] <- as.integer(result_df[[x]])
        }
      }
      result_df$value <- var_values
      result_df$variable <- var_name
      colnames(result_df) <- c(free_vars, "value", "variable")
      result_df <- result_df[, c("variable", free_vars, "value")]

      # at last, for backwards compatibility order by free vars
      ordering <- do.call(order, lapply(rev(free_vars), function(x) result_df[[x]]))
      result_df <- result_df[ordering, ]
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

#' @export
objective_value.solution <- function(solution) {
  solution$objective_value
}

#' @export
solver_status.solution <- function(solution) {
  solution$status
}

#' @export
additional_solver_output.solution <- function(solution) {
  solution$additional_solver_output
}

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

#' @export
get_row_duals.solution <- function(solution) {
  solution_row_duals <- solution$solution_row_duals()
  stopifnot(
    is.numeric(solution_row_duals),
    (length(solution_row_duals) == 1L && is.na(solution_row_duals)) ||
      !anyNA(solution_row_duals)
  )
  solution_row_duals
}
