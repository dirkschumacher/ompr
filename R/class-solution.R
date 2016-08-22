#' An S4 class to encode a solution
#' @export
Solution <- setClass("Solution",
                              slots = c(
                                objective_value = "numeric",
                                model = "Model",
                                status = "character",
                                solution = "numeric"),
                              validity = function(object) {
                                object@status %in% c("infeasible",
                                                     "unbounded", "optimal") &&
                                  all(nchar(names(object@solution)) > 0)
                              })

#' Extracts variable values from a solution
#'
#' @param solution the solution object
#' @param exp a variable expression. You can partially bind indexes.
#'
#' @return a data.frame. One row for each variable instance
#'         and a column for each index.
#' @importFrom stringr str_match
#' @export
setGeneric("get_solution", function(solution, exp) {
  ast <- substitute(exp)
  is_indexed_var <- is.call(ast)
  stopifnot(!is_indexed_var || ast[[1]] == "[" && length(ast) >= 3)
  var_name <- as.character(if (is_indexed_var) ast[[2]] else ast)
  if (is.null(solution@model@variables[[var_name]])) {
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
      return(solution@solution[grepl(x = names(solution@solution),
                                     pattern = instance_pattern)])
    } else {
      # the solution is sorted lexigographically
      var_index <- stringr::str_match(names(solution@solution),
                                      pattern = instance_pattern)
      na_rows <- as.logical(apply(is.na(var_index), 1, all))
      var_index <- var_index[!na_rows, ]
      var_values <- solution@solution[grepl(names(solution@solution),
                                            pattern = instance_pattern)]
      result_df <- as.data.frame(var_index[, seq_len(ncol(var_index))[-1]])
      for (x in colnames(result_df)) {
        result_df[[x]] <- as.integer(result_df[[x]])
      }
      result_df$value <- var_values
      result_df$variable <- var_name
      colnames(result_df) <- c(free_vars, "value", "variable")
      result_df <- result_df[, c("variable", free_vars, "value")]
      if (solution@status != "optimal") {
        result_df <- result_df[FALSE, ]
      }
      return(result_df)
    }
  } else {
    if (!var_name %in% names(solution@solution)) {
      stop(paste0("Either variable is not part of the model or you",
                  " have to specify the indexes."))
    }
    return(solution@solution[var_name])
  }
})

#' Outputs a model summary to the console.
#' @param object the solution object
#' @export
setMethod("show", signature(object = "Solution"),
          definition = function(object) {
            cat("Status:", object@status)
            cat("\n")
            cat("Objective value:", object@objective_value)
          })
