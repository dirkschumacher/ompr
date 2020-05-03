# this is a generic function to iterativly traverse an AST
# in a pre-order way.
#' @noRd
ast_walker <- function(ast, on_element) {
  # rather just use the path to query ast on demand
  stack_data <- list()
  push <- function(x) stack_data <<- list(x, stack_data)
  push(list(ast = ast, path = c(), multiplier = NULL))
  get_ast_value <- function(path) {
    if (length(path) > 0) {
      ast[[path]]
    } else {
      ast
    }
  }
  inplace_update_ast <- function(path, value) {
    # update the ast in place
    if (length(path) > 0) {
      ast[[path]] <<- value
    } else {
      ast <<- value
    }
  }
  while (length(stack_data) > 0) {
    element <- stack_data[[1]]
    stack_data <- stack_data[[2]]
    on_element(push, inplace_update_ast, get_ast_value, element)
  }
  ast
}

#' always null
#' @noRd
return_null <- function(x) NULL

#' @noRd
try_eval_exp <- function(ast, envir = baseenv()) {
  result <- tryCatch(eval(ast, envir = envir), error = return_null)
  if (!is.numeric(result)) {
    ast
  } else {
    result
  }
}

#' @noRd
try_eval_exp_rec <- function(base_ast, envir = baseenv()) {
  on_element <- function(push, inplace_update_ast, get_ast_value, element) {
    path <- element$path
    ast <- if (is.null(element$ast)) get_ast_value(path) else element$ast
    stop_traversal <- element$stop_traversal
    is_final_sum_expr_call <- element$is_final_sum_expr_call
    exclude_vars <- element$exclude_vars
    if (is.null(stop_traversal)) {
      stop_traversal <- FALSE
    }
    if (is.null(is_final_sum_expr_call)) {
      is_final_sum_expr_call <- FALSE
    }
    if (is.null(exclude_vars)) {
      exclude_vars <- ""
    }
    if (!is.call(ast)) {
      # let's try to evaluate the whole sub-tree
      if (lazyeval::is_name(ast) &&
        !as.character(ast) %in% exclude_vars) {
        new_ast_eval <- try_eval_exp(ast, envir)
        if (is.numeric(new_ast_eval)) {
          inplace_update_ast(path, new_ast_eval)
        }
      }
    } else if (is.call(ast)) {
      if (as.character(ast[[1]]) == "sum_expr") {
        # we have a sum, let's expand it
        # TODO: detect that automatically
        # we need to evaluate anything from argument 2 onwards
        # e.g. sum_expr(x[i], i = 1:n) <- the n needs to be bound
        if (!is_final_sum_expr_call) {
          push(list(path = path, is_final_sum_expr_call = TRUE))
          free_vars <- free_indexes_rec(ast[[2]])
          # we need to make sure that we exclude only those free vars
          # that are not part of the calling environment
          free_vars <- free_vars[names(envir)]
          for (i in 3:length(ast)) {
            new_element <- list(
              ast = ast[[i]],
              path = c(path, i),
              exclude_vars = free_vars
            )
            push(new_element)
          }
        } else {
          # this expands the sum_expr expression
          # and triggers a reevaluation
          expanded_ast <- eval(ast)
          inplace_update_ast(path, expanded_ast)
          push(list(ast = expanded_ast, path = path))
        }
      } else if (!stop_traversal) {
        # we need to revisit the same node after the updates
        push(list(ast = ast, path = path, stop_traversal = TRUE))
        for (i in 2:length(ast)) {
          new_element <- list(
            ast = ast[[i]],
            path = c(path, i),
            exclude_vars = exclude_vars
          )
          push(new_element)
        }
      } else {
        new_ast_eval <- try_eval_exp(ast, envir)
        if (is.numeric(new_ast_eval)) {
          inplace_update_ast(path, new_ast_eval)
        }
      }
    } else {
      stop("Does not compute.", call. = FALSE)
    }
  }
  ast_walker(base_ast, on_element)
}

# given an AST in the form of an indexed variable
# it returns the names of the free variables in the indexes
# if var is not an index it returns character()
free_indexes <- function(expr) {
  if (expr[[1]] == "[" && length(expr) >= 3) {
    vars <- vapply(3:length(expr), function(i) {
      x <- expr[[i]]
      if (is.name(x)) {
        as.character(x)
      } else {
        NA_character_
      }
    }, character(1))
    return(vars[!is.na(vars)])
  }
  character()
}

# same as free_indexes but traverses the AST
free_indexes_rec <- function(expr) {
  free_vars <- character()
  on_element <- function(push, inplace_update_ast, get_ast_value, element) {
    path <- element$path
    ast <- if (is.null(element$ast)) get_ast_value(path) else element$ast
    if (lazyeval::is_call(ast)) {
      if (ast[[1]] == "[") {
        free_vars <<- c(free_vars, free_indexes(ast))
      } else {
        for (i in seq_len(length(ast))) {
          if (i > 1) {
            push(list(ast = ast[[i]], path = c(path, i)))
          }
        }
      }
    }
  }
  ast_walker(expr, on_element)
  unique(free_vars)
}

#' @noRd
bind_expression <- function(var_name, exp, envir, bound_subscripts) {
  var_values <- as.list(envir)
  for (x in c(var_name, names(bound_subscripts))) {
    var_values[[x]] <- NULL
  }
  eval(substitute(substitute(x, var_values), list(x = exp)))
}

#' @noRd
bind_variables <- function(model, ast, calling_env) {
  stopifnot(is.environment(calling_env))
  if (exists(names(model$variables), calling_env)) {
    problematic_vars <- mapply(function(x) {
      exists(x, calling_env)
    }, names(model$variables))
    problematic_vars <- names(model$variables)[problematic_vars]
    warning(paste0(
      "There are variables in your environment that interfere",
      " with your defined model variables: ",
      paste0(problematic_vars, collapse = ","),
      ". This can lead",
      " to unexpected behaviour."
    ), call. = FALSE)
  }
  try_eval_exp_rec(ast, calling_env)
}

check_expression <- function(model, the_ast) {
  if (lazyeval::is_call(the_ast) || lazyeval::is_name(the_ast)) {
    check_for_unknown_vars_impl(model, the_ast)
  }
}

#' @noRd
is_non_linear <- function(var_names, ast) {
  is_non_linear_impl(var_names, ast)
}

# this function takes an ast and transforms it
# into a an ast that only constists of either symbols,
# numerics, or muliplications of (numer & symbol)
# it walks through the ast and changes sub trees
# iterator based and not recusrive
#' @noRd
standardize_ast <- function(ast) {
  on_element <- function(push, inplace_update_ast, get_ast_value, element) {

    # this function pushes a new item to onto the stack
    push_idx <- function(local_ast, new_path, multiplier = NULL) {
      push(list(
        ast = local_ast,
        path = new_path,
        multiplier = multiplier
      ))
    }
    continue_traversal <- function(local_ast, path) {
      stopifnot(length(local_ast) == 3)
      push_idx(try_eval_exp(local_ast[[2]]), c(path, 2))
      push_idx(try_eval_exp(local_ast[[3]]), c(path, 3))
    }
    local_ast <- element$ast
    path <- element$path
    multiplier <- element$multiplier
    need_multiplication <- is.numeric(multiplier)
    multiplier <- if (!need_multiplication) 1 else multiplier
    mult_num <- if (is.numeric(multiplier)) multiplier else 1
    if (is.call(local_ast)) {
      operator <- as.character(local_ast[[1]])
      ast_length <- length(local_ast)
      if (operator == "(") {
        push_idx(local_ast[[2]], path, multiplier = multiplier)
      } else if (operator == "[") {
        if (need_multiplication && multiplier != 1) {
          new_ast <- substitute(
            x * y,
            list(x = multiplier, y = local_ast)
          )
          inplace_update_ast(path, new_ast)
        }
      } else if (operator %in% c("+", "-") && ast_length == 2) {
        # -x or -(23 + x)
        if (operator == "-") {
          new_ast <- substitute(-y * x, list(x = local_ast[[2]], y = mult_num))
        } else {
          new_ast <- substitute(y * x, list(x = local_ast[[2]], y = mult_num))
        }
        inplace_update_ast(c(path), new_ast)
        push_idx(new_ast, c(path))
      } else if (operator %in% c("-", "+")) {
        # if we have to multiply then we need to do something
        if (need_multiplication) {
          if (operator == "+") {
            new_ast <- substitute(
              x * y + x * z,
              list(
                x = multiplier, y = local_ast[[2]],
                z = local_ast[[3]]
              )
            )
          } else if (operator == "-") {
            new_ast <- substitute(
              x1 * y + x2 * z,
              list(
                x1 = multiplier, x2 = -1 * multiplier,
                y = local_ast[[2]],
                z = local_ast[[3]]
              )
            )
          }

          # also try to evaluate the two branches, maybe we can simplify sth.
          new_ast[[2]] <- try_eval_exp(new_ast[[2]])
          new_ast[[3]] <- try_eval_exp(new_ast[[3]])

          # update the ast and continue to traverse
          inplace_update_ast(c(path), new_ast)
          push_idx(new_ast, path)
        } else if (operator == "-") {
          new_ast <- substitute(x + -1 * y, list(
            x = local_ast[[2]],
            y = local_ast[[3]]
          ))
          inplace_update_ast(c(path), new_ast)
          push_idx(new_ast, path)
        } else {
          continue_traversal(local_ast, path)
        }
      } else if (operator %in% c("*", "/")) {
        left_is_num <- is.numeric(try_eval_exp(local_ast[[2]]))
        right_is_num <- is.numeric(try_eval_exp(local_ast[[3]]))
        is_multiplication <- operator == "*"
        if (!left_is_num && !right_is_num) {
          # this means either it is non-linear
          # or we need to further evaluate one of the branches
          continue_traversal(local_ast, path)
        } else if (left_is_num && right_is_num) {
          inplace_update_ast(path, try_eval_exp(local_ast) * multiplier)
        } else if (!is_multiplication) {
          # if it is a devision, let's make it a multiplication
          if (left_is_num && !right_is_num) {
            replacement <- list(
              x = mult_num / local_ast[[2]],
              y = local_ast[[3]]
            )
          } else if (!left_is_num && right_is_num) {
            replacement <- list(
              x = local_ast[[2]],
              y = mult_num / local_ast[[3]]
            )
          }
          new_ast <- substitute(y * x, replacement)
          inplace_update_ast(c(path), new_ast)
          push_idx(new_ast, path)
        } else {
          # multiplication & one of the sides is numeric
          if (left_is_num) {
            num_idx <- 2
            term_idx <- 3
          } else {
            num_idx <- 3
            term_idx <- 2
          }
          new_multiplier <- try_eval_exp(local_ast[[num_idx]])
          if (need_multiplication) {
            new_multiplier <- new_multiplier * multiplier
          }
          term <- local_ast[[term_idx]]
          push_idx(term, path, new_multiplier)
        }
      }
    } else if (lazyeval::is_name(local_ast)) {
      if (need_multiplication && multiplier != 1) {
        new_ast <- substitute(
          x * y,
          list(x = multiplier, y = local_ast)
        )
        inplace_update_ast(path, new_ast)
      }
    }
  }
  ast_walker(ast, on_element)
}

#' @noRd
normalize_expression <- function(model, expression, envir) {
  ast <- bind_variables(model, expression, envir)
  check_expression(model, ast)
  ast <- standardize_ast(ast)
  ast
}

#' Extract the coefficients out of an ast
#'
#' Should only be used if you want to develop your own solver.
#'
#' The expression should be a simple sum.
#' Only multiplication and + operators are allowed.
#' For each multiplication operation, one operand must be a numeric
#' the other a non-numeric.
#'
#' @param ast the abstract syntax tree (usually a call)
#'
#' @return a list with values for constants and coefficients
#' @noRd
extract_coefficients_internal <- function(ast) {
  result <- list()
  on_element <- function(push, inplace_update_ast, get_ast_value, element) {
    path <- element$path
    local_ast <- if (is.null(element$ast)) get_ast_value(path) else element$ast
    if (is.call(local_ast)) {
      operator <- local_ast[[1]]
      if (as.character(operator) == "+") {
        push(list(
          ast = try_eval_exp(local_ast[[2]]),
          path = c(path, 2)
        ))
        push(list(
          ast = try_eval_exp(local_ast[[3]]),
          path = c(path, 3)
        ))
      } else if (as.character(operator) == "*") {
        local_ast[[2]] <- try_eval_exp(local_ast[[2]])
        local_ast[[3]] <- try_eval_exp(local_ast[[3]])
        left_is_numeric <- is.numeric(local_ast[[2]])
        if (left_is_numeric) {
          numeric_idx <- 2
          expr_idx <- 3
        } else {
          numeric_idx <- 3
          expr_idx <- 2
        }

        coef <- try_eval_exp(local_ast[[numeric_idx]])
        if (!is.numeric(coef)) coef <- 0
        coefficent <- list(
          ast = local_ast[[expr_idx]],
          coef = coef
        )
        result <<- c(result, list(list(
          constant = 0,
          coefficients = list(coefficent)
        )))
      } else if (as.character(operator) == "[") {
        result <<- c(result, list(list(
          constant = 0,
          coefficients = list(list(ast = local_ast, coef = 1))
        )))
      } else {
        stop(paste0("Unexpected operator '", operator, "' found."),
          call. = FALSE
        )
      }
    } else {
      if (is.numeric(local_ast)) {
        result <<- c(
          result,
          list(list(
            constant = as.numeric(local_ast),
            coefficients = list()
          ))
        )
      } else {
        result <<- c(result, list(list(
          constant = 0,
          coefficients = list(list(ast = local_ast, coef = 1))
        )))
      }
    }
  }

  ast_walker(ast = ast, on_element = on_element)
  return_tuple <- list()

  # last step is to reduce the coefficients
  return_tuple$coefficients <- Reduce(function(acc, el) {
    key <- deparse(el$ast)
    value <- el$coef
    if (is.null(acc[[key]])) {
      acc[[key]] <- el
    } else {
      old_list <- acc[[key]]
      old_list$coef <- old_list$coef + value
      acc[[key]] <- old_list
    }
    acc
  }, unlist(lapply(result, function(x) x$coefficients),
    recursive = FALSE
  ), init = list())

  # sum over all constants
  return_tuple$constant <- Reduce("+", lapply(result, function(x) x$constant))

  return_tuple
}

#' Construct a sum expression
#'
#' This functions helps to create dynamic sum expression
#' based on external variables. Should only be used within
#' other 'ompr' functions.
#'
#' @param expr an expression that can be expanded to a sum
#' @param ... bind variables in expr using dots. See examples.
#'
#' @return the expanded sum as an AST
#'
#' @seealso \code{\link{add_constraint}}
#' @seealso \code{\link{set_objective}}
#'
#' @examples
#' # create a sum from x_1 to x_10
#' sum_expr(x[i], i = 1:10)
#' # create a sum from x_2 to x_10 with even indexes
#' sum_expr(x[i], i = 1:10, i %% 2 == 0)
#' @export
sum_expr <- function(expr, ...) {
  ast <- substitute(expr)
  stopifnot(lazyeval::is_call(ast))
  lazy_dots <- lazyeval::lazy_dots(...)
  classified_quantifiers <- classify_quantifiers(lazy_dots)
  bound_subscripts <- lapply(
    classified_quantifiers$quantifiers,
    lazyeval::lazy_eval
  )
  subscript_combinations <- build_quantifier_candidates(
    bound_subscripts,
    names(bound_subscripts),
    classified_quantifiers$filters
  )
  if (nrow(subscript_combinations) == 0) {
    return(substitute(0))
  }
  list_of_eval_exps <- apply(subscript_combinations, 1, function(row) {
    binding_env <- as.environment(as.list(row))
    try_eval_exp_rec(ast, binding_env)
  })
  Reduce(f = function(acc, el) {
    if (is.null(acc)) {
      return(el)
    }
    substitute(x + y, list(x = acc, y = el))
  }, x = list_of_eval_exps, init = NULL)
}

# extracts quantifiers and filters from lazydots
classify_quantifiers <- function(lazy_dots) {
  assignments <- names(lazy_dots) != ""
  list(
    quantifiers = lazy_dots[assignments],
    filters = lazy_dots[!assignments]
  )
}

# construct a quantifier candidate table
build_quantifier_candidates <- function(subscripts,
                                        subscript_names, filter_dots) {
  candidates <- expand.grid(subscripts, stringsAsFactors = FALSE)
  colnames(candidates) <- subscript_names
  if (length(filter_dots) > 0) {
    filter <- Reduce(function(acc, x) {
      rlang::quo(!!(acc) & (!!rlang::as_quosure(x$expr, x$env)))
    }, filter_dots, init = TRUE)
    filtered_candidates <- rlang::quo(candidates[(!!filter), , drop = FALSE])
    candidates <- rlang::eval_tidy(filtered_candidates, data = candidates)
  }
  rownames(candidates) <- NULL
  candidates
}

# validates a set of quantifier candidates
validate_quantifier_candidates <- function(candidates, zero_vars_msg) {
  if (nrow(candidates) == 0) {
    stop(zero_vars_msg)
  }
  only_integer_candidates <- apply(candidates, 1, function(r) {
    !anyNA(suppressWarnings(as.integer(r)))
  })
  stopifnot(only_integer_candidates)
}

print_model <- function(x) {
  cat("Mixed integer linear optimization problem\n")
  var_count <- nvars(x)
  cat("Variables:\n")
  cat("  Continuous:", var_count$continuous, "\n")
  cat("  Integer:", var_count$integer, "\n")
  cat("  Binary:", var_count$binary, "\n")

  # obj function
  objective <- x$objective
  if (!is.null(objective) &&
    length(objective$sense) == 1) {
    cat(
      "Model sense:",
      if (objective$sense == "max") "maximize" else "minimize",
      "\n"
    )
  } else {
    cat("No objective function. \n")
  }

  # constraints
  cat("Constraints:", nconstraints(x), "\n")
}
