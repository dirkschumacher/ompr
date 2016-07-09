#' @export
sum_exp <- function(exp, free_vars = c(), ...) {
  # TODO: This should probably be moved into the model
  # TODO: Make sure the input is correct
  # TODO: Take model and make sure
  # we do not overwrite any model variables
  parent_frame <- parent.frame()
  iterators <- list(...)
  subscript_combinations <- expand.grid(iterators)
  ast <- substitute(exp)
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


normalize_expression <- function(model, expression, environment) {
  ast <- bind_variables(model, expression, environment)
  ast <- try_eval_exp_rec(ast)
  check_expression(model, ast)
  ast <- standardize_ast(ast)
  ast
}

bind_variables <- function(model, ast, calling_env) {
  # clean calling environment
  if (is.list(calling_env)) {
    env_filter <- !(names(calling_env) %in% names(model@variables))
    calling_env <- calling_env[env_filter]
    calling_env <- calling_env[nchar(names(calling_env)) > 0]
  }
  if (is.environment(calling_env)) {
    return(try_eval_exp_rec(ast, calling_env))
  }

  # bind variables
  eval(substitute(substitute(x, calling_env), list(x = ast)))
}

# checks if an ast has unbounded index
# i.e. unevaluated ones
any_unbounded_indexes <- function(ast) {
  if (is.call(ast)) {
    if (ast[[1]] == "[") {
      for (i in 3:length(ast)) {
        if (is.name(ast[[i]])) {
          return(TRUE)
        } else {
          if (!is.numeric(ast[[i]])) {
            return(FALSE)
          }
        }
      }
      return(FALSE)
    } else {
      for (i in 3:length(ast)) {
        return(any_unbounded_indexes(ast[[i]]))
      }
    }
  } else {
    return(FALSE)
  }
}

check_expression <- function(model, the_ast) {
  if (is.call(the_ast) && the_ast[[1]] == "[" &&
      class(the_ast[[2]]) == "name") {
    var_name <- as.character(the_ast[[2]])
    search_key <- paste0(as.character(the_ast[3:length(the_ast)]),
                         collapse = "_")
    var <- model@variables[[var_name]]
    if (!is.null(var) && !search_key %in% var@instances) {
      stop(paste0("The expression contains a variable,",
                  " that is not part of the model."))
    }
  } else if (is.call(the_ast)) {
    for (i in 2:length(the_ast)) {
      check_expression(model, the_ast[[i]])
    }
  }
}

try_eval_exp <- function(ast, envir = baseenv()) {
  result <- try(eval(ast, envir = envir), silent = TRUE)
  if (!is.numeric(result)) {
    ast
  } else {
    result
  }
}

try_eval_exp_rec <- function(ast, envir = baseenv()) {
  if (!is.call(ast)) {
    # let's try to evaluate the whole sub-tree
    new_ast_eval <- try_eval_exp(ast, envir)
    if (is.numeric(new_ast_eval)) {
      return(new_ast_eval)
    }
    return(ast)
  } else if (is.call(ast)) {
    new_ast <- ast
    if (as.character(new_ast[[1]]) == "sum_exp") {
      # we have a sum, let's expand it
      # TODO: detect that automatically
      # we need to evaluate anything from argument 2 onwards
      # e.g. sum_exp(x[i], i = 1:n) <- the n needs to be bound
      le_ast <- new_ast
      for (i in 3:length(le_ast)) {
        le_ast[[i]] <- try_eval_exp_rec(le_ast[[i]], envir)
      }
      new_ast <- eval(le_ast)
    }
    for (i in 2:length(new_ast)) {
      new_ast[[i]] <- try_eval_exp_rec(new_ast[[i]], envir)
    }
    # let's try to evaluate the whole sub-tree
    new_ast_eval <- try_eval_exp(new_ast, envir)
    if (is.numeric(new_ast_eval)) {
      return(new_ast_eval)
    }
    return(new_ast)
  } else {
    stop("Does not compute.")
  }
}

is_non_linear <- function(var_names, ast) {
  contains_vars <- function(le_ast) {
    if (is.call(le_ast)) {
      for (i in 2:length(le_ast)) {
        result <- contains_vars(le_ast[[i]])
        if (result) return(TRUE)
      }
      return(FALSE)
    } else if (is.name(le_ast)) {
      as.character(le_ast) %in% var_names
    } else {
      FALSE
    }
  }
  if (is.call(ast) && as.character(ast[[1]]) %in% c("*", "/", "^")) {
    contains_vars(ast[[2]]) && contains_vars(ast[[3]])
  } else if (is.call(ast)) {
    for (i in 2:length(ast)) {
      result <- is_non_linear(var_names, ast[[i]])
      if (result) return(TRUE)
    }
    return(FALSE)
  } else {
    FALSE
  }
}


# this function takes an ast and transforms it
# into a an ast that only constists of either symbols,
# numerics, or muliplications of (numer & symbol)
standardize_ast <- function(ast, multiply = NULL) {
  if (is.call(ast)) {
    if (ast[[1]] == "/") {
      multiplier <- if (is.null(multiply)) 1 else multiply
      left_is_num <-  is.numeric(ast[[2]])
      right_is_num <-  is.numeric(ast[[3]])
      if (left_is_num && !right_is_num) {
        replacement <- list(x = multiplier / ast[[2]], y = ast[[3]])
        ast <- substitute(x * y, replacement)
      } else if (!left_is_num && right_is_num) {
        replacement <- list(x = ast[[2]], y =  multiplier / ast[[3]])
        ast <- substitute(y * x, replacement)
      }
      standardize_ast(ast, multiply)
    } else if (ast[[1]] == "*") {
      left_is_num <-  is.numeric(ast[[2]])
      right_is_num <-  is.numeric(ast[[3]])
      if (left_is_num) {
        term <- ast[[3]]
        multiplier <- as.numeric(ast[[2]])
        if (is.call(term) && all(as.character(term) != "[")) {
          return(standardize_ast(term, multiply = multiplier))
        }
      } else if (right_is_num) {
        term <- ast[[2]]
        multiplier <- as.numeric(ast[[3]])
        if (is.call(term) && as.character(term) != "[") {
          return(standardize_ast(term, multiply = multiplier))
        }
      }
      ast
    } else if (as.character(ast[[1]]) %in% c("+", "-")) {
      if (!is.null(multiply)) {
        operator <- ast[[1]]
        if (operator == "(") {
          return(standardize_ast(ast[[2]], multiply = multiply))
        }
        if (length(ast) == 2 && ast[[1]] == "-") {
          return(standardize_ast(substitute(-y * x, list(x = ast[[2]],
                                                         y = multiply))))
        }
        if (as.character(ast[[1]]) == "+") {
          new_ast <- substitute(x * y + x * z,
                                list(x = multiply, y = ast[[2]], z = ast[[3]]))
        } else {
          new_ast <- substitute(x1 * y + x2 * z,
                                list(x1 = multiply, x2 = -1 * multiply,
                                     y = ast[[2]], z = ast[[3]]))
        }
        # also try to evaluate the two branches, maybe it works
        new_ast[[2]] <- try_eval_exp(new_ast[[2]])
        new_ast[[3]] <- try_eval_exp(new_ast[[3]])
        standardize_ast(new_ast)
      } else {
        if (length(ast) == 2 && as.character(ast[[1]]) == "-") {
          # convert -x to -1 * x
          standardize_ast(substitute(-1 * x, list(x = ast[[2]])))
        } else if (as.character(ast[[1]]) == "-") {
          standardize_ast(substitute(x + -1 * y,
                                     list(x = ast[[2]], y = ast[[3]])))
        } else {
          new_ast <- ast
          new_ast[[2]] <- standardize_ast(try_eval_exp(new_ast[[2]]))
          new_ast[[3]] <- standardize_ast(try_eval_exp(new_ast[[3]]))
          new_ast
        }
      }
    } else if (as.character(ast[[1]]) == "(") {
      standardize_ast(ast[[2]], multiply)
    } else {
      ast
    }
  } else {
    ast
  }
}

#standardize_ast(substitute(3 * x[1]))
#standardize_ast(substitute(3 * (y + x)))
#standardize_ast(substitute((y + x) * 3))
#standardize_ast(substitute((y - x + z + 10) * 3))
#standardize_ast(substitute(((y - x) / 3 + z + 10) * 3))
#standardize_ast(substitute(-3 * x[3] - 23))

# extracts the coefficients out of an ast
# the expression should be a simple sum
# only multiplication and + operators are allowed
# for each multiplication operation, one operand must be a numeric
# the other a non-numeric
#' @export
extract_coefficients <- function(ast) {
  extr_coef_internal <- function(the_ast) {
    if (is.call(the_ast)) {
      operator <- the_ast[[1]]
      if (as.character(operator) == "+") {
        left <- extr_coef_internal(try_eval_exp(the_ast[[2]]))
        right <- extr_coef_internal(try_eval_exp(the_ast[[3]]))
        list(
          constant = left$constant + right$constant,
          coefficients = c(left$coefficients, right$coefficients)
        )
      } else if (as.character(operator) == "*") {
        the_ast[[2]] <- try_eval_exp(the_ast[[2]])
        the_ast[[3]] <- try_eval_exp(the_ast[[3]])
        left_is_numeric <- is.numeric(the_ast[[2]])
        if (left_is_numeric) {
          numeric_idx <- 2
          expr_idx <- 3
        } else {
          numeric_idx <- 3
          expr_idx <- 2
        }
        # What the hell
        coef <- try(as.numeric(the_ast[[numeric_idx]]), silent = TRUE)
        if (!is.numeric(coef)) coef <- 0
        coefficent <- list(ast = the_ast[[expr_idx]],
                                   coef = coef)
        list(
          constant = 0,
          coefficients = list(coefficent)
        )
      } else if (as.character(operator) == "[") {
        list(constant = 0,
             coefficients = list(list(ast = the_ast, coef = 1)))
      } else {
        stop(paste0("Unexpected operator '", operator,"' found."))
      }
    } else {
      if (is.numeric(the_ast)) {
        list(constant = as.numeric(the_ast), coefficients = list())
      } else {
        list(constant = 0,
             coefficients = list(list(ast = the_ast, coef = 1)))
      }
    }
  }
  result <- extr_coef_internal(ast)

  # last step is to reduce the coefficients
  result$coefficients <- Reduce(function(acc, el) {
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
  }, result$coefficients, init = list())
  result
}
