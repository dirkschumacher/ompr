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

bind_expression <- function(var_name, exp, envir, bound_subscripts) {
  var_values <- as.list(envir)
  for (x in c(var_name, names(bound_subscripts))) {
    var_values[[x]] <- NULL
  }
  eval(substitute(substitute(x, var_values), list(x = exp)))
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
# it walks through the ast and changes sub trees
# iterator based and not recusrive
standardize_ast <- function(ast) {
  stack <- rstackdeque::rstack()
  push <- function(x) stack <<- rstackdeque::insert_top(stack, x)
  push(list(ast = ast, path = c(), multiplier = NULL))
  inplace_update_ast <- function(path, value) {
    # update the ast in place
    # it constracts a call like ast[[2]][[2]] <<- 42
    the_call <- substitute(ast)
    if (length(path > 0)) {
      for (i in 1:length(path)) {
        the_call <- substitute(x[[y]], list(x = the_call, y = path[i]))
      }
    }
    eval(substitute(x <<- substitute(y), list(x = the_call, y = value)))
  }

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
  while (length(stack) > 0) {
    element <- rstackdeque::peek_top(stack)
    stack <- rstackdeque::without_top(stack)
    local_ast <- element$ast
    path <- element$path
    multiplier <- element$multiplier
    need_multiplication <- is.numeric(multiplier)
    mult_num <- if (is.numeric(multiplier)) multiplier else 1
    if (is.call(local_ast)) {
      operator <- as.character(local_ast[[1]])
      ast_length <- length(local_ast)
      if (operator == "(") {
        push_idx(local_ast[[2]], path, multiplier = multiplier)
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
            new_ast <- substitute(x * y + x * z,
                                  list(x = multiplier, y = local_ast[[2]],
                                       z = local_ast[[3]]))
          } else if (operator == "-") {
            new_ast <- substitute(x1 * y + x2 * z,
                                  list(x1 = multiplier, x2 = -1 * multiplier,
                                       y = local_ast[[2]],
                                       z = local_ast[[3]]))
          }

          # also try to evaluate the two branches, maybe we can simplify sth.
          new_ast[[2]] <- try_eval_exp(new_ast[[2]])
          new_ast[[3]] <- try_eval_exp(new_ast[[3]])

          # update the ast and continue to traverse
          inplace_update_ast(c(path), new_ast)
          push_idx(new_ast, path)
        } else if (operator == "-") {
          new_ast <- substitute(x + -1 * y, list(x = local_ast[[2]],
                                                 y = local_ast[[3]]))
          inplace_update_ast(c(path), new_ast)
          push_idx(new_ast, path)
        } else {
          continue_traversal(local_ast, path)
        }
      } else if (operator %in% c("*", "/")) {
        left_is_num <-  is.numeric(local_ast[[2]])
        right_is_num <-  is.numeric(local_ast[[3]])
        is_multiplication <- operator == "*"
        if (!left_is_num && !right_is_num) {
          # this means either it is non-linear
          # or we need to further evaluate one of the branches
          continue_traversal(local_ast, path)
        } else if (left_is_num && right_is_num) {
          inplace_update_ast(path, try_eval_exp(local_ast))
        } else if (!is_multiplication ) {
          # if it is a devision, let's make it a multiplication
          if (left_is_num && !right_is_num) {
            replacement <- list(x = mult_num / local_ast[[2]],
                                y = local_ast[[3]])
          } else if (!left_is_num && right_is_num) {
            replacement <- list(x = local_ast[[2]],
                                y =  mult_num / local_ast[[3]])
          }
          new_ast <- substitute(y * x, replacement)
          push_idx(new_ast, c(path))
        } else {
          # multiplication & one of the sides is numeric
          if (left_is_num) {
            num_idx <- 2
            term_idx <- 3
          } else {
            num_idx <- 3
            term_idx <- 2
          }
          new_multiplier <- local_ast[[num_idx]]
          if (!is.null(multiplier)) {
            new_multiplier <- new_multiplier * multiplier
          }
          term <- local_ast[[term_idx]]
          push_idx(term, path, new_multiplier)
        }
      }
    }
  }
  ast
}

normalize_expression <- function(model, expression, envir) {
  ast <- bind_variables(model, expression, envir)
  ast <- try_eval_exp_rec(ast, envir)
  check_expression(model, ast)
  ast <- standardize_ast(ast)
  ast
}

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
        stop(paste0("Unexpected operator '", operator, "' found."))
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

#' @export
sum_exp <- function(exp, free_vars = c(), ...) {
  # TODO: This should probably be moved into the model
  # TODO: Make sure the input is correct
  # TODO: Take model and make sure
  # we do not overwrite any model variables
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
