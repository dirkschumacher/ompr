

# this is a generic function to iterativly traverse an AST
# in a pre-order way.
ast_walker <- function(ast, on_element) {
  # TODO: do not save ast in element
  # rather just use the path to query ast on demand
  stack <- rstackdeque::rstack()
  push <- function(x) stack <<- rstackdeque::insert_top(stack, x)
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
  while (length(stack) > 0) {
    element <- rstackdeque::peek_top(stack)
    stack <- rstackdeque::without_top(stack)
    on_element(push, inplace_update_ast, get_ast_value, element)
  }
  ast
}

try_eval_exp <- function(ast, envir = baseenv()) {
  result <- try(eval(ast, envir = envir), silent = TRUE)
  if (!is.numeric(result)) {
    ast
  } else {
    result
  }
}

try_eval_exp_rec <- function(base_ast, envir = baseenv()) {
  on_element <- function(push, inplace_update_ast, get_ast_value, element) {
    path <- element$path
    ast <- if (is.null(element$ast)) get_ast_value(path) else element$ast
    stop_traversal <- element$stop_traversal
    is_final_sum_exp_call <- element$is_final_sum_exp_call
    if (is.null(stop_traversal)) {
      stop_traversal <- FALSE
    }
    if (is.null(is_final_sum_exp_call)) {
      is_final_sum_exp_call <- FALSE
    }
    if (!is.call(ast)) {
      # let's try to evaluate the whole sub-tree
      new_ast_eval <- try_eval_exp(ast, envir)
      if (is.numeric(new_ast_eval)) {
        inplace_update_ast(path, new_ast_eval)
      }
    } else if (is.call(ast)) {
      if (as.character(ast[[1]]) == "sum_exp") {
        # we have a sum, let's expand it
        # TODO: detect that automatically
        # we need to evaluate anything from argument 2 onwards
        # e.g. sum_exp(x[i], i = 1:n) <- the n needs to be bound
        if (!is_final_sum_exp_call) {
          push(list(path = path, is_final_sum_exp_call = TRUE))
          for (i in 3:length(ast)) {
            new_element <- list(
              ast = ast[[i]],
              path = c(path, i)
            )
            push(new_element)
          }
        } else {
          # this expands the sum_exp expression
          # and triggers a reevaluation
          expanded_ast <- eval(ast)
          inplace_update_ast(path, expanded_ast)
          push(list(ast = expanded_ast, path = path))
        }
      } else if (!stop_traversal) {
        # we need to revisit the same node after the updates
        push(list(ast = ast, path = path, stop_traversal = TRUE))
        for (i in 2:length(ast)) {
          new_element <- list(ast = ast[[i]],
                              path = c(path, i))
          push(new_element)
        }
      } else {
        new_ast_eval <- try_eval_exp(ast, envir)
        if (is.numeric(new_ast_eval)) {
          inplace_update_ast(path, new_ast_eval)
        }
      }
    } else {
      stop("Does not compute.")
    }
  }
  ast_walker(base_ast, on_element)
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
  unbounded_indexes <- FALSE
  on_element <- function(push, inplace_update_ast, get_ast_value, element) {
    local_ast <- element$ast
    path <- element$path
    if (is.call(local_ast)) {
      if (local_ast[[1]] == "[") {
        for (i in 3:length(local_ast)) {
          if (is.name(local_ast[[i]])) {
            return(unbounded_indexes <<- TRUE)
          }
        }
      } else if (!unbounded_indexes) {
        for (i in 3:length(local_ast)) {
          push(list(ast = local_ast[[i]], path = c(path, i)))
        }
      }
    }
  }
  ast_walker(ast, on_element)
  unbounded_indexes
}

check_expression <- function(model, the_ast) {
  on_element <- function(push, inplace_update_ast, get_ast_value, element) {
    ast <- element$ast
    path <- element$path
    if (is.call(ast) && ast[[1]] == "[" &&
        class(ast[[2]]) == "name") {
      var_name <- as.character(ast[[2]])
      search_key <- paste0(as.character(ast[3:length(ast)]),
                           collapse = "_")
      var <- model@variables[[var_name]]
      if (!is.null(var) && !search_key %in% var@instances) {
        stop(paste0("The expression contains a variable,",
                    " that is not part of the model."))
      }
    } else if (is.call(ast)) {
      for (i in 2:length(ast)) {
        push(list(ast = ast[[i]], path = c(path, i)))
      }
    }
  }
  invisible(ast_walker(the_ast, on_element))
}

is_non_linear <- function(var_names, ast) {
  contains_vars <- function(le_ast) {
    vars_found <- FALSE
    on_element <- function(push, inplace_update_ast, get_ast_value, element) {
      local_ast <- element$ast
      path <- element$path
      if (is.call(local_ast) && !vars_found) {
        for (i in 2:length(local_ast)) {
          push(list(ast = local_ast[[i]], path = c(path, i)))
        }
      } else if (is.name(local_ast) && !vars_found) {
        vars_found <<- all(as.character(local_ast) %in% var_names)
      }
    }
    ast_walker(le_ast, on_element)
    vars_found
  }
  non_linear <- FALSE
  on_element <- function(push, inplace_update_ast, get_ast_value, element) {
    local_ast <- element$ast
    path <- element$path
    if (non_linear) {
      return()
    }
    if (is.call(local_ast) &&
        as.character(local_ast[[1]]) %in% c("*", "/", "^")) {
      non_linear <<- contains_vars(local_ast[[2]]) &&
        contains_vars(local_ast[[3]])
    } else if (is.call(local_ast)) {
      for (i in 2:length(local_ast)) {
        push(list(ast = local_ast[[i]], path = c(path, i)))
      }
    }
  }
  ast_walker(ast, on_element)
  non_linear
}

# this function takes an ast and transforms it
# into a an ast that only constists of either symbols,
# numerics, or muliplications of (numer & symbol)
# it walks through the ast and changes sub trees
# iterator based and not recusrive
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
  ast_walker(ast, on_element)
}

normalize_expression <- function(model, expression, envir) {
  ast <- bind_variables(model, expression, envir)
  if (!is.environment(envir)) {
    ast <- try_eval_exp_rec(ast, envir)
  } # otherwise this has been done in bind_variables
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
