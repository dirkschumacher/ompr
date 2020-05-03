#' Experimental: Create a new MILP Model
#'
#' Create an an empty mixed-integer linear programming model that is
#' about 1000 times faster than `MIPModel`.
#' It will eventually replace the old `MIPModel` backend for linear models.
#'
#' Please only use it if you can deal with potential API changes in the future.
#' When you use `MILPModel` make sure to always model your problem with
#' `MIPModel` as well, just to make sure you get the same results.
#'
#' It is also always a good idea to test your model with very small input sizes
#' and examine the coefficients and rows of the constraint matrix.
#'
#' @export
MILPModel <- function() {
  structure(list(
    variables = list(),
    var_index_mapping_list = list(),
    var_index_mapping = function(x) NULL,
    objective = NULL,
    constraints = list()
  ),
  class = "milp_model"
  )
}

new_milp_variable <- function(name, arity, type, lb, ub, index_mapping_dt,
                              index_mapping) {
  if (arity == 0L) {
    variable <- new("LinearVariableCollection", index_mapping = index_mapping)
    var <- as.name(name)
    expr <- rlang::quo(`[`(!!var, 1L))
    variable <- rlang::eval_tidy(expr, data = setNames(list(variable), name))
    list(
      name = name,
      arity = arity,
      type = type,
      lb = lb,
      ub = ub,
      index_mapping = index_mapping_dt,
      variable = variable
    )
  } else {
    list(
      name = name,
      arity = arity,
      type = type,
      lb = lb,
      ub = ub,
      index_mapping = index_mapping_dt,
      variable = new("LinearVariableCollection", index_mapping = index_mapping)
    )
  }
}


#' @export
add_variable_.milp_model <- function(.model, .variable, ...,
                                     type = "continuous",
                                     lb = -Inf, ub = Inf, .dots) {
  if (length(type) != 1 || !type %in% c("continuous", "binary", "integer")) {
    stop(paste0(
      "The type of a variable needs to be either",
      " continuous, binary or integer."
    ), call. = FALSE)
  }
  check_bounds(lb, ub)

  if (type == "binary") {
    lb_inf <- is.infinite(lb)
    if (lb_inf) {
      lb[lb_inf] <- 0L
    }
    ub_inf <- is.infinite(ub)
    if (ub_inf) {
      ub[ub_inf] <- 1L
    }
    lb <- as.integer(lb)
    ub <- as.integer(ub)
    lb_out_of_bounds <- is.na(lb) | lb < 0L | lb > 1L
    ub_out_of_bounds <- is.na(ub) | ub < 0L | ub > 1L
    if (any(lb_out_of_bounds)) {
      warning(paste0(
        "lower bound of binary variable can ",
        "either be 0 or 1. Setting it to 0"
      ), call. = FALSE)
      lb[lb_out_of_bounds] <- 0L
    }
    if (any(ub_out_of_bounds)) {
      warning(paste0(
        "upper bound of binary variable can ",
        "either be 0 or 1. Setting it to 1"
      ), call. = FALSE)
      ub[ub_out_of_bounds] <- 1L
    }
  }

  variable <- lazyeval::as.lazy(.variable)
  model <- .model
  expr <- variable$expr
  if (lazyeval::is_name(expr)) {
    assert_var_bounds_length_1(lb, ub)
    var_name <- as.character(expr)
    model$var_index_mapping_list[[var_name]] <- data.table::data.table(
      variable = var_name,
      V1 = 1L, col = 1L
    )
    model$var_index_mapping <- function(x) model$var_index_mapping_list[[x]]
    var <- new_milp_variable(var_name,
      arity = 0L,
      type = type,
      lb = lb,
      ub = ub,
      index_mapping_dt = model$var_index_mapping(var_name),
      index_mapping = model$var_index_mapping
    )
    model$variables[[var_name]] <- var
  } else if (lazyeval::is_call(expr) && expr[[1]] == "[") {

    # first we need to bind all variables
    var_name <- as.character(expr[[2]])

    lazy_dots <- lazyeval::lazy_dots(...)
    if (!missing(.dots)) {
      lazy_dots <- c(lazyeval::as.lazy_dots(.dots), lazy_dots)
    }

    classified_quantifiers <- classify_quantifiers(lazy_dots)
    bound_subscripts <- lapply(
      classified_quantifiers$quantifiers,
      lazyeval::lazy_eval
    )
    bound_expr <- bind_expression(
      var_name, expr, variable$env,
      bound_subscripts
    )
    arity <- length(bound_expr) - 2L

    # two options, either we have bounded indexes
    # or the variable is initialized directly with vectors
    # in any case we create a variable with the correct arity first and then
    # initialize it using standard evaluation
    no_bound_variables <- all(vapply(classified_quantifiers, length, integer(1L)) == 0L)
    if (no_bound_variables) {
      parent_env <- variable$env
      index_values <- lapply(seq_len(arity), function(i) {
        eval(bound_expr[[2L + i]], envir = parent_env)
      })
      candidates <- do.call(data.frame, setNames(index_values, paste0("V", seq_len(arity))))
      n_vars <- nrow(candidates)
    } else {
      assert_var_bounds_length_1(lb, ub)

      # then check if all free variables are present in "..."
      subscripts <- lapply(
        3:length(bound_expr),
        function(x) as.character(bound_expr[x])
      )
      bound_subscripts <- bound_subscripts[
        names(bound_subscripts) %in% subscripts
      ]
      replacement_idxs <- subscripts %in% names(bound_subscripts)
      subscripts[replacement_idxs] <- bound_subscripts
      subscripts <- suppressWarnings(lapply(subscripts, as.integer))

      # now generate all variables
      candidates <- build_quantifier_candidates(
        subscripts,
        names(bound_subscripts),
        classified_quantifiers$filters
      )
      n_vars <- nrow(candidates)
      zero_vars_msg <- paste0(
        "The number of different indexes for variable ",
        var_name, " is 0."
      )
      any_col_non_numeric <- any(vapply(seq_len(ncol(candidates)), function(j) {
        !is.numeric(candidates[[j]]) || anyNA(candidates[[j]])
      }, logical(1L)))

      if (n_vars == 0L) {
        stop("The number of different indexes for variable ",
          var_name, " is 0.",
          call. = FALSE
        )
      }
      if (any_col_non_numeric) {
        stop("At least one index of ", var_name, " is not an integer vector or not bound.",
          call. = FALSE
        )
      }
      colnames(candidates) <- paste0("V", seq_len(ncol(candidates)))
    }

    model$var_index_mapping_list[[var_name]] <- cbind(
      data.table::data.table(variable = var_name),
      data.table::as.data.table(candidates),
      data.table::data.table(col = seq_len(n_vars))
    )
    model$var_index_mapping <- function(x) model$var_index_mapping_list[[x]]

    if (length(lb) == 1L) lb <- rep.int(lb, n_vars)
    if (length(ub) == 1L) ub <- rep.int(ub, n_vars)
    var <- new_milp_variable(var_name,
      arity = arity,
      type = type,
      lb = lb,
      ub = ub,
      index_mapping_dt = model$var_index_mapping(var_name),
      index_mapping = model$var_index_mapping
    )
    model$variables[[var_name]] <- var
  } else {
    stop(paste0(
      "The variable definition does not seem to be right.",
      " Take a look at the example models on the website on how",
      " to formulate variables"
    ), call. = FALSE)
  }
  model
}

assert_var_bounds_length_1 <- function(lb, ub) {
  if (length(lb) != 1L || length(ub) != 1L) {
    stop("lb and ub must be of length 1. I.e. just a single number.",
      call. = FALSE
    )
  }
}

new_milp_constraint <- function(lhs, sense, rhs) {
  stopifnot(sense %in% c("<=", "==", ">="))
  structure(list(
    lhs = lhs,
    sense = sense,
    rhs = rhs
  ),
  class = "milp_model_constraint"
  )
}

create_model_env <- function(model) {
  as.environment(
    lapply(model$variables, function(x) x$variable)
  )
}

#' @export
add_constraint_.milp_model <- function(.model,
                                       .constraint_expr,
                                       ...,
                                       .dots,
                                       .show_progress_bar = TRUE) {
  if (any(.show_progress_bar != TRUE)) {
    warning("A progress bar is deprecated for MILPModel, please do not explicitly use it",
      call. = FALSE
    )
  }
  constraint_expr <- lazyeval::as.lazy(.constraint_expr)
  model <- .model
  constraint_ast <- constraint_expr$expr
  if (length(constraint_ast) != 3) {
    stop("constraint not well formed. Must be a linear (in)equality.",
      call. = FALSE
    )
  }
  sense <- as.character(constraint_ast[[1]])
  if (!sense %in% c(">=", "<=", "==")) {
    stop("Does not recognize constraint expr. Missing the constraint relation",
      call. = FALSE
    )
  }
  lhs_ast <- constraint_ast[[2]]
  rhs_ast <- constraint_ast[[3]]
  parent_env <- constraint_expr$env
  lazy_dots <- lazyeval::lazy_dots(...)
  if (!missing(.dots)) {
    lazy_dots <- c(lazyeval::as.lazy_dots(.dots), lazy_dots)
  }
  classified_quantifiers <- classify_quantifiers(lazy_dots)
  bound_subscripts <- lapply(
    classified_quantifiers$quantifiers,
    lazyeval::lazy_eval
  )

  var_envir <- create_model_env(model)
  eval_constraint <- function(lhs_ast, rhs_ast, sense, data = NULL, envir = parent_env) {
    parent.env(var_envir) <- parent_env
    var_envir$`sum_expr` <- sum_expr_milp
    lhs <- rlang::eval_tidy(lhs_ast, env = var_envir, data = data)
    rhs <- rlang::eval_tidy(rhs_ast, env = var_envir, data = data)
    new_milp_constraint(lhs, sense, rhs)
  }
  if (is.list(bound_subscripts) && length(bound_subscripts) > 0) {
    filter_fn <- function(x) is.numeric(x) & length(x) > 0
    bound_subscripts <- Filter(filter_fn, bound_subscripts)
    var_combinations <- build_quantifier_candidates(
      bound_subscripts,
      names(bound_subscripts),
      classified_quantifiers$filters
    )
    if (nrow(var_combinations) == 0L) {
      stop("The number of different indexes is 0 for this constraint", call. = FALSE)
    }
    constraint <- eval_constraint(lhs_ast, rhs_ast, sense, data = var_combinations)
  } else {
    constraint <- eval_constraint(lhs_ast, rhs_ast, sense)
  }
  model$constraints <- c(model$constraints, list(constraint))
  model
}


sum_expr_milp <- function(expr, ...) {
  ast <- rlang::enquo(expr)
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
  if (nrow(subscript_combinations) == 0L) {
    return(0)
  }
  for (col in colnames(subscript_combinations)) {
    subscript_combinations[[col]] <- as_colwise(subscript_combinations[[col]])
  }
  rlang::eval_tidy(ast, data = subscript_combinations)
}


new_milp_objective_function <- function(objective,
                                        original_expression,
                                        sense) {
  stopifnot(length(sense) == 1 &&
    sense %in% c("min", "max"))
  structure(list(
    objective = objective,
    original_expression = original_expression,
    sense = sense
  ), class = "milp_model_objective")
}

#' @export
set_objective_.milp_model <- function(model, expression,
                                      sense = c("max", "min")) {
  var_envir <- create_model_env(model)
  expression <- lazyeval::as.lazy(expression)
  parent.env(var_envir) <- expression$env
  var_envir$`sum_expr` <- sum_expr_milp
  expression <- rlang::as_quosure(expression$expr, var_envir)
  sense <- match.arg(sense)

  obj <- new_milp_objective_function(
    objective = rlang::eval_tidy(expression),
    original_expression = rlang::get_expr(expression),
    sense = sense
  )
  model$objective <- obj
  model
}

#' @export
solve_model.milp_model <- function(model, solver) {
  if (!is.function(solver)) {
    stop(paste0(
      "Solver is not a function Model -> Solution.\n",
      "Take a look at the examples on the website on how to call",
      " solve_model."
    ))
  }
  solver(model)
}


## model api

#' @export
nvars.milp_model <- function(model) {
  stopifnot(is.list(model$variables))
  mapped_vars <- Map(f = function(var) {
    setNames(nrow(var$index_mapping), var$type)
  }, model$variables)
  Reduce(f = function(acc, el) {
    acc[[names(el)]] <- acc[[names(el)]] + as.numeric(el)
    acc
  }, mapped_vars, init = list(
    continuous = 0, integer = 0,
    binary = 0
  ))
}

variable_ordering <- function(model) {
  ordering <- data.table::rbindlist(lapply(sort(names(model$variables)), function(i) {
    x <- model$variables[[i]]
    x$index_mapping[, c("variable", "col"), with = FALSE]
  }))
  ordering[["order"]] <- seq_len(nrow(ordering))
  ordering
}

#' @export
variable_types.milp_model <- function(model) {
  vars <- model$variables
  if (length(vars) == 0) {
    return(factor())
  }
  factor(unlist(lapply(sort(names(model$variables)), function(key) {
    var <- vars[[key]]
    rep.int(x = var$type, times = nrow(var$index_mapping))
  })))
}

#' @export
variable_keys.milp_model <- function(model) {
  if (length(model$variables) == 0) {
    return(character(0))
  }
  unlist(lapply(
    sort(names(model$variables)),
    function(x) {
      var <- model$variables[[x]]
      if (var$arity > 0) {
        apply(var$index_mapping[, paste0("V", seq_len(var$arity)), with = FALSE], 1, function(row) {
          paste0(x, "[", paste0(row, collapse = ","), "]")
        })
      } else {
        x
      }
    }
  ), use.names = FALSE)
}

#' @export
objective_function.milp_model <- function(model) {
  objective <- model$objective
  has_objective <- !is.null(objective)
  n_vars <- sum(unlist(nvars(model)))
  obj_constant <- if (is.numeric(model$objective$objective)) {
    model$objective$objective
  } else {
    0
  }
  if (has_objective) {
    ordering <- variable_ordering(model)
    if (inherits(objective$objective, "LinearVariableCollection")) {
      dt <- objective$objective@variables
    } else if (inherits(objective$objective, "LinearVariableSum")) {
      dt <- objective$objective@variables@variables
      if (nrow(objective$objective@constant) != 1L) {
        stop("It appears the objective function has multiple rows. Something must be wrong", call. = FALSE)
      }
      obj_constant <- objective$objective@constant[["constant"]]
    } else if (is.numeric(objective$objective)) {
      obj <- Matrix::sparseVector(integer(), integer(), n_vars)
      return(list(solution = obj, constant = obj_constant))
    } else {
      stop("Not implemented", call. = FALSE)
    }
    coefs <- merge(ordering, dt, by = c("variable", "col"))
    obj_vector <- Matrix::sparseVector(
      x = coefs[["coef"]],
      i = coefs[["order"]],
      length = n_vars
    )
    list(solution = obj_vector, constant = obj_constant)
  } else {
    obj <- Matrix::sparseVector(integer(), integer(), n_vars)
    list(solution = obj, constant = obj_constant)
  }
}

#' @export
extract_constraints.milp_model <- function(model) {
  if (length(model$constraints) == 0L) {
    return(list(matrix = NULL, sense = character(0L), rhs = numeric(0L)))
  }

  constraints <- lapply(model$constraints, function(constraint) {
    lhs <- constraint$lhs - constraint$rhs

    # TODO: write generic method
    if (inherits(lhs, "LinearVariableCollection")) {
      var_collection <- lhs@variables
      rhs_constraint_dt <- data.table::data.table(
        row = unique(var_collection$row),
        constant = 0
      )
    } else if (inherits(lhs, "LinearVariableSum")) {
      var_collection <- lhs@variables@variables
      rhs_constraint_dt <- lhs@constant
      rhs_constraint_dt[["constant"]] <- rhs_constraint_dt[["constant"]] * -1
    } else {
      stop("not implemented")
    }
    ordering <- variable_ordering(model)
    coefs <- merge(ordering, var_collection, by = c("variable", "col"))
    n_rows <- length(unique(coefs[["row"]]))
    non_zero <- coefs[["coef"]] != 0
    mat <- Matrix::sparseMatrix(
      i = coefs[["row"]][non_zero],
      j = coefs[["order"]][non_zero],
      x = coefs[["coef"]][non_zero],
      dims = c(n_rows, nrow(ordering))
    )
    rhs_constraint_constant <- rep.int(0, n_rows)
    rhs_constraint_constant[rhs_constraint_dt[["row"]]] <- rhs_constraint_dt[["constant"]]
    constraint_dir <- rep.int(constraint$sense, n_rows)
    list(mat, constraint_dir, rhs_constraint_constant)
  })

  constraint_matrix <- Reduce(Matrix::rbind2, lapply(constraints, function(x) x[[1L]]))

  # build row upper bound (aka b)
  constraint_rhs <- Reduce(c, lapply(constraints, function(x) x[[3L]]))

  constraint_dir <- Reduce(c, lapply(constraints, function(x) x[[2L]]))
  list(
    matrix = constraint_matrix,
    sense = constraint_dir,
    rhs = constraint_rhs
  )
}

#' @export
variable_bounds.milp_model <- function(model) {
  model_vars <- model$variables
  if (length(model_vars) == 0) {
    return(list(lower = numeric(0), upper = numeric(0)))
  }
  extract_bounds_l <- extract_var_bounds_fun("lb")
  extract_bounds_u <- extract_var_bounds_fun("ub")
  keys <- sort(names(model$variables))
  list(
    lower = extract_bounds_l(model_vars, keys),
    upper = extract_bounds_u(model_vars, keys)
  )
}

#' @export
nconstraints.milp_model <- function(model) {
  mat <- extract_constraints(model)[["matrix"]]
  n <- dim(mat)[1L]
  if (!is.numeric(n)) 0L else n
}

#' @export
#' @inheritParams print
#' @importFrom stats setNames
print.milp_model <- function(x, ...) {
  print_model(x)
}

#' @export
set_bounds_.milp_model <- function(.model, .variable, ...,
                                   lb = NULL, ub = NULL, .dots) {
  if (is.numeric(lb) && is.numeric(ub)) {
    check_bounds(lb, ub)
  }
  variable <- lazyeval::as.lazy(.variable)
  model <- .model
  is_single_variable <- lazyeval::is_name(variable$expr)
  is_indexed_variable <- lazyeval::is_call(variable$expr) &&
    variable$expr[[1]] == "[" &&
    length(variable$expr) >= 3
  model_variable_names <- names(model$variables)
  replace_lb <- !is.null(lb) && is.numeric(lb)
  replace_ub <- !is.null(ub) && is.numeric(ub)
  if (is_single_variable) {
    var_name <- as.character(variable$expr)
    if (!var_name %in% model_variable_names) {
      stop("Variable does not exists in model", call. = FALSE)
    }
    variable <- model$variables[[var_name]]
    if (replace_lb) {
      variable$lb <- lb
    }
    if (replace_ub) {
      variable$ub <- ub
    }
    model$variables[[var_name]] <- variable
  } else if (is_indexed_variable) {
    var_name <- as.character(variable$expr[[2]])
    if (!var_name %in% model_variable_names) {
      stop("Variable does not exists in model", call. = FALSE)
    }
    model_variable <- model$variables[[var_name]]
    index_names <- vapply(3:length(variable$expr), function(i) {
      as.character(variable$expr[i])
    }, character(1))
    indexes <- suppressWarnings(as.integer(index_names))
    quantified_indexes <- !is.integer(indexes) || any(is.na(indexes))
    lazy_dots <- lazyeval::lazy_dots(...)
    if (!missing(.dots)) {
      lazy_dots <- c(lazyeval::as.lazy_dots(.dots), lazy_dots)
    }
    classified_quantifiers <- classify_quantifiers(lazy_dots)
    bound_subscripts <- lapply(
      classified_quantifiers$quantifiers,
      lazyeval::lazy_eval
    )
    quantifier_combinations <- build_quantifier_candidates(
      bound_subscripts,
      names(bound_subscripts),
      classified_quantifiers$filters
    )
    if (quantified_indexes && nrow(quantifier_combinations) == 0L) {
      stop("The number of different indexes for set_bounds ",
        "for variable ", var_name, " is 0.",
        call. = FALSE
      )
    }

    var_mapping <- model$var_index_mapping_list[[var_name]]
    var_envir <- create_model_env(model)
    parent.env(var_envir) <- variable$env
    expr <- rlang::as_quosure(variable$expr, var_envir)
    var_collection <- rlang::eval_tidy(expr,
      data = quantifier_combinations
    )
    if (!inherits(var_collection, "LinearVariableCollection")) {
      stop("You can only set bounds one variable at a time.", call. = FALSE)
    }

    var_indexes <- var_collection@variables$col
    if (replace_lb && any(var_indexes)) {
      model_variable$lb[var_indexes] <- lb
    }
    if (replace_ub && any(var_indexes)) {
      model_variable$ub[var_indexes] <- ub
    }
    model$variables[[var_name]] <- model_variable
  }
  model
}
