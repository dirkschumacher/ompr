#' Create a new MIP Model
#' @include model-api.R
#' @export
MIPModel <- function() structure(list(variables = list(),
                                      objective = NULL,
                                      constraints = list()),
                                 class = c("optimization_model",
                                           "abstract_model"))

#' An S3 class to represent a Variable.
#'
#' This is class should only directly be used if you develop your own solver.
#'
#' @param arity the number of subscripts the variable has
#' @param type the type of the variable must be one of
#'        (binary, continuous, integer). Default is continuous.
#' @param lb an optional lower bound. No value means unbounded.
#' @param ub an optional upper bound. No value means unbounded.
#' @param instances a character vector with an entry for each
#'        variable instance.
#'
#' @noRd
new_variable <- function(arity, type, instances, lb, ub,
                         variable_expression, variable_quantifiers) {
  stopifnot(length(arity) == 1)
  stopifnot(length(type) == 1)
  stopifnot(arity >= 0)
  stopifnot(type %in% c("binary", "continuous", "integer"))
  structure(list(arity = arity,
                 type = type, instances = instances,
                 lb = lb, ub = ub),
            class = "model_variable")
}

#' @export
add_variable_.optimization_model <- function(.model, .variable, ...,
                                             type = "continuous",
                                             lb = -Inf, ub = Inf, .dots) {
  if (length(lb) != 1 || length(ub) != 1) {
    stop("lb and ub must be of length 1. I.e. just a single number.",
         call. = FALSE)
  }
  check_bounds(lb, ub)
  if (length(type) != 1 || !type %in% c("continuous", "binary", "integer")) {
    stop(paste0("The type of a variable needs to be either",
                " continuous, binary or integer."), call. = FALSE)
  }
  if (type == "binary") {
    if (is.infinite(lb)) {
      lb <- 0
    }
    if (is.infinite(ub)) {
      ub <- 1
    }
    if (!lb %in% c(0, 1)) {
      warning(paste0("lower bound of binary variable can ",
                     "either be 0 or 1. Setting it to 0"), call. = FALSE)
      lb <- 0
    }
    if (!ub %in% c(0, 1)) {
      warning(paste0("upper bound of binary variable can ",
                     "either be 0 or 1. Setting it to 1"), call. = FALSE)
      ub <- 1
    }
  }
  variable <- lazyeval::as.lazy(.variable)
  model <- .model
  expr <- variable$expr
  if (lazyeval::is_name(expr)) {
    var_name <- as.character(expr)
    var <- new_variable(arity = 0L,
                        type = type, instances = "",
                        lb = lb, ub = ub
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
    bound_subscripts <- lapply(classified_quantifiers$quantifiers,
                               lazyeval::lazy_eval)
    bound_expr <- bind_expression(var_name, expr, variable$env,
                                  bound_subscripts)
    arity <- as.integer(length(bound_expr) - 2)

    # then check if all free variables are present in "..."
    subscripts <- lapply(3:length(bound_expr),
                         function(x) as.character(bound_expr[x]))
    bound_subscripts <- bound_subscripts[
      names(bound_subscripts) %in% subscripts]
    replacement_idxs <- subscripts %in% names(bound_subscripts)
    subscripts[replacement_idxs] <- bound_subscripts

    # now generate all variables
    candidates <- build_quantifier_candidates(subscripts,
                                              names(bound_subscripts),
                                              classified_quantifiers$filters)
    zero_vars_msg <- paste0("The number of different indexes for variable ",
                            var_name, " is 0.")
    validate_quantifier_candidates(candidates, zero_vars_msg)
    n_vars <- nrow(candidates)
    instances <- apply(candidates, 1, function(row) {
      paste0(as.integer(row), collapse = "_")
    })
    var <- new_variable(
      arity = arity,
      type = type,
      instances = instances,
      lb = rep.int(lb, n_vars),
      ub = rep.int(ub, n_vars))
    model$variables[[var_name]] <- var
  } else {
    stop(paste0("The variable definition does not seem to be right.",
                " Take a look at the example models on the website on how",
                " to formulate variables"), call. = FALSE)
  }
  model
}

#' @export
set_bounds_.optimization_model <- function(.model, .variable, ...,
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
    bound_subscripts <- lapply(classified_quantifiers$quantifiers,
                               lazyeval::lazy_eval)
    quantifier_combinations <- build_quantifier_candidates(bound_subscripts,
                                                           names(bound_subscripts),
                                                           classified_quantifiers$filters)
    zero_vars_msg <- paste0("The number of different indexes for set_bounds ",
                            "for variable ", var_name, " is 0.")
    if (quantified_indexes) {
      validate_quantifier_candidates(quantifier_combinations, zero_vars_msg)
    }

    # now we have a pool of quantifiers
    # let's now generate combinations of variable indexes
    bound_subscripts <- setNames(lapply(index_names, function(x) {
      index_value <- suppressWarnings(as.integer(x))
      if (is.integer(index_value) && !is.na(index_value)) {
        index_value
      } else {
        i_name <- as.character(x)
        if (!i_name %in% colnames(quantifier_combinations)) {
          stop(paste0("Index ", i_name, " not bound by quantifier"),
               call. = FALSE)
        }
        quantifier_combinations[[i_name]]
      }
    }), index_names)

    bound_subscripts <- as.data.frame(bound_subscripts)
    indexes <- apply(bound_subscripts, 1, as.list)

    instance_keys <- vapply(indexes, function(x) {
      paste0(x, collapse = "_")
    }, character(1))
    var_indexes <- which(model_variable$instances %in% instance_keys)
    if (any(!instance_keys %in% model_variable$instances)) {
      stop("Indexed variable out of bounds.", call. = FALSE)
    }
    if (replace_lb) {
      model_variable$lb[var_indexes] <- lb
    }
    if (replace_ub) {
      model_variable$ub[var_indexes] <- ub
    }
    model$variables[[var_name]] <- model_variable
  }
  model
}

#' @export
set_objective_.optimization_model <- function(model, expression,
                                              sense = c("max", "min")) {
  stopifnot(length(expression) != 1)
  expression <- lazyeval::as.lazy(expression)
  sense <- match.arg(sense)
  obj_ast <- expression$expr
  ast <- normalize_expression(model, obj_ast, expression$env)
  var_names <- names(model$variables)
  if (is_non_linear(var_names, ast)) {
    stop(paste0("The objective is probably non-linear. ",
                "Currently, only linear functions are supported."),
         call. = FALSE)
  }
  obj <- new_objective_function(
    expression = as.expression(ast),
    original_expression = as.expression(obj_ast),
    sense = sense)
  model$objective <- obj
  model
}

#' @export
#' @inheritParams print
#' @importFrom stats setNames
print.optimization_model <- function(x, ...) {
  print_model(x)
}

#' @export
add_constraint_.optimization_model <- function(.model,
                                               .constraint_expr,
                                               ...,
                                               .dots,
                                               .show_progress_bar = TRUE) {
  constraint_expr <- lazyeval::as.lazy(.constraint_expr)
  model <- .model
  constraint_ast <- constraint_expr$expr
  if (length(constraint_ast) != 3) {
    stop("constraint not well formed. Must be a linear (in)equality.",
         call. = FALSE)
  }
  sense <- as.character(constraint_ast[[1]])
  if (!sense %in% c(">=", "<=", "==")) {
    stop("Does not recognize constraint expr. Missing the constraint relation",
         call. = FALSE)
  }
  lhs_ast <- constraint_ast[[2]]
  rhs_ast <- constraint_ast[[3]]
  parent_env <- constraint_expr$env
  lazy_dots <- lazyeval::lazy_dots(...)
  if (!missing(.dots)) {
    lazy_dots <- c(lazyeval::as.lazy_dots(.dots), lazy_dots)
  }
  classified_quantifiers <- classify_quantifiers(lazy_dots)
  bound_subscripts <- lapply(classified_quantifiers$quantifiers,
                             lazyeval::lazy_eval)
  add_constraint_internal <- function(envir = parent_env) {
    lhs_ast <- normalize_expression(model, lhs_ast, envir)
    rhs_ast <- normalize_expression(model, rhs_ast, envir)
    var_names <- names(model$variables)
    if (is_non_linear(var_names, lhs_ast)) {
      stop(paste0("The left-hand-side is probably non-linear. ",
                  "Currently, only linear constraints are ",
                  "supported."), call. = FALSE)
    }
    if (is_non_linear(var_names, rhs_ast)) {
      stop(paste0("The right-hand-side is probably non-linear. ",
                  "Currently, only linear constraints are ",
                  "supported."), call. = FALSE)
    }
    new_constraint(lhs = as.expression(lhs_ast),
                   rhs = as.expression(rhs_ast),
                   sense = sense)
  }
  constraints <- model$constraints
  if (is.list(bound_subscripts) && length(bound_subscripts) > 0) {
    filter_fn <- function(x) is.numeric(x) & length(x) > 0
    bound_subscripts <- Filter(filter_fn, bound_subscripts)
    var_combinations <- build_quantifier_candidates(bound_subscripts,
                                                    names(bound_subscripts),
                                                    classified_quantifiers$filters)
    zero_vars_msg <- "The number of different indexes is 0 for this constraint"
    validate_quantifier_candidates(var_combinations, zero_vars_msg)

    # let's init a progress bar
    progress_format <- "  adding constraints [:bar] :percent eta :eta"
    p <- progress::progress_bar$new(total = nrow(var_combinations),
                                    format = progress_format)
    p$tick(0)
    new_constraints <- apply(var_combinations, 1, function(row) {
      calling_env <- as.environment(as.list(row))
      parent.env(calling_env) <- parent_env
      constraint <- add_constraint_internal(calling_env)
      if (.show_progress_bar) {
        p$tick()
      }
      constraint
    })
    constraints <- c(constraints, new_constraints)
  } else {
    constraints <- c(constraints, list(add_constraint_internal()))
  }
  model$constraints <- constraints
  model
}

#' @export
variable_keys.optimization_model <- function(model) {
  if (length(model$variables) == 0) {
    return(character(0))
  }
  unlist(lapply(sort(names(model$variables)),
                function(x) {
                  var <- model$variables[[x]]
                  if (var$arity > 0) {
                    vapply(var$instances, function(var_code) {
                      splitted_els <- strsplit(var_code, "_", fixed = TRUE)[[1]]
                      paste0(x, "[",
                             paste0(splitted_els[seq_len(length(splitted_els))],
                                    collapse = ","),
                             "]")
                    }, character(1))
                  } else {
                    x
                  }
                }), use.names = FALSE)
}

# helper function that creates a function
# that can extract coefficients and constants
# out of an expression
build_coefficent_vector_fun <- function(model_var_keys) {
  n_cols <- length(model_var_keys)
  function(extracted_coefficients) {
    coef_vector <- Matrix::spMatrix(1, n_cols)
    coefficients <- extracted_coefficients
    names(coefficients) <- NULL
    bound_coefs <- unlist(Map(function(var_coef) {
      var_ast <- var_coef$ast
      if (is.call(var_ast) && length(var_ast) > 1) {
        var_name <- as.character(var_ast[[2]])
        search_key <- paste0(var_name, "[",
                             paste0(as.character(var_ast[3:length(var_ast)]),
                                    collapse = ","), "]")
      } else {
        var_name <- as.character(var_ast)
        search_key <- var_name
      }
      setNames(var_coef$coef, search_key)
    }, coefficients))
    coef_positions <- match(names(bound_coefs), model_var_keys)
    coef_positions <- coef_positions[!is.na(coef_positions)]
    if (length(coef_positions) > 0) {
      coef_vector[coef_positions] <- as.numeric(bound_coefs)
    }
    coef_vector
  }
}

#' @export
objective_function.optimization_model <- function(model) {
  objective <- model$objective
  has_objective <- !is.null(objective)
  build_coefficent_vector <- build_coefficent_vector_fun(variable_keys(model))
  if (has_objective) {
    coefficients <- extract_coefficients_internal(
      model$objective$expression[[1]])
    obj_constant <- coefficients$constant
    if (!is.numeric(obj_constant)) obj_constant <- 0
    coefficients <- coefficients$coefficients
    names(coefficients) <- NULL
    obj_vector <- build_coefficent_vector(coefficients)
    ordered_i <- order(obj_vector@j)
    obj_vector <- Matrix::sparseVector(x = obj_vector@x[ordered_i],
                                       i = obj_vector@j[ordered_i] + 1,
                                       length = ncol(obj_vector))
    list(solution = obj_vector, constant = obj_constant)
  } else {
    n_vars <- sum(unlist(nvars(model)))
    obj <- Matrix::sparseVector(integer(), integer(), n_vars)
    list(solution = obj, constant = 0)
  }
}


#' @export
extract_constraints.optimization_model <- function(model) {
  build_coefficent_vector <- build_coefficent_vector_fun(variable_keys(model))
  matrices <- lapply(model$constraints, function(constraint) {
    coefficients_lhs <- extract_coefficients_internal(constraint$lhs[[1]])
    coefficients_rhs <- extract_coefficients_internal(constraint$rhs[[1]])
    sense <- constraint$sense
    list(
      lhs = build_coefficent_vector(coefficients_lhs$coefficients),
      rhs = build_coefficent_vector(coefficients_rhs$coefficients),
      sense = sense,
      lhs_constant = coefficients_lhs$constant,
      rhs_constant = coefficients_rhs$constant
    )
  })
  constraint_matrix <- Reduce(Matrix::rbind2, lapply(matrices, function(constraint) {
    constraint$lhs - constraint$rhs
  }))

  # build row upper bound (aka b)
  constraint_rhs <- vapply(matrices, function(constraint) {
    constraint$rhs_constant - constraint$lhs_constant
  }, numeric(1L))

  constraint_dir <- vapply(matrices, function(constraint) {
    constraint$sense
  }, character(1L))
  list(
    matrix = constraint_matrix,
    sense = constraint_dir,
    rhs = constraint_rhs
  )
}

#' @export
nvars.optimization_model <- function(model) {
  stopifnot(is.list(model$variables))
  mapped_vars <- Map(f = function(var) {
    setNames(length(var$instances), var$type)
  }, model$variables)
  Reduce(f = function(acc, el) {
    acc[[names(el)]] <- acc[[names(el)]] + as.numeric(el)
    acc
  }, mapped_vars, init = list(continuous = 0, integer = 0,
                              binary = 0))
}

#' @export
variable_types.optimization_model <- function(model) {
  vars <- model$variables
  if (length(vars) == 0) {
    return(factor())
  }
  factor(unlist(lapply(sort(names(model$variables)), function(key) {
    var <- vars[[key]]
    rep.int(x = var$type, times = length(var$instances))
  })))
}

# a function to extract the lower/upper bounds of variables
extract_var_bounds_fun <- function(type) {
  stopifnot(type %in% c("lb", "ub"))
  default_val <- if (type == "lb") -Inf else Inf
  function(variables, keys) {
    unlist(lapply(keys, function(key) {
      var <- variables[[key]]
      bound <- if (length(var[[type]]) == 0) default_val else var[[type]]
      is_binary_var <- length(var[[type]]) == 0 && var$type == "binary"
      if (is_binary_var && type == "lb") bound <- 0
      if (is_binary_var && type == "ub") bound <- 1
      bound
    }))
  }
}

#' @export
variable_bounds.optimization_model <- function(model) {
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
nconstraints.optimization_model <- function(model) {
  length(model$constraints)
}
