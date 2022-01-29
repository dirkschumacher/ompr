#' Create a new MIP Model
#' @export
MIPModel <- function() {
  structure(list(
    variables = list(), # a named list of variables. Either a variable or a collection of variables,
    variable_types = list(),
    variable_bounds_lower = numeric(),
    variable_bounds_upper = numeric(),
    column_count = 0,
    objective = NULL,
    constraints = list()
  ),
  class = c("linear_optimization_model", "abstract_model")
  )
}


#' @export
`[.OmprLinearVariableCollection` <- function(x, i, j, ..., drop) {
  indexes <- c(
    if (!missing(i)) i,
    if (!missing(j)) j,
    ...
  )
  var_name <- hash_var_indexes(indexes)
  var <- x$map$get(var_name)
  if (is.null(var)) {
    abort("Variable not found for indexes")
  }
  var
}

n_columns <- function(x) {
  UseMethod("n_columns")
}

#' @export
n_columns.OmprLinearVariableCollection <- function(x) {
  x$map$size()
}

#' @export
n_columns.OmprLinearVariable <- function(x) {
  1
}

#' @export
n_columns.LinearTerm <- function(x) {
  1
}

#' @export
#' @inheritParams print
print.linear_optimization_model <- function(x, ...) {
  print_model(x)
}

#' @export
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom rlang syms
#' @importFrom rlang get_expr
#' @importFrom listcomp gen_list
#' @importFrom fastmap fastmap
add_variable.linear_optimization_model <- function(.model, .variable, ...,
                                                   type = "continuous",
                                                   lb = -Inf, ub = Inf) {
  if (length(lb) != 1 || length(ub) != 1) {
    stop("lb and ub must be of length 1. I.e. just a single number.",
      call. = FALSE
    )
  }
  check_bounds(lb, ub)
  if (length(type) != 1 || !type %in% c("continuous", "binary", "integer")) {
    stop(paste0(
      "The type of a variable needs to be either",
      " continuous, binary or integer."
    ), call. = FALSE)
  }
  if (type == "binary") {
    if (is.infinite(lb)) {
      lb <- 0
    }
    if (is.infinite(ub)) {
      ub <- 1
    }
    if (!lb %in% c(0, 1)) {
      warning(paste0(
        "lower bound of binary variable can ",
        "either be 0 or 1. Setting it to 0"
      ), call. = FALSE)
      lb <- 0
    }
    if (!ub %in% c(0, 1)) {
      warning(paste0(
        "upper bound of binary variable can ",
        "either be 0 or 1. Setting it to 1"
      ), call. = FALSE)
      ub <- 1
    }
  }

  variable <- get_expr(enquo(.variable))
  dots <- enquos(...)
  model <- .model

  # first we need to extract the variable
  if (is.call(variable) && variable[[1]] != "[") {
    abort("The variable need to be in the form of <var>[<indexes>]")
  }
  is_indexed_var <- is.call(variable) && variable[[1]] == "["
  variable_base_name <- if (is_indexed_var) {
    as.character(variable[[2]])
  } else {
    as.character(variable)
  }
  variable_index_names <- if (is_indexed_var) {
    stopifnot(length(variable) >= 3)
    as.character(variable[3:length(variable)])
  }

  if (variable_base_name %in% names(model$variables)) {
    stop(
      "A variable called '", variable_base_name,
      "' is already part of the model"
    )
  }

  current_column_count <- model$column_count
  if (is_indexed_var) {
    stopifnot(
      !is.null(variable_index_names),
      all(variable_index_names != "")
    )
    vars <- gen_list(
      c(!!!syms(variable_index_names)), !!!dots,
      .env = parent.frame()
    )
    el_names <- lapply(vars, hash_var_indexes)
    model$variable_bounds_lower <- c(
      model$variable_bounds_lower,
      rep.int(lb, length(vars))
    )
    model$variable_bounds_upper <- c(
      model$variable_bounds_upper,
      rep.int(ub, length(vars))
    )
    vars <- lapply(vars, function(x) {
      current_column_count <<- current_column_count + 1
      var <- new_linear_variable(column_idx = current_column_count)
      new_linear_term(var, 1)
    })
    vars <- setNames(vars, el_names)
    map <- fastmap()
    map$mset(.list = vars)
    new_variable <- new_variable_collection(map = map)
  } else {
    current_column_count <- current_column_count + 1
    model$variable_bounds_lower[[current_column_count]] <- lb
    model$variable_bounds_upper[[current_column_count]] <- ub
    new_variable <- new_linear_variable(column_idx = current_column_count)
    new_variable <- new_linear_term(new_variable, 1)
  }
  model$column_count <- current_column_count
  model$variables[[variable_base_name]] <- new_variable
  model$variable_types[[variable_base_name]] <- type

  model
}

#' @export
add_variable_.linear_optimization_model <- function(.model, .variable, ...,
                                                    type = "continuous",
                                                    lb = -Inf, ub = Inf, .dots) {
  var <- to_quosure(as.lazy(.variable))
  dots <- capture_lazy_dots(.dots, ...)
  add_variable(.model,
    .variable = !!var, !!!dots,
    type = type, lb = lb, ub = ub
  )
}

#' @export
#' @importFrom rlang caller_env
set_objective.linear_optimization_model <- function(model,
                                                    expression,
                                                    sense = "max") {
  sense <- match.arg(sense, c("max", "min"))
  expression <- enquo(expression)
  obj_fun <- eval_in_model(model, get_expr(expression), caller_env())
  stopifnot(inherits(obj_fun, "LinearFunction") ||
    inherits(obj_fun, "LinearTerm") ||
    is.numeric(obj_fun))
  model$objective <- list(
    fun = obj_fun,
    sense = sense
  )
  model
}

#' @export
#' @importFrom lazyeval as.lazy
set_objective_.linear_optimization_model <- function(model,
                                                     expression,
                                                     sense = "max") {
  expression <- to_quosure(as.lazy(expression))
  set_objective(
    model = model,
    expression = !!expression,
    sense = sense
  )
}

`%||%` <- function(lhs, rhs) invisible(if (!lhs) rhs)

#' @export
#' @importFrom rlang caller_env
set_bounds.linear_optimization_model <- function(.model, .variable, ...,
                                                 lb = NULL, ub = NULL) {
  expr <- enquo(.variable)
  dots <- enquos(...)
  if (is.null(lb) && is.null(ub)) {
    set_bounds_ineq(.model, expr, dots = dots, env = caller_env())
  } else {
    set_bounds_old(
      .model, expr,
      dots = dots, lb = lb, ub = ub, env = caller_env()
    )
  }
}

#' @importFrom listcomp gen_list
set_bounds_ineq <- function(.model, .expr, dots, env) {
  eval_env <- build_model_environment(.model, env)
  constraints <- gen_list(!!get_expr(.expr), !!!dots, .env = eval_env)
  any_type_errors <- !all(vapply(
    constraints,
    function(x) {
      inherits(x, "LinearConstraint")
    }, logical(1L)
  ))
  if (any_type_errors) {
    abort(
      "Bounds need to be defined as linear constraints with a single",
      " variable on one side and a numeric value on the other"
    )
  }
  for (constraint in constraints) {
    bound <- extract_bound_from_constraint(
      constraint$lhs, constraint$rhs, constraint$sense
    )
    col_idx <- bound$variable$column_idx
    numeric_bound <- bound$bound
    if (inherits(bound$sense, "LinearConstraintSenseLeq")) {
      .model$variable_bounds_upper[[col_idx]] <- numeric_bound
    } else if (inherits(bound$sense, "LinearConstraintSenseGeq")) {
      .model$variable_bounds_lower[[col_idx]] <- numeric_bound
    } else if (inherits(bound$sense, "LinearConstraintSenseEq")) {
      .model$variable_bounds_lower[[col_idx]] <- numeric_bound
      .model$variable_bounds_upper[[col_idx]] <- numeric_bound
    } else {
      abort("Constraint type not implemented")
    }
  }
  .model
}

extract_bound_from_constraint <- function(lhs, rhs, sense) {
  UseMethod("extract_bound_from_constraint", lhs)
}

#' @export
extract_bound_from_constraint.LinearFunction <- function(lhs, rhs, sense) {
  terms <- terms_list(lhs)
  stopifnot(
    length(terms) == 1,
    lhs$constant == 0
  )
  extract_bound_from_constraint(terms[[1]], rhs, sense)
}

#' @export
extract_bound_from_constraint.LinearTerm <- function(lhs, rhs, sense) {
  term <- lhs
  coefficient <- term$coefficient
  if (coefficient < 0) {
    sense <- flip_constaint_sense(sense)
  }
  list(
    sense = sense,
    variable = term$variable,
    bound = rhs / coefficient
  )
}

#' @importFrom listcomp gen_list
set_bounds_old <- function(.model, expr, dots,
                           lb = NULL, ub = NULL, env = NULL) {
  is.null(lb) %||% stopifnot(
    is.numeric(lb), length(lb) == 1,
    !is.na(lb), is.finite(lb)
  )
  is.null(ub) %||% stopifnot(
    is.numeric(ub), length(ub) == 1,
    !is.na(ub), is.finite(ub)
  )
  eval_env <- build_model_environment(.model, env)
  vars <- gen_list(!!expr, !!!dots, .env = eval_env)
  for (var in vars) {
    if (inherits(var, "LinearTerm")) {
      var <- var$variable
    }
    col_idx <- var$column_idx
    if (!is.null(lb)) {
      .model$variable_bounds_lower[[col_idx]] <- lb
    }
    if (!is.null(ub)) {
      .model$variable_bounds_upper[[col_idx]] <- ub
    }
  }
  .model
}

#' @export
set_bounds_.linear_optimization_model <- function(.model, .variable, ...,
                                                  lb = NULL, ub = NULL,
                                                  .dots) {
  var <- to_quosure(as.lazy(.variable))
  dots <- capture_lazy_dots(.dots, ...)
  set_bounds(.model,
    .variable = !!var, !!!dots,
    lb = lb, ub = ub
  )
}

#' @importFrom rlang get_expr
#' @importFrom rlang eval_bare
#' @noRd
eval_in_model <- function(model, expression, parent_env) {
  eval_bare(
    get_expr(expression),
    env = build_model_environment(model, parent_env)
  )
}

#' @importFrom rlang new_environment
#' @noRd
build_model_environment <- function(model, parent_env) {
  data <- c(
    model$variables,
    list(
      sum_expr = sum_over,
      sum_over = sum_over
    )
  )
  new_environment(
    data = data,
    parent = parent_env
  )
}

#' Sum over indexes
#'
#' This functions helps to create summations over indexes.
#'
#' @param .expr an expression that can be expanded to a sum
#' @param ... bind variables in expr using dots. See examples.
#'
#' @return the sum over all the indexes
#'
#' @seealso \code{\link{add_constraint}}
#' @seealso \code{\link{set_objective}}
#'
#' Please note that \code{sum_expr} is deprecated when used together with
#' \code{MIPModel}.
#'
#' @examples
#' if (FALSE) {
#'   # create a sum from x_1 to x_10
#'   sum_over(x[i], i = 1:10)
#'   # create a sum from x_2 to x_10 with even indexes
#'   sum_over(x[i], i = 1:10, i %% 2 == 0)
#'   sum_over(x[i, j], i = 1:10, j = 1:i)
#' }
#' @export
#' @importFrom listcomp gen_list
sum_over <- function(.expr, ...) {
  vals <- gen_list(!!enquo(.expr), !!!enquos(...),
    .env = parent.frame()
  )
  res <- 0
  for (x in vals) {
    res <- res + x
  }
  res
}

#' @export
#' @rdname sum_over
sum_expr <- sum_over

#' @export
#' @importFrom rlang enquos
#' @importFrom rlang get_expr
#' @importFrom rlang enquo
#' @importFrom rlang caller_env
#' @importFrom listcomp gen_list
add_constraint.linear_optimization_model <- function(.model, .constraint_expr,
                                                     ...,
                                                     .show_progress_bar) {
  dots <- enquos(...)
  constraint_expr <- get_expr(enquo(.constraint_expr))
  eval_env <- build_model_environment(.model, parent_env = caller_env())
  constraints <- gen_list(!!constraint_expr, !!!dots, .env = eval_env)
  any_type_errors <- !all(vapply(
    constraints,
    function(x) {
      inherits(x, "LinearConstraint") ||
        (is.logical(x) && x)
    }, logical(1L)
  ))
  if (any_type_errors) {
    abort("Some constraints are not proper linear constraints or are not true.")
  }
  constraints <- Filter(function(x) {
    inherits(x, "LinearConstraint")
  }, constraints)
  .model$constraints <- c(.model$constraints, constraints) # O(n)
  .model
}

#' @importFrom rlang new_quosure
#' @importFrom lazyeval as.lazy_dots
#' @importFrom lazyeval lazy_dots
capture_lazy_dots <- function(.dots, ...) {
  lazy_dots <- lazy_dots(...)
  if (!missing(.dots)) {
    lazy_dots <- c(lazy_dots, as.lazy_dots(.dots))
  }
  lapply(lazy_dots, function(x) {
    new_quosure(x$expr, x$env)
  })
}

to_quosure <- function(expr) {
  new_quosure(expr$expr, expr$env)
}

#' @export
#' @importFrom lazyeval as.lazy
#' @importFrom rlang new_quosure
add_constraint_.linear_optimization_model <- function(.model,
                                                      .constraint_expr,
                                                      ...,
                                                      .dots,
                                                      .show_progress_bar = TRUE) {
  constraint_expr <- to_quosure(as.lazy(.constraint_expr))
  dots <- capture_lazy_dots(.dots, ...)
  add_constraint(
    .model = .model,
    .constraint_expr = !!constraint_expr,
    !!!dots
  )
}

hash_var_indexes <- function(indexes) {
  paste0(indexes, collapse = ",")
}

#' @export
nvars.linear_optimization_model <- function(model) {
  buckets <- list(
    continuous = 0,
    integer = 0,
    binary = 0
  )
  Reduce(function(buckets, var_name) {
    var_count <- n_columns(model$variables[[var_name]])
    var_type <- model$variable_types[[var_name]]
    buckets[[var_type]] <- buckets[[var_type]] + var_count
    buckets
  }, names(model$variables), init = buckets)
}

#' @export
nconstraints.linear_optimization_model <- function(model) {
  length(model$constraints)
}

#' @export
variable_types.linear_optimization_model <- function(model) {
  if (length(model$variables) == 0) {
    return(factor(levels = c("binary", "continuous", "integer")))
  }
  type_mapping <- lapply(names(model$variables), function(var_name) {
    type <- model$variable_types[[var_name]]
    variable <- model$variables[[var_name]]
    column_indexes <- if (inherits(variable, "LinearTerm")) {
      variable$variable$column_idx
    } else {
      vapply(variable$map$as_list(), function(x) x$variable$column_idx, numeric(1))
    }
    data.frame(column_idx = column_indexes, type = type)
  })
  type_mapping <- do.call(rbind, type_mapping)
  type_mapping$type <- factor(type_mapping$type,
    levels = c("binary", "continuous", "integer")
  )
  type_mapping$type[order(type_mapping$column_idx)]
}

#' @export
variable_keys.linear_optimization_model <- function(model) {
  if (length(model$variables) == 0) {
    return(character(0))
  }
  # variable keys need to have a special format
  # for scalar variables it is simply the name
  # for indexed variables it is <name>[index1,index2,...,indexN]
  column_indexes <- lapply(names(model$variables), function(var_name) {
    type <- model$variable_types[[var_name]]
    variable <- model$variables[[var_name]]

    column_indexes <- if (inherits(variable, "LinearTerm")) {
      setNames(variable$variable$column_idx, var_name)
    } else {
      var_names <- paste0(var_name, "[", variable$map$keys(), "]")
      setNames(
        vapply(
          variable$map$as_list(),
          function(x) x$variable$column_idx,
          numeric(1)
        ),
        var_names
      )
    }
    column_indexes
  })
  names(sort(unlist(column_indexes)))
}

#' @export
#' @importFrom Matrix sparseVector
#' @importFrom rlang abort
objective_function.linear_optimization_model <- function(model) {
  nvars <- Reduce(`+`, nvars(model))
  obj_constant <- 0
  obj <- sparseVector(numeric(), integer(), nvars)
  has_obj_function <- !is.null(model$objective)
  if (!has_obj_function) {
    return(list(solution = obj, constant = obj_constant))
  }
  obj_fun <- model$objective$fun
  stopifnot(inherits(obj_fun, "LinearFunction") ||
    inherits(obj_fun, "LinearTerm") ||
    is.numeric(obj_fun))
  if (is.numeric(obj_fun)) {
    obj_constant <- obj_fun
    has_obj_function <- FALSE
  }
  if (has_obj_function) {
    stopifnot(inherits(obj_fun, "LinearFunction") ||
      inherits(obj_fun, "LinearTerm"))
    obj_constant <- if (inherits(obj_fun, "LinearTerm")) {
      0
    } else {
      obj_fun$constant
    }
    if (inherits(obj_fun, "LinearTerm")) {
      obj[obj_fun$variable$column_idx] <- obj_fun$coefficient
    } else {
      terms <- terms_list(obj_fun)
      for (term in terms) {
        if (term$coefficient != 0) {
          obj[term$variable$column_idx] <- term$coefficient
        }
      }
    }
  }
  list(solution = obj, constant = obj_constant)
}

#' @export
#' @importFrom Matrix sparseMatrix
extract_constraints.linear_optimization_model <- function(model) {
  n_constraints <- nconstraints(model)
  n_vars <- Reduce(`+`, nvars(model))
  if (n_constraints == 0) {
    return(list(
      matrix = sparseMatrix(i = numeric(), j = numeric(), dims = c(0, n_vars)),
      sense = character(0L),
      rhs = numeric(0L)
    ))
  }
  row_counter <- 0
  matrix_coefs <- lapply(model$constraints, function(constraint) {
    lhs <- (constraint$lhs - constraint$rhs) + 0
    stopifnot(inherits(lhs, "LinearFunction"))
    rhs_numeric <- -1 * lhs$constant
    lhs_terms <- terms_list(lhs)
    coefs <- vapply(lhs_terms, function(x) x$coefficient, numeric(1))
    cols <- vapply(lhs_terms, function(x) x$variable$column_idx, numeric(1))
    cols <- c(cols, n_vars + 1) # O(n)
    coefs <- c(coefs, rhs_numeric) # O(n)
    row_counter <<- row_counter + 1
    list(rep.int(row_counter, length(coefs)), cols, coefs)
  })
  # The below could be made faster. It probably runs O(n^2) as the vectors
  # get concatenated all the time. But the bottleneck is somewhere else
  i_s <- unlist(lapply(matrix_coefs, function(x) x[[1]]))
  j_s <- unlist(lapply(matrix_coefs, function(x) x[[2]]))
  values <- unlist(lapply(matrix_coefs, function(x) x[[3]]))
  matrix <- sparseMatrix(
    i = i_s,
    j = j_s,
    x = values,
    dims = c(n_constraints, n_vars + 1)
  )
  sense <- vapply(model$constraints, function(x) x$sense$sense, character(1))
  list(
    matrix = matrix[, seq_len(ncol(matrix) - 1), drop = FALSE],
    sense = sense,
    rhs = matrix[, ncol(matrix), drop = TRUE]
  )
}

#' @export
variable_bounds.linear_optimization_model <- function(model) {
  list(
    lower = model$variable_bounds_lower,
    upper = model$variable_bounds_upper
  )
}
