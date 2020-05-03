#' An S3 class that model an objective function
#'
#' @param expression the expression in standard form
#' @param original_expression the original expression as supplied by the user
#' @param sense the sense of the model
#' @noRd
new_objective_function <- function(expression,
                                   original_expression,
                                   sense) {
  stopifnot(length(sense) == 1 &&
    sense %in% c("min", "max"))
  structure(list(
    expression = expression,
    original_expression = original_expression,
    sense = sense
  ), class = "model_objective")
}


#' Add a variable to the model
#'
#' A variable can either be a name or an indexed name. See examples.
#'
#' @param .model the model
#' @param .variable the variable name/definition
#' @param ... quantifiers for the indexed variable. Including filters
#' @param type must be either continuous, integer or binary
#' @param lb the lower bound of the variable
#' @param ub the upper bound of the variable
#'
#' @examples
#' library(magrittr)
#' MIPModel() %>%
#'   add_variable(x) %>% # creates 1 variable named x
#'   add_variable(y[i],
#'     i = 1:10, i %% 2 == 0,
#'     type = "binary"
#'   ) # creates 4 variables
#' @export
add_variable <- function(.model, .variable, ..., type = "continuous",
                         lb = -Inf, ub = Inf) {
  add_variable_(
    .model = .model,
    .variable = lazyeval::as.lazy(
      substitute(.variable),
      parent.frame()
    ),
    type = type,
    lb = lb, ub = ub, .dots = lazyeval::lazy_dots(...)
  )
}

# helper function to check variable bounds
check_bounds <- function(lb, ub) {
  if (any(ub < lb)) {
    stop("The upper bound must not be smaller than the lower bound.",
      call. = FALSE
    )
  }
  if (any(!is.numeric(lb) | !is.numeric(ub))) {
    stop("lb and ub must be a number.", call. = FALSE)
  }
}

#' @inheritParams add_variable
#' @param .dots Used to work around non-standard evaluation.
#' @rdname add_variable
#' @export
add_variable_ <- function(.model, .variable, ..., type = "continuous",
                          lb = -Inf, ub = Inf, .dots) {
  UseMethod("add_variable_")
}

#' @export
add_variable_.optimization_model <- function(.model, .variable, ...,
                                             type = "continuous",
                                             lb = -Inf, ub = Inf, .dots) {
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
  variable <- lazyeval::as.lazy(.variable)
  model <- .model
  expr <- variable$expr
  if (lazyeval::is_name(expr)) {
    var_name <- as.character(expr)
    var <- new_variable(
      arity = 0L,
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
    bound_subscripts <- lapply(
      classified_quantifiers$quantifiers,
      lazyeval::lazy_eval
    )
    bound_expr <- bind_expression(
      var_name, expr, variable$env,
      bound_subscripts
    )
    arity <- as.integer(length(bound_expr) - 2)

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

    # now generate all variables
    candidates <- build_quantifier_candidates(
      subscripts,
      names(bound_subscripts),
      classified_quantifiers$filters
    )
    zero_vars_msg <- paste0(
      "The number of different indexes for variable ",
      var_name, " is 0."
    )
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
      ub = rep.int(ub, n_vars)
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


#' Set the bounds of a variable
#'
#' Change the lower and upper bounds of a named variable,
#' indexed variable or a group of variables.
#'
#' @param .model the model
#' @param .variable the variable name/definition
#' @param ... quantifiers for the indexed variable
#' @param lb the lower bound of the variable
#' @param ub the upper bound of the variable
#'
#' @examples
#' library(magrittr)
#' MIPModel() %>%
#'   add_variable(x[i], i = 1:5) %>%
#'   add_constraint(x[i] >= 1, i = 1:5) %>% # creates 5 constraints
#'   set_bounds(x[i], lb = 3, i = 1:3)
#' @export
set_bounds <- function(.model, .variable, ..., lb = NULL, ub = NULL) {
  set_bounds_(.model, lazyeval::as.lazy(substitute(.variable), parent.frame()),
    lb = lb, ub = ub,
    .dots = lazyeval::lazy_dots(...)
  )
}

#' @inheritParams set_bounds
#' @param .dots Used to work around non-standard evaluation.
#' @rdname set_bounds
#' @export
set_bounds_ <- function(.model, .variable, ...,
                        lb = NULL, ub = NULL, .dots) {
  UseMethod("set_bounds_")
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
    bound_subscripts <- lapply(
      classified_quantifiers$quantifiers,
      lazyeval::lazy_eval
    )
    quantifier_combinations <- build_quantifier_candidates(
      bound_subscripts,
      names(bound_subscripts),
      classified_quantifiers$filters
    )
    zero_vars_msg <- paste0(
      "The number of different indexes for set_bounds ",
      "for variable ", var_name, " is 0."
    )
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
            call. = FALSE
          )
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

#' Set the model objective
#'
#' @param model the model
#' @param expression the linear objective as a sum of variables and constants
#' @param sense the model sense. Must be either "max" or "min".
#'
#' @return a Model with a new objective function definition
#' @examples
#' library(magrittr)
#' MIPModel() %>%
#'   add_variable(x, lb = 2) %>%
#'   add_variable(y, lb = 40) %>%
#'   set_objective(x + y, sense = "min")
#' @export
set_objective <- function(model, expression,
                          sense = c("max", "min")) {
  set_objective_(model,
    expression = lazyeval::as.lazy(
      substitute(expression),
      parent.frame()
    ),
    sense = sense
  )
}

#' @inheritParams set_objective
#' @rdname set_objective
#' @export
set_objective_ <- function(model, expression,
                           sense = c("max", "min")) {
  UseMethod("set_objective_")
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
    stop(paste0(
      "The objective is probably non-linear. ",
      "Currently, only linear functions are supported."
    ),
    call. = FALSE
    )
  }
  obj <- new_objective_function(
    expression = as.expression(ast),
    original_expression = as.expression(obj_ast),
    sense = sense
  )
  model$objective <- obj
  model
}

#' @export
#' @inheritParams print
#' @importFrom stats setNames
print.optimization_model <- function(x, ...) {
  print_model(x)
}


#' Add a constraint
#'
#' Add one or more constraints to the model using quantifiers.
#'
#' @param .model the model
#' @param .constraint_expr the constraint. Must be a linear (in)equality with
#'        operator "<=", "==" or ">=".
#' @param ... quantifiers for the indexed variables. For all combinations of
#'            bound variables a new constraint is created. In addition
#'            you can add filter expressions
#' @param .show_progress_bar displays a progressbar when adding multiple
#'                           constraints
#'
#' @return a Model with new constraints added
#'
#' @examples
#' library(magrittr)
#' MIPModel() %>%
#'   add_variable(x[i], i = 1:5) %>%
#'   add_constraint(x[i] >= 1, i = 1:5) # creates 5 constraints
#' @export
add_constraint <- function(.model, .constraint_expr, ...,
                           .show_progress_bar = TRUE) {
  add_constraint_(.model, lazyeval::as.lazy(
    substitute(.constraint_expr),
    parent.frame()
  ),
  .dots = lazyeval::lazy_dots(...),
  .show_progress_bar = .show_progress_bar
  )
}

#' @inheritParams add_constraint
#' @param .dots Used to work around non-standard evaluation.
#' @rdname add_constraint
#' @export
add_constraint_ <- function(.model,
                            .constraint_expr,
                            ...,
                            .dots,
                            .show_progress_bar = TRUE) {
  UseMethod("add_constraint_")
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
  add_constraint_internal <- function(envir = parent_env) {
    lhs_ast <- normalize_expression(model, lhs_ast, envir)
    rhs_ast <- normalize_expression(model, rhs_ast, envir)
    var_names <- names(model$variables)
    if (is_non_linear(var_names, lhs_ast)) {
      stop(paste0(
        "The left-hand-side is probably non-linear. ",
        "Currently, only linear constraints are ",
        "supported."
      ), call. = FALSE)
    }
    if (is_non_linear(var_names, rhs_ast)) {
      stop(paste0(
        "The right-hand-side is probably non-linear. ",
        "Currently, only linear constraints are ",
        "supported."
      ), call. = FALSE)
    }
    new_constraint(
      lhs = as.expression(lhs_ast),
      rhs = as.expression(rhs_ast),
      sense = sense
    )
  }
  constraints <- model$constraints
  if (is.list(bound_subscripts) && length(bound_subscripts) > 0) {
    filter_fn <- function(x) is.numeric(x) & length(x) > 0
    bound_subscripts <- Filter(filter_fn, bound_subscripts)
    var_combinations <- build_quantifier_candidates(
      bound_subscripts,
      names(bound_subscripts),
      classified_quantifiers$filters
    )
    zero_vars_msg <- "The number of different indexes is 0 for this constraint"
    validate_quantifier_candidates(var_combinations, zero_vars_msg)

    # let's init a progress bar
    progress_format <- "  adding constraints [:bar] :percent eta :eta"
    p <- progress::progress_bar$new(
      total = nrow(var_combinations),
      format = progress_format
    )
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

#' Solve a model
#'
#' @param model the model
#' @param solver a function mapping a model to a solution
#'
#' @return solver(model)
#'
#' @export
solve_model <- function(model, solver) UseMethod("solve_model")

#' @export
solve_model.optimization_model <- function(model, solver) {
  if (!is.function(solver)) {
    stop(paste0(
      "Solver is not a function Model -> Solution.\n",
      "Take a look at the examples on the website on how to call",
      " solve_model."
    ))
  }
  solver(model)
}

#' Create a new MIP Model
#' @export
MIPModel <- function() {
  structure(list(
    variables = list(),
    objective = NULL,
    constraints = list()
  ),
  class = "optimization_model"
  )
}
