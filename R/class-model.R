#' An S4 class to represent a ObjectiveFunction
#'
#' @slot expression the expression in standard form
#' @slot original_expression the original expression as supplied by the user
#' @slot direction the direction of optimization
#' @noRd
ObjectiveFunction <- setClass("ObjectiveFunction", # Exclude Linting
                              slots = c(
                                expression = "expression",
                                original_expression = "expression",
                                direction = "character"),
                              validity = function(object) {
                                length(object@direction) == 1 &&
                                  object@direction %in% c("min", "max")
                              })

#' An S4 class to represent a Model.
#'
#' @slot variables a list of S4 Variable objects
#' @slot objective the objective function
#' @slot constraints a list of constraints
#' @noRd
Model <- setClass("Model",
         slots = c(
           "variables" = "list",
           "objective" = "ObjectiveFunction",
           "constraints" = "list"
         ),
         validity = function(object) TRUE)

#' Checks if variable is defined in model
#'
#' @param model a model object
#' @param variable the variable expression to check
#'
#' @return TRUE iff the variable is part of the model
#' @export
setGeneric("is_defined", function(model, variable) {
  exp <- substitute(variable)
  exp_class <- class(exp)
  if (lazyeval::is_name(exp)) {
    var_name <- as.character(exp)
    return(var_name %in% names(model@variables))
  } else if (exp_class == "call" && exp[[1]] == "[") {
    var_name <- as.character(exp[[2]])
    bound_exp <- bind_expression(var_name, exp, parent.frame(),
                                 list())
    if (!var_name %in% names(model@variables)) {
      return(FALSE)
    }
    var_obj <- model@variables[[var_name]]
    search_key <- paste0(as.character(bound_exp[3:length(bound_exp)])
                         , collapse = "_")
    return(search_key %in% var_obj@instances)
  }
  else {
    stop("Did not recognize variable expression.")
  }
})

#' Adds a variable to the model
#'
#' A variable can either be a name or an indexed name. See examples.
#'
#' @param model the model
#' @param variable the variable name/definition
#' @param ... quantifiers for the indexed variabled. Including filters
#' @param type must be either continuous, integer or binary
#' @param lb the lower bound of the variable
#' @param ub the upper bound of the variable
#' @param .dots Used to work around non-standard evaluation.
#'
#' @examples
#' library(magrittr)
#' MIPModel() %>%
#'  add_variable(x) %>% # creates 1 variable named x
#'  add_variable(y[i], i = 1:10, i %% 2 == 0,
#'                type = "binary") # creates 4 variables
#'
#' @aliases add_variable_
#' @export
setGeneric("add_variable", function(model, variable, ..., type = "continuous",
                                     lb = -Inf, ub = Inf) {
  add_variable_(model, lazyeval::lazy(variable), type = type,
                lb = lb, ub = ub, .dots = lazyeval::lazy_dots(...))
})

# helper function to check variable bounds
check_bounds <- function(lb, ub) {
  if (any(ub < lb)) {
    stop("The upper bound must not be smaller than the lower bound.")
  }
  if (any(!is.numeric(lb) | !is.numeric(ub))) {
    stop("lb and ub must be a number.")
  }
}

#' @export
setGeneric("add_variable_", function(model, variable, ..., type = "continuous",
                                    lb = -Inf, ub = Inf, .dots) {
  if (length(lb) != 1 || length(ub) != 1) {
    stop("lb and ub must be of length 1. I.e. just a single number.")
  }
  check_bounds(lb, ub)
  if (length(type) != 1 || !type %in% c("continuous", "binary", "integer")) {
    stop(paste0("The type of a variable needs to be either",
                " continuous, binary or integer."))
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
              "either be 0 or 1. Setting it to 0"))
      lb <- 0
    }
    if (!ub %in% c(0, 1)) {
      warning(paste0("upper bound of binary variable can ",
                     "either be 0 or 1. Setting it to 1"))
      ub <- 1
    }
  }
  variable <- lazyeval::as.lazy(variable)
  exp <- variable$expr
  if (lazyeval::is_name(exp)) {
    var_name <- as.character(exp)
    var <- new("Variable", arity = 0L,
               type = type, instances = "",
               lb = lb, ub = ub,
               variable_expression = as.expression(as.symbol(var_name)),
               variable_quantifiers = list()
               )
    model@variables[[var_name]] <- var
  } else if (lazyeval::is_call(exp) && exp[[1]] == "[") {

    # first we need to bind all variables
    var_name <- as.character(exp[[2]])

    lazy_dots <- lazyeval::lazy_dots(...)
    if (!missing(.dots)) {
      lazy_dots <- c(lazyeval::as.lazy_dots(.dots), lazy_dots)
    }

    classified_quantifiers <- classify_quantifiers(lazy_dots)
    bound_subscripts <- lapply(classified_quantifiers$quantifiers,
                               lazyeval::lazy_eval)
    bound_exp <- bind_expression(var_name, exp, variable$env,
                                 bound_subscripts)
    arity <- as.integer(length(bound_exp) - 2)

    # then check if all free variables are present in "..."
    subscripts <- lapply(3:length(bound_exp),
                         function(x) as.character(bound_exp[x]))
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
    var <- new("Variable",
               arity = arity,
               type = type,
               instances = instances,
               lb = rep.int(lb, n_vars),
               ub = rep.int(ub, n_vars),
               variable_expression = as.expression(exp),
               variable_quantifiers = bound_subscripts)
    model@variables[[var_name]] <- var
  } else {
    stop(paste0("The variable definition does not seem to be right.",
                "Take a look at the vignettes if you need examples on how",
                " to formulate variables"))
  }
  model
})


#' Sets the bounds of a variable
#'
#' Change the lower and upper bounds of a named variable,
#' indexed variable or a group of variables.
#'
#' @usage
#' set_bounds(model, variable, ..., lb = NULL, ub = NULL)
#' set_bounds_(model, variable, ..., lb = NULL, ub = NULL, .dots)
#'
#' @param model the model
#' @param variable the variable name/definition
#' @param ... quantifiers for the indexed variabled
#' @param lb the lower bound of the variable
#' @param ub the upper bound of the variable
#' @param .dots Used to work around non-standard evaluation.
#'
#' @aliases set_bounds_
#' @examples
#' library(magrittr)
#' MIPModel() %>%
#'  add_variable(x[i], i = 1:5) %>%
#'  add_constraint(x[i] >= 1, i = 1:5) %>% # creates 5 constraints
#'  set_bounds(x[i], lb = 3, i = 1:3)
#'
#' @export
setGeneric("set_bounds", function(model, variable, ..., lb = NULL, ub = NULL) {
  set_bounds_(model, lazyeval::lazy(variable), lb = lb, ub = ub,
              .dots = lazyeval::lazy_dots(...))
})

#' @export
setGeneric("set_bounds_", function(model, variable, ..., lb = NULL, ub = NULL, .dots) {
  if (is.numeric(lb) && is.numeric(ub)) {
    check_bounds(lb, ub)
  }
  variable <- lazyeval::as.lazy(variable)
  is_single_variable <- lazyeval::is_name(variable$expr)
  is_indexed_variable <- lazyeval::is_call(variable$expr) &&
                          variable$expr[[1]] == "[" &&
                          length(variable$expr) >= 3
  model_variable_names <- names(model@variables)
  replace_lb <- !is.null(lb) && is.numeric(lb)
  replace_ub <- !is.null(ub) && is.numeric(ub)
  if (is_single_variable) {
    var_name <- as.character(variable$expr)
    if (!var_name %in% model_variable_names) {
      stop("Variable does not exists in model")
    }
    variable <- model@variables[[var_name]]
    if (replace_lb) {
      variable@lb <- lb
    }
    if (replace_ub) {
      variable@ub <- ub
    }
    model@variables[[var_name]] <- variable
  } else if (is_indexed_variable) {
    var_name <- as.character(variable$expr[[2]])
    if (!var_name %in% model_variable_names) {
      stop("Variable does not exists in model")
    }
    model_variable <- model@variables[[var_name]]
    index_names <- sapply(3:length(variable$expr), function(i) {
      as.character(variable$expr[i])
    })
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
          stop(paste0("Index ", i_name, " not bound by quantifier"))
        }
        quantifier_combinations[[i_name]]
      }
    }), index_names)

    bound_subscripts <- as.data.frame(bound_subscripts)
    indexes <- apply(bound_subscripts, 1, as.list)

    instance_keys <- sapply(indexes, function(x) {
      paste0(x, collapse = "_")
    })
    var_indexes <- which(model_variable@instances %in% instance_keys)
    if (any(!instance_keys %in% model_variable@instances)) {
      stop("Indexed variable out of bounds.")
    }
    if (replace_lb) {
      model_variable@lb[var_indexes] <- lb
    }
    if (replace_ub) {
      model_variable@ub[var_indexes] <- ub
    }
    model@variables[[var_name]] <- model_variable
  }
  model
})

#' Sets the model objective
#'
#' @param model the model
#' @param expression the linear objective as a sum of variables and constants
#' @param direction the optimization direction. Must be either "max" or "min".
#'
#' @return a Model with a new objective function definition
#' @examples
#' library(magrittr)
#' MIPModel() %>%
#'  add_variable(x, lb = 2) %>%
#'  add_variable(y, lb = 40) %>%
#'  set_objective(x + y, direction = "min")
#'
#' @aliases set_objective_
#' @export
setGeneric("set_objective", function(model, expression,
                                     direction = c("max", "min")) {
  set_objective_(model, expression = lazyeval::lazy(expression),
                 direction = direction)
})

#' @export
setGeneric("set_objective_", function(model, expression,
                                      direction = c("max", "min")) {
  stopifnot(length(expression) != 1)
  expression <- lazyeval::as.lazy(expression)
  direction <- match.arg(direction)
  obj_ast <- expression$expr
  ast <- normalize_expression(model, obj_ast, expression$env)
  var_names <- names(model@variables)
  if (is_non_linear(var_names, ast)) {
    stop(paste0("The objective is probably non-linear. ",
                "Currently, only linear functions are supported."))
  }
  obj <- new("ObjectiveFunction",
             expression = as.expression(ast),
             original_expression = as.expression(obj_ast),
             direction = direction)
  model@objective <- obj
  model
})

#' Outputs the model to the console
#'
#' @param object the model object
#' @importFrom stats setNames
#' @export
setMethod("show", signature(object = "Model"),
          definition = function(object) {
            cat("Mixed linear integer optimization problem\n")
            mapped_vars <- Map(f = function(var) {
              setNames(length(var@instances), var@type)
            }, object@variables)
            var_count <- Reduce(f = function(acc, el) {
              acc[[names(el)]] <- acc[[names(el)]] + el
              acc
            }, mapped_vars, init = list(continuous = 0, integer = 0,
                                        binary = 0))
            cat("Variables:\n")
            cat("  Continuous:", var_count$continuous, "\n")
            cat("  Integer:", var_count$integer, "\n")
            cat("  Binary:", var_count$binary, "\n")

            # obj function
            objective <- object@objective
            if (!is.null(objective) &&
                length(objective@direction) == 1) {
              cat("Search direction:",
                if (objective@direction == "max") "maximize" else "minimize",
                "\n")
            } else {
              cat("No objective function. \n")
            }

            # constraints
            cat("Constraints:", length(object@constraints), "\n")
          })


#' Add a constraint
#'
#' Add one or more constraints to the model using quantifiers.
#'
#' @param model the model
#' @param constraint_expr the constraint. Must be a linear (in)equality with
#'        operator "<=", "==" or ">=".
#' @param .show_progress_bar displays a progressbar when adding multiple
#'                           constraints
#' @param ... quantifiers for the indexed variables. For all combinations of
#'            bound variables a new constraint is created. In addition
#'            you can add filter expressions
#'
#' @return a Model with new constraints added
#' @usage
#' add_constraint(model, constraint_expr, ..., .show_progress_bar)
#' add_constraint_(model, constraint_expr, ..., .dots, .show_progress_bar)
#' @examples
#' library(magrittr)
#' MIPModel() %>%
#'  add_variable(x[i], i = 1:5) %>%
#'  add_constraint(x[i] >= 1, i = 1:5) # creates 5 constraints
#'
#' @aliases add_constraint_
#' @export
setGeneric("add_constraint", function(model,
                                      constraint_expr,
                                      ...,
                                      .show_progress_bar = TRUE) {
  add_constraint_(model, lazyeval::lazy(constraint_expr),
                  .dots = lazyeval::lazy_dots(...),
                  .show_progress_bar = .show_progress_bar)
})

#' @export
setGeneric("add_constraint_", function(model,
                                      constraint_expr,
                                      ...,
                                      .dots,
                                      .show_progress_bar = TRUE) {
  constraint_expr <- lazyeval::as.lazy(constraint_expr)
  constraint_ast <- constraint_expr$expr
  if (length(constraint_ast) != 3) {
    stop("constraint not well formed. Must be a linear (in)equality.")
  }
  direction <- as.character(constraint_ast[[1]])
  if (!direction %in% c(">=", "<=", "==")) {
    stop("Does not recognize constraint expr. Missing the constraint relation")
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
    var_names <- names(model@variables)
    if (is_non_linear(var_names, lhs_ast)) {
      stop(paste0("The left-hand-side is probably non-linear. ",
                  "Currently, only linear constraints are ",
                  "supported."))
    }
    if (is_non_linear(var_names, rhs_ast)) {
      stop(paste0("The right-hand-side is probably non-linear. ",
                  "Currently, only linear constraints are ",
                  "supported."))
    }
    new("Constraint", lhs = as.expression(lhs_ast),
                      rhs = as.expression(rhs_ast),
                      direction = direction)
  }
  constraints <- model@constraints
  if (is.list(bound_subscripts) && length(bound_subscripts) > 0) {
    filter_fn <- function(x) is.numeric(x) & length(x) > 0
    bound_subscripts <- Filter(filter_fn, bound_subscripts)
    var_combinations <- build_quantifier_candidates(bound_subscripts,
                                              names(bound_subscripts),
                                              classified_quantifiers$filters)
    zero_vars_msg <- "The number of different indexes is 0 for this constraint"
    validate_quantifier_candidates(var_combinations, zero_vars_msg)

    # let's init a progress bar
    p <- dplyr::progress_estimated(nrow(var_combinations), min_time = 2)
    new_constraints <- apply(var_combinations, 1, function(row) {
      calling_env <- as.environment(as.list(row))
      parent.env(calling_env) <- parent_env
      constraint <- add_constraint_internal(calling_env)
      if (.show_progress_bar) {
        p$tick()$print()
      }
      constraint
    })
    constraints <- c(constraints, new_constraints)
  } else {
    constraints <- c(constraints, add_constraint_internal())
  }
  model@constraints <- constraints
  model
})

#' Solve a model
#'
#' @param model the model
#' @param solver a function mapping a model to a solution
#'
#' @return solver(model)
#'
#' @export
setGeneric("solve_model", function(model, solver) {
  if (!is.function(solver)) {
    stop(paste0("Solver is not a function Model -> Solution.\n",
                "Take a look at one of the vignettes on how to call",
                " solve_model."))
  }
  solver(model)
})

#' Creates a new MIP Model
#' @seealso MILPModel
#' @export
MIPModel <- function() Model()

#' Creates a new MILP Model
#' @seealso MIPModel
#' @export
MILPModel <- function() Model()
