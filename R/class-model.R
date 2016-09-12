#' An S4 class to represent a ObjectiveFunction
#'
#' @slot expression the expression in standard form
#' @slot original_expression the original expression as supplied by the user
#' @slot direction the direction of optimization
#' @export
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
#' @export
Model <- setClass("Model",
         slots = c(
           "variables" = "list",
           "objective" = "ObjectiveFunction",
           "constraints" = "list"
         ),
         validity = function(object) TRUE)

#' Check if variable is defined in model
#'
#' @param model a model object
#' @param variable the variable expression to check
#'
#' @return TRUE iff the variable is part of the model
#' @export
setGeneric("is_defined", function(model, variable) {
  exp <- substitute(variable)
  exp_class <- class(exp)
  if (exp_class == "name") {
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
#' @param type must be either continuous, integer or binary
#' @param lb the lower bound of the variable
#' @param ub the upper bound of the variable
#' @param ... quantifiers for the indexed variabled
#'
#' @examples
#' library(magrittr)
#' MIPModel() %>%
#'  add_variable(x) %>% # creates 1 variable named x
#'  add_variable(y[i], i = 1:10) # creates 10 variables
#'
#' @export
setGeneric("add_variable", function(model, variable, type = "continuous",
                                    lb = -Inf, ub = Inf, ...) {
  if (ub < lb) {
    stop("The upper bound must not be smaller than the lower bound.")
  }
  if (length(lb) != 1 || length(ub) != 1) {
    stop("lb and ub must be of length 1. I.e. just a single number.")
  }
  if (!is.numeric(lb) || !is.numeric(ub)) {
    stop("lb and ub must be a number.")
  }
  if (length(type) != 1 || !type %in% c("continuous", "binary", "integer")) {
    stop(paste0("The type of a variable needs to be either",
                " continuous, binary or integer."))
  }
  exp <- substitute(variable)
  exp_class <- class(exp)
  if (exp_class == "name") {
    var_name <- as.character(exp)
    var <- new("Variable", arity = 0L,
               type = type, instances = "",
               lb = lb, ub = ub,
               variable_expression = as.expression(
                 substitute(x, list(x = var_name))),
                 variable_quantifiers = list()
               )
    model@variables[[var_name]] <- var
  } else if (exp_class == "call" && exp[[1]] == "[") {

    # first we need to bind all variables
    var_name <- as.character(exp[[2]])
    bound_subscripts <- list(...)
    bound_exp <- bind_expression(var_name, exp, parent.frame(),
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
    candidates <- expand.grid(subscripts)
    only_integer_candidates <- apply(candidates, 1, function(r) {
      all(!is.na(suppressWarnings(as.integer(r))))
    })
    stopifnot(only_integer_candidates)
    instances <- apply(candidates, 1, function(row) {
      paste0(as.integer(row), collapse = "_")
    })
    var <- new("Variable",
               arity = arity,
               type = type,
               instances = instances,
               lb = lb,
               ub = ub,
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

#' Set the model objective
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
#' @export
setGeneric("set_objective", function(model, expression, direction = "max") {
  obj_ast <- substitute(expression)
  ast <- normalize_expression(model, obj_ast, parent.frame())
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
#'            bound variables a new constraint is created.
#'
#' @return a Model with new constraints added
#'
#' @examples
#' library(magrittr)
#' MIPModel() %>%
#'  add_variable(x[i], i = 1:5) %>%
#'  add_constraint(x[i] >= 1, i = 1:5) # creates 5 constraints
#'
#' @export
setGeneric("add_constraint", function(model,
                                      constraint_expr,
                                      .show_progress_bar = TRUE, ...) {
  constraint_ast <- substitute(constraint_expr)
  if (length(constraint_ast) != 3) {
    stop("constraint not well formed. Must be a linear (in)equality.")
  }
  direction <- as.character(constraint_ast[[1]])
  if (!direction %in% c(">=", "<=", "==")) {
    stop("Does not recognize constraint expr. Missing the constraint relation")
  }
  lhs_ast <- constraint_ast[[2]]
  rhs_ast <- constraint_ast[[3]]
  parent_env <- parent.frame()
  bound_subscripts <- list(...)
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
    direction <- if (direction == "=") "==" else direction
    new("Constraint", lhs = as.expression(lhs_ast),
                      rhs = as.expression(rhs_ast),
                      direction = direction)
  }
  constraints <- model@constraints
  if (is.list(bound_subscripts) && length(bound_subscripts) > 0) {
    filter_fn <- function(x) is.numeric(x) & length(x) > 0
    bound_subscripts <- Filter(filter_fn, bound_subscripts)
    var_combinations <- expand.grid(bound_subscripts)

    # let's init a progress bar
    p <- dplyr::progress_estimated(nrow(var_combinations), min_time = 2)
    new_constraints <- apply(var_combinations, 1, function(row) {
      calling_env <- as.environment(as.list(row))
      parent.env(calling_env) <- parent_env
      constraint <- add_constraint_internal(calling_env)
      if (.show_progress_bar) {
        p$pause(0.1)$tick()$print()
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
#' @export
MIPModel <- function() Model()

#' Creates a new MILP Model
#' @export
MILPModel <- function() Model()
