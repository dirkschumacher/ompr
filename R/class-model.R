
bind_expression <- function(var_name, exp, envir, bound_subscripts) {
  var_values <- as.list(envir)
  for(x in c(var_name, names(bound_subscripts))) {
    var_values[[x]] <- NULL
  }
  eval(substitute(substitute(x, var_values), list(x = exp)))
}

#' An S4 class to represent a ObjectiveFunction
#'
#' @export
ObjectiveFunction <- setClass("ObjectiveFunction",
                              slots = c(
                                expression = "expression",
                                original_expression = "expression",
                                direction = "character"),
                              validity = function(object) {
                                object@direction %in% c("min", "max")
                              })

#' An S4 class to represent a Model.
#'
#' @slot variables a list of S4 Variable objects
#' @slot objective -
#' @slot constraints -
#' @export
Model <- setClass("Model",
         slots = c(
           "variables" = "list",
           "objective" = "ObjectiveFunction",
           "constraints" = "list"
         ),
         validity = function(object) TRUE)

#' @export
setGeneric("is_defined", function(model, variable) {
  standardGeneric("is_defined")
})

#' @export
setMethod("is_defined",
          signature(model = "Model"),
          definition = function(model, variable) {
            exp <- substitute(variable)
            exp_class <- class(exp)
            if (exp_class == "name") {
              var_name <- as.character(exp)
              return(var_name %in% names(model@variables))
            } else if (exp_class == "call" && exp[[1]] == "[") {
              var_name <- as.character(exp[[2]])
              bound_exp <- bind_expression(var_name, exp, parent.frame(), list())
              if (!var_name %in% names(model@variables)) {
                return(FALSE)
              }
              var_obj <- model@variables[[var_name]]
              search_key <- paste0(as.character(bound_exp[3:length(bound_exp)]), collapse = "_")
              return(search_key %in% var_obj@instances)
            }
            else {
              stop("Did not recognize variable expression.")
            }
          })

#' @export
setGeneric("add_variable", function(model, variable, type = "continuous", lb = -Inf, ub = Inf, ...) {
  standardGeneric("add_variable")
})

#' @export
setMethod("add_variable",
          signature(model = "Model"),
          definition = function(model, variable, type = "continuous", lb = -Inf, ub = Inf, ...) {
            if (ub < lb) {
              stop("ub must not be smaller than lb.")
            }
            exp <- substitute(variable)
            exp_class <- class(exp)
            if (exp_class == "name") {
              var_name <- as.character(exp)
              var <- new("Variable", arity = 0L,
                         type = type, instances = "",
                         lb = lb, ub = ub,
                         variable_expression = as.expression(substitute(x, list(x = var_name))),
                         variable_quantifiers = list()
                         )
              model@variables[[var_name]] <- var
            } else if (exp_class == "call" && exp[[1]] == "[") {

              # first we need to bind all variables
              var_name <- as.character(exp[[2]])
              bound_subscripts <- list(...)
              bound_exp <- bind_expression(var_name, exp, parent.frame(), bound_subscripts)
              arity <- as.integer(length(bound_exp) - 2)

              # then check if all free variables are present in "..."
              subscripts <- lapply(3:length(bound_exp), function(x) as.character(bound_exp[x]))
              bound_subscripts <- bound_subscripts[names(bound_subscripts) %in% subscripts]
              subscripts[subscripts %in% names(bound_subscripts)] <- bound_subscripts

              # now generate all variables
              candidates <- expand.grid(subscripts)
              only_integer_candidates <- apply(candidates, 1,
                                               function(r) all(!is.na(suppressWarnings(as.integer(r)))))
              stopifnot(only_integer_candidates)
              instances <- apply(candidates, 1, function(row) paste0(as.integer(row), collapse = "_"))
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
              stop("Did not recognize variable expression.")
            }
            model
          }
)

#' @export
setGeneric("set_objective", function(model, expression, direction = "max") {
  standardGeneric("set_objective")
})

#' @export
setMethod("set_objective",
          signature(model = "Model"),
          definition = function(model, expression, direction = "max") {
            obj_ast <- substitute(expression)
            ast <- normalize_expression(model, obj_ast, parent.frame())
            var_names <- names(model@variables)
            if (is_non_linear(var_names, ast)) {
              stop("The objective is probably non-linear. Currently, only linear functions are supported.")
            }
            obj <- new("ObjectiveFunction",
                       expression = as.expression(ast),
                       original_expression = as.expression(obj_ast),
                       direction = direction)
            model@objective <- obj
            model
          }
)

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
            }, mapped_vars, init = list(continuous = 0, integer = 0, binary = 0))
            cat("Variables:\n")
            cat("  Continuous:", var_count$continuous, "\n")
            cat("  Integer:", var_count$integer, "\n")
            cat("  Binary:", var_count$binary, "\n")

            # obj function
            objective <- object@objective
            if (!is.null(objective)) {
              cat("Search direction:",
                if (objective@direction == "max") "maximize" else "minimize",
                "\n")
            } else {
              cat("No objective function. \n")
            }

            # constraints
            cat("Constraints:", length(object@constraints), "\n")
          })

#' @export
setGeneric("add_constraint", function(model, lhs, direction, rhs, ...) {
  standardGeneric("add_constraint")
})

#' @export
setMethod("add_constraint",
          signature(model = "Model"),
          definition = function(model, lhs, direction, rhs, ...) {
            lhs_ast <- substitute(lhs)
            rhs_ast <- substitute(rhs)
            parent_env = parent.frame()
            add_constraint_internal <- function(envir = parent_env) {
              lhs_ast <- normalize_expression(model, lhs_ast, envir)
              rhs_ast <- normalize_expression(model, rhs_ast, envir)
              var_names <- names(model@variables)
              if (is_non_linear(var_names, lhs_ast)) {
                stop("The left-hand-side is probably non-linear. Currently, only linear constraints are supported.")
              }
              if (is_non_linear(var_names, rhs_ast)) {
                stop("The right-hand-side is probably non-linear. Currently, only linear constraints are supported.")
              }
              if (any_unbounded_indexes(lhs_ast)) {
                stop("Some variable indexes are unbounded left hand expression.")
              }
              if (any_unbounded_indexes(rhs_ast)) {
                stop("Some variable indexes are unbounded in the right hand expression.")
              }
              new("Constraint", lhs = as.expression(lhs_ast),
                                rhs = as.expression(rhs_ast),
                                direction = if (direction == "=") "==" else direction)
            }
            bound_subscripts <- list(...)
            constraints <- model@constraints
            if (is.list(bound_subscripts) && length(bound_subscripts) > 0) {
              bound_subscripts <- Filter(function(x) is.integer(x) & length(x) > 0,
                                         bound_subscripts)
              var_combinations <- expand.grid(bound_subscripts)
              new_constraints <- apply(var_combinations, 1, function(row) {
                calling_env <- as.environment(as.list(row))
                parent.env(calling_env) <- parent_env
                constraint <- add_constraint_internal(calling_env)
                constraint
              })
              constraints <- c(constraints, new_constraints)
            } else {
              constraints <- c(constraints, add_constraint_internal())
            }
            model@constraints <- constraints
            model
          }
)

#' @export
setGeneric("solve_model", function(model, solver) {
  standardGeneric("solve_model")
})

#' @export
setMethod("solve_model",
          signature(model = "Model", solver = "function"),
          definition = function(model, solver) {
            solver(model)
          }
)

#' Creates a new MIP Model
#' @export
MIPModel <- function() Model()
