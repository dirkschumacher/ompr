#' Get all unique names of the model variables
#'
#' @param model the model
#' @return a character vector ordered in the same way
#'         as the constraint matrix columns and objective vector
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:3)
#' variable_keys(model)
#' @export
variable_keys <- function(model) UseMethod("variable_keys")

#' @export
variable_keys.optimization_model <- function(model) {
  if (length(model$variables) == 0) {
    return(character(0))
  }
  unlist(lapply(
    sort(names(model$variables)),
    function(x) {
      var <- model$variables[[x]]
      if (var$arity > 0) {
        vapply(var$instances, function(var_code) {
          splitted_els <- strsplit(var_code, "_", fixed = TRUE)[[1]]
          paste0(
            x, "[",
            paste0(splitted_els[seq_len(length(splitted_els))],
              collapse = ","
            ),
            "]"
          )
        }, character(1))
      } else {
        x
      }
    }
  ), use.names = FALSE)
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
        search_key <- paste0(
          var_name, "[",
          paste0(as.character(var_ast[3:length(var_ast)]),
            collapse = ","
          ), "]"
        )
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


#' Extract the objective function from a model
#'
#' @param model the model
#'
#' @return a list with two named elements, 'solution' and 'constant'.
#' 'solution' is a sparse vector from the Matrix package.
#' 'constant' is a constant that needs to be added to get the final obj. value.
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:5) %>%
#'   set_objective(sum_expr(i * x[i], i = 1:5) + 10)
#' objective_function(model)
#' @export
objective_function <- function(model) UseMethod("objective_function")

#' @export
objective_function.optimization_model <- function(model) {
  objective <- model$objective
  has_objective <- !is.null(objective)
  build_coefficent_vector <- build_coefficent_vector_fun(variable_keys(model))
  if (has_objective) {
    coefficients <- extract_coefficients_internal(
      model$objective$expression[[1]]
    )
    obj_constant <- coefficients$constant
    if (!is.numeric(obj_constant)) obj_constant <- 0
    coefficients <- coefficients$coefficients
    names(coefficients) <- NULL
    obj_vector <- build_coefficent_vector(coefficients)
    ordered_i <- order(obj_vector@j)
    obj_vector <- Matrix::sparseVector(
      x = obj_vector@x[ordered_i],
      i = obj_vector@j[ordered_i] + 1,
      length = ncol(obj_vector)
    )
    list(solution = obj_vector, constant = obj_constant)
  } else {
    n_vars <- sum(unlist(nvars(model)))
    obj <- Matrix::sparseVector(integer(), integer(), n_vars)
    list(solution = obj, constant = 0)
  }
}

#' Extract the constraint matrix, the right hand side and the sense from a model
#'
#' @param model the model
#' @return a list with three named elements.
#'         'matrix' the (sparse) constraint matrix from the Matrix package.
#'         'rhs' is the right hand side vector in the order of the matrix.
#'         'sense' is a vector of the constraint senses
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:3) %>%
#'   add_variable(y[i], i = 1:3) %>%
#'   add_constraint(x[i] + y[i] <= 1, i = 1:3)
#' extract_constraints(model)
#' @export
extract_constraints <- function(model) UseMethod("extract_constraints")

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

#' Number of variables of a model
#'
#' @param model the model
#' @return a list with three named elements.
#'         'binary' => number of binary variables,
#'         'integer' => number of integer variables,
#'         'continuous' => number of continuous variables.
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x[i], i = 1:10, type = "binary") %>%
#'   add_variable(y[i], i = 1:5, type = "continuous") %>%
#'   add_variable(z[i], i = 1:2, type = "integer")
#' nvars(model)
#' @export
nvars <- function(model) UseMethod("nvars")

#' @export
nvars.optimization_model <- function(model) {
  stopifnot(is.list(model$variables))
  mapped_vars <- Map(f = function(var) {
    setNames(length(var$instances), var$type)
  }, model$variables)
  Reduce(f = function(acc, el) {
    acc[[names(el)]] <- acc[[names(el)]] + as.numeric(el)
    acc
  }, mapped_vars, init = list(
    continuous = 0, integer = 0,
    binary = 0
  ))
}

#' Variable types of a model
#'
#' One component for each variable in the correct order
#' @param model the model
#' @return a factor with levels binary, continuous, integer
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x, type = "binary") %>%
#'   add_variable(y, type = "continuous") %>%
#'   add_variable(z, type = "integer")
#' variable_types(model)
#' @export
variable_types <- function(model) UseMethod("variable_types")

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

#' Variable lower and upper bounds of a model
#'
#' @param model the model
#'
#' @return a list with two components 'lower' and 'upper' each
#' having a numeric vector of bounds. One for each variable.
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x, type = "binary") %>%
#'   add_variable(y, type = "continuous", lb = 2) %>%
#'   add_variable(z, type = "integer", ub = 3)
#' variable_bounds(model)
#' @export
variable_bounds <- function(model) UseMethod("variable_bounds")

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

#' Number of variables (rows) of the model
#'
#' @param model the model
#'
#' @return An integer equal to the number of variables. A
#' variable is here a column in the resulting constraint matrix.
#'
#' @examples
#' library(magrittr)
#' model <- MIPModel() %>%
#'   add_variable(x) %>%
#'   add_variable(y[i], i = 1:10)
#' nconstraints(model) # 11
#' @export
nconstraints <- function(model) UseMethod("nconstraints")

#' @export
nconstraints.optimization_model <- function(model) {
  length(model$constraints)
}
