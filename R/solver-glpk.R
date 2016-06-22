#' This function constructs a glpk solver
#' @param verbose would you like to see GLPK messages?
#' @export
configure_glpk_solver <- function(verbose = FALSE) {
  function(model) {
    # define the order of all variables
    vars <- model@variables
    var_list <- unlist(lapply(names(vars), function(key) {
      var <- vars[[key]]
      if (length(var@instances) == 1 && nchar(var@instances) == 0) {
        key
      } else {
        paste0(key, "_", var@instances)
      }
    }))
    column_types <- unlist(lapply(names(vars), function(key) {
      var <- vars[[key]]
      rep.int(x = var@type, times = length(var@instances))
    }))
    ncols <- length(var_list)
    nrows <- length(model@constraints)

    # build column bounds
    column_bounds_l <- unlist(lapply(names(vars), function(key) {
      var <- vars[[key]]
      lb <- if (length(var@lb) == 0) -Inf else var@lb
      if (var@type == "binary") lb <- 1
      rep.int(x = lb, times = length(var@instances))
    }))

    column_bounds_u <- unlist(lapply(names(vars), function(key) {
      var <- vars[[key]]
      ub <- if (length(var@ub) == 0) Inf else var@ub
      if (var@type == "binary") ub <- 1
      rep.int(x = ub, times = length(var@instances))
    }))

    # build objective coeffcient vector
    objective <- model@objective
    build_coefficent_vector <- function(extracted_coefficients) {
      coef_vector <- rep.int(0, ncols)
      coefficients <- extracted_coefficients
      names(coefficients) <- NULL
      obj_constant <- extracted_coefficients$constant
      bound_coefs <- unlist(Map(function(var_coef) {
        var_ast <- var_coef$ast
        if (is.call(var_ast) && length(var_ast) > 1) {
          var_name <- as.character(var_ast[[2]])
          search_key <- paste0(c(var_name, as.character(var_ast[3:length(var_ast)])), collapse = "_")
        } else {
          var_name <- as.character(var_ast)
          search_key <- var_name
        }
        setNames(var_coef$coef, search_key)
      }, coefficients))
      coef_positions <- which(var_list %in% names(bound_coefs))
      if (length(coef_positions) > 0) {
        coef_vector[coef_positions] <- as.numeric(bound_coefs)
      }
      coef_vector
    }
    if (!is.null(objective)) {
      coefficients <- extract_coefficients(model@objective@expression[[1]])
      obj_constant <- coefficients$constant
      if (!is.numeric(obj_constant)) obj_constant <- 0
      coefficients <- coefficients$coefficients
      names(coefficients) <- NULL
      obj_vector <- build_coefficent_vector(coefficients)
    } else {
      obj_constant <- 0
      obj_vector <- rep.int(0, ncols)
    }

    # build constraint matrix
    matrices <- lapply(model@constraints, function(constraint) {
      coefficients_lhs <- extract_coefficients(constraint@lhs[[1]])
      coefficients_rhs <- extract_coefficients(constraint@rhs[[1]])
      direction <- constraint@direction
      list(
        lhs = build_coefficent_vector(coefficients_lhs$coefficients),
        rhs = build_coefficent_vector(coefficients_rhs$coefficients),
        direction = direction,
        lhs_constant = coefficients_lhs$constant,
        rhs_constant = coefficients_rhs$constant
      )
    })
    constraint_matrix <- t(rbind(sapply(matrices, function(constraint) {
      constraint$lhs - constraint$rhs
    })))

    # build row upper bound (aka b)
    constraint_rhs <- sapply(matrices, function(constraint) {
      constraint$rhs_constant - constraint$lhs_constant
    })

    constraint_dir <- sapply(matrices, function(constraint) {
      constraint$direction
    })

    binary_vars <-
      row_ub <- sapply(matrices, function(constraint) {
        if (constraint$direction != ">=") {
          constraint$rhs_constant - constraint$lhs_constant
        } else {
          Inf
        }
      })
    # build row lower bound
    row_lb <- sapply(matrices, function(constraint) {
      if (constraint$direction != "<=") {
        constraint$rhs_constant - constraint$lhs_constant
      } else {
        -Inf
      }
    })

    # this will become a separte component
    stopifnot(require("Rglpk"))
    glpk_column_types <- column_types
    glpk_column_types[glpk_column_types == "binary"] <- "B"
    glpk_column_types[glpk_column_types == "integer"] <- "I"
    glpk_column_types[glpk_column_types == "continuous"] <- "C"

    result <- Rglpk_solve_LP(obj_vector,
                   constraint_matrix,
                   constraint_dir,
                   constraint_rhs,
                   bounds = list(
                     lower = list(
                       ind = 1:length(column_bounds_l),
                       val = column_bounds_l
                      ),
                     upper = list(
                       ind = 1:length(column_bounds_u),
                       val = column_bounds_u
                     )
                   ),
                   types = glpk_column_types,
                   max = model@objective@direction == "max",
                   control = list(verbose = verbose))

    status <- if (result$status == 0) "optimal" else "infeasible"
    solution <- result$solution
    # the solution should be named
    names(solution) <- sapply(var_list, function(var) {
      if (grepl(x = var, pattern = "_", fixed = TRUE)) {
        splited_els <- strsplit(var, "_", fixed = TRUE)[[1]]
        paste0(splited_els[1], "[",
               paste0(splited_els[2:length(splited_els)], collapse = ","),
               "]")
      } else {
        var
      }
    })
    solution <- new("Solution",
                    status = status,
                    model = model,
                    objective_value = result$optimum + obj_constant,
                    solution = solution)
    solution
  }
}
