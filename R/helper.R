#' @noRd
bind_expression <- function(var_name, exp, envir, bound_subscripts) {
  var_values <- as.list(envir)
  for (x in c(var_name, names(bound_subscripts))) {
    var_values[[x]] <- NULL
  }
  eval(substitute(substitute(x, var_values), list(x = exp)))
}

# extracts quantifiers and filters from lazydots
classify_quantifiers <- function(lazy_dots) {
  assignments <- names(lazy_dots) != ""
  list(
    quantifiers = lazy_dots[assignments],
    filters = lazy_dots[!assignments]
  )
}

# construct a quantifier candidate table
build_quantifier_candidates <- function(subscripts,
                                        subscript_names, filter_dots) {
  candidates <- expand.grid(subscripts, stringsAsFactors = FALSE)
  colnames(candidates) <- subscript_names
  if (length(filter_dots) > 0) {
    filter <- Reduce(function(acc, x) {
      rlang::quo(!!(acc) & (!!rlang::as_quosure(x$expr, x$env)))
    }, filter_dots, init = TRUE)
    filtered_candidates <- rlang::quo(candidates[(!!filter), , drop = FALSE])
    candidates <- rlang::eval_tidy(filtered_candidates, data = candidates)
  }
  rownames(candidates) <- NULL
  candidates
}

print_model <- function(x) {
  cat("Mixed integer linear optimization problem\n")
  var_count <- nvars(x)
  cat("Variables:\n")
  cat("  Continuous:", var_count$continuous, "\n")
  cat("  Integer:", var_count$integer, "\n")
  cat("  Binary:", var_count$binary, "\n")

  # obj function
  objective <- x$objective
  if (!is.null(objective) &&
    length(objective$sense) == 1) {
    cat(
      "Model sense:",
      if (objective$sense == "max") "maximize" else "minimize",
      "\n"
    )
  } else {
    cat("No objective function. \n")
  }

  # constraints
  cat("Constraints:", nconstraints(x), "\n")
}

# helper function to check variable bounds
#' @importFrom rlang abort
check_bounds <- function(lb, ub) {
  if (any(ub < lb)) {
    abort("The upper bound must not be smaller than the lower bound.")
  }
  if (any(!is.numeric(lb) | !is.numeric(ub))) {
    abort("lb and ub must be a number.")
  }
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
