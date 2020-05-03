# for data.table code
utils::globalVariables(c("coef", "constant", "mult"))

#' An S4 class that represents a collection of variables
#'
#' @slot variables a data frame hold the variable coefficients. One line for reach variable, row and column.
#' @slot index_mapping a function that takes a variable name as character and returns a mapping table that maps column ids to variable indexes.
setClass(
  "LinearVariableCollection",
  representation(variables = "data.frame", index_mapping = "function"),
  prototype(variables = data.table::data.table(
    variable = character(0L),
    row = integer(0L),
    col = integer(0L),
    coef = numeric(0L)
  ))
)

#' An S4 class that represents a single variable
#'
#' @slot variable a linear variable collection with just one index '1'
setClass(
  "LinearVariable",
  representation(variable = "LinearVariableCollection")
)

#' Holds a sum of a constant and a linear variable collection
#'
#' @slot constant a numeric vector
#' @slot variables a variable collection
setClass(
  "LinearVariableSum",
  representation(constant = "data.frame", variables = "LinearVariableCollection"),
  prototype(constant = data.table::data.table(
    row = integer(0L),
    constant = numeric(0L)
  ))
)

is_colwise <- function(x) {
  isTRUE(attr(x, "LinearTransposedVector"))
}

#' Format variables colwise
#'
#' This function should be used if you to expand a variable
#' across columns and not rows. When passing a vector of indexes
#' to MILPModel variable, it creates a new row for each vector element.
#' With colwise you can create columns instead. Please see the examples
#' below.
#'
#' `colwise` is probably the concept that is likely to change in the future.
#'
#' @param ... create a colwise vector
#' @examples
#' \dontrun{
#' # vectors create matrix rows
#' # x[1, 1]
#' # x[2, 1]
#' # x[3, 1]
#' x[1:3, 1]
#'
#' # colwise() creates columns per row
#' # 1 * x[1, 1] + 2 * x[1, 2] + 3 * x[1, 3]
#' colwise(1, 2, 3) * x[1, colwise(1, 2, 3)]
#'
#' # or you have multiple rows and columns and different coefficients
#' # 1 * x[1, 1] + 2 * x[1, 2] + 3 * x[1, 3]
#' # 4 * x[2, 1] + 5 * x[2, 2] + 6 * x[1, 3]
#' colwise(1:6) * x[1:2, colwise(1:3)]
#' # in the example above, the colwise vector multiplied with the variable
#' # has an element per row and column
#' # in general, it can be a multiple of number of columns
#'
#' # you can also combine the two
#' # x[1, 1]
#' # x[2, 1] + x[2, 2]
#' # x[3, 1] + x[3, 2] + x[3, 2]
#' x[1:3, colwise(1, 1:2, 1:3)]
#' }
#' @export
colwise <- function(...) {
  elements <- list(...)
  if (length(elements) == 1L) {
    return(as_colwise(elements[[1L]]))
  }
  all_l_1 <- all(vapply(elements, function(x) {
    length(x) == 1L && is.numeric(x)
  }, logical(1L)))
  as_colwise(if (all_l_1) {
    as.numeric(elements)
  } else {
    do.call(c, lapply(elements, list))
  })
}

#' As_colwise
#'
#' Convert lists or vectors to colwise semantic.
#'
#' @param x a list of numeric vectors or a numeric vector
#'
#' @export
as_colwise <- function(x) {
  all_numeric <- if (is.list(x)) vapply(x, is.numeric, logical(1L)) else TRUE
  if (!(is.numeric(x) || (is.list(x) && all(all_numeric)))) {
    stop("Only numeric vectors or list of numeric vectors can be made colwise.", call. = FALSE)
  }
  attr(x, "LinearTransposedVector") <- TRUE
  x
}

#' Multiply
#'
#' It will multiply the numeric vector with both the constant and the variable in
#' 'LinearVariableSum'
#'
#' @param e1 an object of type 'LinearVariableSum'
#' @param e2 a numeric vector
setMethod("*", signature(e1 = "LinearVariableSum", e2 = "numeric"), function(e1, e2) {
  e1@variables <- e1@variables * e2

  mult_dt <- data.table::data.table(row = if (length(e2) == 1L) seq_len(max(e1@variables@variables$row)) else seq_along(e2), mult = e2)
  new_vars <- e1@constant
  new_vars <- merge(new_vars, mult_dt, "row")
  new_vars <- setDT(new_vars)[, constant := constant * mult]
  e1@constant <- new_vars[, c("row", "constant")]
  e1
})

#' Multiply
#'
#' It will multiply the numeric vector with both the constant and the variable in
#' 'LinearVariableSum'
#'
#' @param e2 an object of type 'LinearVariableSum'
#' @param e1 a numeric vector
setMethod("*", signature(e1 = "numeric", e2 = "LinearVariableSum"), function(e1, e2) {
  e2 * e1
})

#' Minus
#'
#' Equivalent to `e1 + -1 * e2`
#'
#' @param e1 an object of type 'LinearVariableSum'
#' @param e2 a numeric vector
setMethod("-", signature(e1 = "LinearVariableSum", e2 = "numeric"), function(e1, e2) {
  e1 + -1 * e2
})

#' Minus
#'
#' Equivalent to `(-1 * e2) - (-1 * e1)`
#'
#' @param e1 a numeric vector
#' @param e2 an object of type 'LinearVariableSum'
setMethod("-", signature(e1 = "numeric", e2 = "LinearVariableSum"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

#' Plus
#'
#' Add two object of 'LinearVariableSum'. I.e. variables + constants
#'
#' @param e1 an object of type 'LinearVariableSum'
#' @param e2 an object of type 'LinearVariableSum'
#'
#' @return Returns an object of type 'LinearVariableSum'
setMethod("+", signature(e1 = "LinearVariableSum", e2 = "LinearVariableSum"), function(e1, e2) {
  e1@variables <- e1@variables + e2@variables

  # join the constant
  e1@constant <- merge_two_constants(e1@constant, e2@constant)
  e1
})

#' Minus
#'
#' Equivalent to `e1 + (-1) * e2`
#'
#' @param e1 an object of type 'LinearVariableSum'
#' @param e2 an object of type 'LinearVariableSum'
#'
#' @return Returns an object of type 'LinearVariableSum'
setMethod("-", signature(e1 = "LinearVariableSum", e2 = "LinearVariableSum"), function(e1, e2) {
  e1 + (-1) * e2
})

#' Plus
#'
#' Adds the variables in the rhs to the variables in the lhs and returns another 'LinearVariableSum'.
#'
#' @param e1 an object of type 'LinearVariableSum'
#' @param e2 an object of type 'LinearVariableCollection'
#'
#' @return Returns an object of type 'LinearVariableSum'
setMethod("+", signature(e1 = "LinearVariableSum", e2 = "LinearVariableCollection"), function(e1, e2) {
  e1@variables <- e1@variables + e2
  e1
})

#' Plus
#'
#' Equivalent to `e2 + e1`
#'
#' @param e1 an object of type 'LinearVariableCollection'
#' @param e2 an object of type 'LinearVariableSum'
setMethod("+", signature(e1 = "LinearVariableCollection", e2 = "LinearVariableSum"), function(e1, e2) {
  e2 + e1
})

#' Minus
#'
#' Equivalent to `-1 * (e2 - e1)`
#'
#' @param e1 an object of type 'LinearVariableCollection'
#' @param e2 an object of type 'LinearVariableSum'
setMethod("-", signature(e1 = "LinearVariableCollection", e2 = "LinearVariableSum"), function(e1, e2) {
  -1 * (e2 - e1)
})

#' Minus
#'
#' Equivalent to `e1 + -1 * e2`
#'
#' @param e1 an object of type 'LinearVariableSum'
#' @param e2 an object of type 'LinearVariableCollection'
setMethod("-", signature(e1 = "LinearVariableSum", e2 = "LinearVariableCollection"), function(e1, e2) {
  e1 + -1 * e2
})

#' Plus
#'
#' Adds a constant (rhs) to constant slot of the lhs object.
#'
#' @param e1 an object of type 'LinearVariableSum'
#' @param e2 a numeric vector
#' @return an object of type 'LinearVariableSum'
setMethod("+", signature(e1 = "LinearVariableSum", e2 = "numeric"), function(e1, e2) {
  constant_dt <- numeric_to_constant_dt(variables = e1@variables@variables, e2)
  e1@constant <- merge_two_constants(e1@constant, constant_dt)
  e1
})

merge_two_constants <- function(x, y) {
  new_vars <- data.table::rbindlist(list(x, y))
  idx_names <- c("row")
  data.table::setkeyv(new_vars, idx_names)
  new_vars[, list(constant = sum(constant)), by = idx_names]
}

#' Plus
#'
#' Equivalent to `e2 + e1`
#'
#' @param e1 a numeric vector
#' @param e2 an object of type 'LinearVariableSum'
setMethod("+", signature(e1 = "numeric", e2 = "LinearVariableSum"), function(e1, e2) {
  e2 + e1
})

#' Unary Minus
#'
#' Equivalent to `e1 * (-1)`
#'
#' @param e2 a missing value
#' @param e1 an object of type 'LinearVariableSum'
setMethod("-", signature(e1 = "LinearVariableSum", e2 = "missing"), function(e1, e2) {
  e1 * (-1)
})

#' Unary Minus
#'
#' Equivalent to `e1 * (-1)`
#'
#' @param e2 a missing value
#' @param e1 an object of type 'LinearVariableCollection'
setMethod("-", signature(e1 = "LinearVariableCollection", e2 = "missing"), function(e1, e2) {
  e1 * (-1)
})

#' Unary Plus
#'
#' Equivalent to `e1`
#'
#' @param e2 a missing value
#' @param e1 an object of type 'LinearVariableSum'
setMethod("+", signature(e1 = "LinearVariableSum", e2 = "missing"), function(e1, e2) {
  e1
})

#' Unary Plus
#'
#' Equivalent to `e1`
#'
#' @param e2 a missing value
#' @param e1 an object of type 'LinearVariableCollection'
setMethod("+", signature(e1 = "LinearVariableCollection", e2 = "missing"), function(e1, e2) {
  e1
})

#' Plus
#'
#' Adds a constant numeric vector to a variable. The constant needs to be a vector of length 1.
#'
#' @param e1 an object of type 'LinearVariableCollection'
#' @param e2 a numeric vector without NAs
setMethod("+", signature(e1 = "LinearVariableCollection", e2 = "numeric"), function(e1, e2) {
  if (anyNA(e2)) {
    stop("You try to add a numeric vector that contains NA values to a variable.", call. = FALSE)
  }
  if (all(e2 == 0)) {
    return(e1)
  }
  err_msg <- paste0(
    "You have definied variables for ", max(e1@variables$row),
    " rows, but you add a constant with ", length(e2), " elements. ",
    "The length of the two have to match or the constant is of length 1, i.e. a scalar."
  )
  constant <- numeric_to_constant_dt(e1@variables, e2, err_msg)
  new("LinearVariableSum", variables = e1, constant = constant)
})

numeric_to_constant_dt <- function(variables, vec, error_msg) {
  no_rows <- max(variables$row)
  is_scalar <- length(vec) == 1L
  row_vec <- if (is_scalar) seq_len(no_rows) else seq_along(vec)
  if (no_rows != length(vec) && !is_scalar) {
    stop(error_msg, call. = FALSE)
  }
  data.table::data.table(row = row_vec, constant = vec, key = "row")
}

#' Plus
#'
#' Equivalent to `e2 + e1`
#'
#' @param e1 a numeric value
#' @param e2 an object of type 'LinearVariableCollection'
setMethod("+", signature(e1 = "numeric", e2 = "LinearVariableCollection"), function(e1, e2) {
  e2 + e1
})

#' Plus
#'
#' Equivalent to `e1 + -1 * e2`
#'
#' @param e1 an object of type 'LinearVariableCollection'
#' @param e2 a numeric value
setMethod("-", signature(e1 = "LinearVariableCollection", e2 = "numeric"), function(e1, e2) {
  e1 + -1 * e2
})

#' Minus
#'
#' Equivalent to `(-1 * e2) - (-1 * e1)`
#'
#' @param e1 a numeric value
#' @param e2 an object of type 'LinearVariableCollection'
setMethod("-", signature(e1 = "numeric", e2 = "LinearVariableCollection"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

#' Division
#'
#' Equivalent to `e1 * (1 / e2)`
#'
#' @param e1 an object of type 'LinearVariableCollection'
#' @param e2 a numeric value
setMethod("/", signature(e1 = "LinearVariableCollection", e2 = "numeric"), function(e1, e2) {
  e1 * (1 / e2)
})

#' Division
#'
#' Equivalent to `e1 * (1 / e2)`
#'
#' @param e2 a numeric value
#' @param e1 an object of type 'LinearVariableCollection'
setMethod("/", signature(e1 = "LinearVariableSum", e2 = "numeric"), function(e1, e2) {
  e1 * (1 / e2)
})

#' Plus
#'
#' Adds two variables together. Same values for variable, row and col will be added. Everything else merged.
#'
#' @param e1 an object of type 'LinearVariableCollection'
#' @param e2 an object of type 'LinearVariableCollection'
#'
#' @import data.table
setMethod("+", signature(e1 = "LinearVariableCollection", e2 = "LinearVariableCollection"), function(e1, e2) {
  expand_e2 <- nrow(e2@variables) == 1L &&
    nrow(e1@variables) > 1L &&
    length(unique(e1@variables$variable)) == 1L
  if (expand_e2) {
    temp <- e2
    e2 <- e1
    e1 <- temp
  }
  expand_e1 <- nrow(e1@variables) == 1L &&
    nrow(e2@variables) > 1L &&
    length(unique(e2@variables$variable)) == 1L
  if (expand_e1) {
    e1_vars <- e1@variables
    e1@variables <- data.table::data.table(
      variable = e1_vars[["variable"]],
      row = sort(unique(e2@variables$row)),
      col = e1_vars[["col"]],
      coef = e1_vars[["coef"]]
    )
  }

  new_vars <- data.table::rbindlist(list(e1@variables, e2@variables))
  idx_names <- c("variable", "row", "col")
  data.table::setkeyv(new_vars, idx_names)
  e1@variables <- new_vars[, list(coef = sum(coef)), by = idx_names]
  e1
})

#' Minus
#'
#' Equivalent to `e1 + -1 * e2`
#'
#' @param e1 an object of type 'LinearVariableCollection'
#' @param e2 an object of type 'LinearVariableCollection'
setMethod("-", signature(e1 = "LinearVariableCollection", e2 = "LinearVariableCollection"), function(e1, e2) {
  e1 + -1 * e2
})

#' Multiply
#'
#' Multiplies the coefficients rowwise with a given numeric vector.
#' If the numeric vector is a `linear_transposed_vector`, it will multiply the vector
#' with each variable per row.
#'
#' @param e1 an object of type 'LinearVariableCollection'
#' @param e2 a numeric vector
setMethod("*", signature(e1 = "LinearVariableCollection", e2 = "numeric"), function(e1, e2) {
  if (is_colwise(e2)) {
    val <- e2
    attr(val, "LinearTransposedVector") <- NULL
    # val needs to be a multiple of the number coefficients per variable
    # it will be recycled using standard R rules
    new_variables <- e1@variables[, list(coef = coef * val), by = "variable"]
    e1@variables[["coef"]] <- new_variables[["coef"]]
    e1
  } else {
    row_indexes <- if (length(e2) == 1L) unique(e1@variables[["row"]]) else seq_along(e2)
    mult_dt <- data.table::data.table(
      row = row_indexes,
      mult = e2, key = "row"
    )
    new_vars <- e1@variables
    new_vars <- merge(new_vars, mult_dt, "row")
    new_vars <- setDT(new_vars)[, coef := coef * mult]
    e1@variables <- new_vars[, c("variable", "row", "col", "coef")]
    e1
  }
})

#' Multiply
#'
#' Equivalent to `e2 * e1`
#'
#' @param e1 a numeric value
#' @param e2 an object of type 'LinearVariableCollection'
setMethod("*", signature(e1 = "numeric", e2 = "LinearVariableCollection"), function(e1, e2) {
  e2 * e1
})


#' Subset model variables
#'
#' This creates a new variable collection as a subset of the previously defined indexed variable.
#' A variable collection essentially is a data frame having values for rows and columns of the final model matrix.
#'
#' @param x an object of type 'LinearVariableCollection'
#' @param i a numeric vector or a colwise vector/list
#' @param j a numeric vector or a colwise vector/list
#' @param ... more a numeric vectors or a colwise vector/list
#' @param drop do not use this parameter
#' @return a new object of type 'LinearVariableCollection'
#'
#' @examples
#' \dontrun{
#' # vectors create matrix rows
#' # x[1, 1]
#' # x[2, 1]
#' # x[3, 1]
#' x[1:3, 1]
#'
#' # colwise() creates columns per row
#' # 1 * x[1, 1] + 2 * x[1, 2] + 3 * x[1, 3]
#' colwise(1, 2, 3) * x[1, colwise(1, 2, 3)]
#'
#' # you can also combine the two
#' # x[1, 1]
#' # x[2, 1] + x[2, 2]
#' # x[3, 1] + x[3, 2] + x[3, 2]
#' x[1:3, colwise(1, 1:2, 1:3)]
#' }
setMethod("[", signature("LinearVariableCollection", i = "ANY", j = "ANY", drop = "missing"), function(x, i, j, ..., drop) {
  var_name <- as.character(as.name(substitute(x)))
  counter <- 0
  indexes <- list()
  min_length <- 1L
  rows <- 1L

  llength <- function(x) {
    if (is_colwise(x) && is.numeric(x)) 1L else length(x)
  }
  update_indexes <- function(x) {
    counter <<- counter + 1
    indexes[[counter]] <<- if (is.list(x)) {
      lapply(x, as.integer)
    } else if (is_colwise(x)) {
      list(as.integer(x))
    } else {
      as.integer(x)
    }
  }
  if (!missing(i)) {
    stopifnot(is.numeric(i) | is.list(i) || is_colwise(i))
    rows <- pmax(llength(i), rows)
    min_length <- pmin(length(i), min_length)
    update_indexes(i)
  }
  if (!missing(j)) {
    stopifnot(is.numeric(j) | is.list(j) || is_colwise(j))
    rows <- pmax(llength(j), rows)
    min_length <- pmin(length(j), min_length)
    stopifnot(rows == length(j) || length(j) == 1L || is_colwise(j))
    update_indexes(j)
  }
  for (arg in list(...)) {
    stopifnot(is.numeric(arg) | is.list(arg) || is_colwise(arg))
    rows <- pmax(llength(arg), rows)
    min_length <- pmin(length(arg), min_length)
    stopifnot(rows == length(arg) || length(arg) == 1L || is_colwise(arg))
    update_indexes(arg)
  }
  index_mapping <- x@index_mapping(var_name)
  list_indexes <- vapply(indexes, is.list, logical(1L))
  names(indexes) <- paste0("V", seq_len(counter))
  new_indexes <- do.call(data.table::data.table, c(
    list(row = seq_len(rows)),
    indexes
  ))
  any_length_0 <- min_length == 0L
  if (any_length_0) {
    stop("One of the indexes of variable '", var_name, "' has a length 0 subscript.",
      " That means that you passed an empty vector as one of the indexes to this variable.",
      call. = FALSE
    )
  }

  if (any(list_indexes)) {
    dt_j <- lapply(names(indexes)[list_indexes], function(x) rlang::get_expr(rlang::quo(unlist(!!as.name(x)))))
    dt_call <- rlang::quo(new_indexes[, list(!!!dt_j), by = c("row", !!!names(indexes)[!list_indexes])])
    new_indexes <- rlang::eval_tidy(dt_call)
    colnames(new_indexes) <- c("row", names(indexes)[!list_indexes], names(indexes)[list_indexes])
  }
  join_cols <- names(indexes)
  if (!(all(join_cols %in% colnames(new_indexes))) ||
    !(all(join_cols %in% colnames(index_mapping)))) {
    stop("The variable '", var_name, "' does not seem to be defined properly", call. = FALSE)
  }
  cols <- merge(new_indexes, index_mapping, by = join_cols)
  if (nrow(cols) != nrow(new_indexes)) {
    warning("You used the variable '", var_name, "' with ", nrow(new_indexes),
      " indexes but only ", nrow(cols), " indexes will be used",
      call. = FALSE
    )
  }
  new_vars <- data.table::data.table(
    variable = var_name,
    row = cols[["row"]],
    col = cols[["col"]],
    coef = 1L
  )
  data.table::setkeyv(new_vars, c("variable", "row", "col"))
  x@variables <- new_vars
  x
})
