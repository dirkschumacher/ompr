#' Output expression as LaTeX math
#'
#' Convert a single constraint, objective function, or some other R
#' expression object to a LaTeX math command. \code{knitr_math()} is a
#' convenience wrapper for \code{latex_math()} plus the knitr chunk
#' option \code{results = 'asis'}.
#'
#' @param x Expression or character string
#' @param model MIP model of class \code{optimization_model}
#' @param expand Whether to process the expanded or original expression
#' @param sense Whether to include the model sense in the output
#' @param fmt Format string, a la \code{\link[base]{sprintf}}
#' @param collapse Character string to separate constraints
#' @param \dots Additional arguments to be passed to
#'   \code{knitr::asis_output()}
#'
#' @return LaTeX command as a character string for \code{latex_math()}.
#'   knit_asis object for \code{knitr_math()}.
#'
#' @export
latex_math <- function(x, fmt = "%s") {

  sprintf(fmt, latex_env(x))

}

#' @rdname latex_math
#' @export
knitr_math <- function(x, fmt = "\\[%s\\]", ...) {

  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Please install knitr to use this function.",
         call. = FALSE)
  }

  knitr::asis_output(latex_math(x, fmt), ...)

}

#' @rdname latex_math
#' @export
latex_constraints <- function(model, fmt = "%s", collapse = ", ") {
  UseMethod("latex_constraints")
}

#' @export
latex_constraints.optimization_model <- function(model, fmt = "%s", collapse = ", ") {

  latex <- lapply(model$constraints, function(x) {
    cons <- paste(c(x$lhs, x$sense, x$rhs), collapse = " ")
    latex_math(parse(text = cons))
  })

  sprintf(fmt, paste(unlist(latex), collapse = collapse))

}

#' @rdname latex_math
#' @export
knitr_constraints <- function(model, fmt = "\\[%s\\]", collapse = ", ", ...) {
  UseMethod("knitr_constraints")
}

#' @export
knitr_constraints.optimization_model <- function(model, fmt = "\\[%s\\]", collapse = ", ", ...) {

   knitr::asis_output(latex_constraints(model, fmt, collapse, ...), ...)

}

#' @rdname latex_math
#' @export
latex_objective <- function(model, expand = FALSE, sense = TRUE, fmt = "%s") {
  UseMethod("latex_objective")
}

#' @export
latex_objective.optimization_model <- function(model, expand = FALSE, sense = TRUE, fmt = "%s") {

  obj <- ifelse(expand, model$objective$expression, model$objective$original_expression)
  dir <- sprintf("\\text{%s}\\hspace{1em}", model$objective$sense)
  m <- latex_math(obj, fmt = "%s")

  sprintf(fmt, ifelse(sense, paste(dir, m), m))

}

#' @rdname latex_math
#' @export
knitr_objective <- function(model, expand = FALSE, sense = TRUE, fmt = "\\[%s\\]", ...) {
  UseMethod("knitr_objective")
}

#' @export
knitr_objective.optimization_model <- function(model, expand = FALSE, sense = TRUE, fmt = "\\[%s\\]", ...) {

  knitr::asis_output(latex_objective(model, expand, sense, fmt), ...)

}

#' Convert call object to LaTeX
#'
#' Evaluates a function call in an R to LaTeX translation environment
#' and returns the resulting command as an character string.
#'
#' @param x Unevaluated call as character string
#'
#' @return Latex command
#'
#' @noRd
#' @keywords internal
latex_env <- function(x) {

  x <- parse(text = x)[[1]]

  # Get expression symbols (i.e., decision variables)
  sym <- all_symbols(x)
  symbols <- lapply(setNames(sym, sym), force)

  # Handle unknown function calls
  unk <- setdiff(all_calls(x), ls(lenv))
  unknown <- lapply(setNames(unk, unk), unknown_op)

  # Create Latex environment
  env <- list2env(c(as.list(lenv), unknown, symbols), parent = emptyenv())

  eval(x, env)

}

#' Recursively extract all call objects
#'
#' @noRd
#' @keywords internal
all_calls <- function(x) {

  if (!is.call(x)) return(NULL)

  unique(c(as.character(x[[1]]), unlist(lapply(x[-1], all_calls))))

}

#' Recursively extract all expression symbols
#'
#' @noRd
#' @keywords internal
all_symbols <- function(x) {

  if ( is.name(x)) return(as.character(x))
  if (!is.call(x)) return(NULL)

  unique(unlist(lapply(x[-1], all_symbols)))

}

# Latex Environment
lenv <- new.env(parent = emptyenv())

lenv$`+`      <- function(a, b) {
  if (missing(b)) return(paste0("+", a))
  paste0(a, " + ", b)
}

lenv$`-`      <- function(a, b) {
  if(missing(b)) return(paste0("-", a))
  paste0(a, " - ", b)
}

lenv$`*`      <- function(a, b) paste0(a, b)
lenv$`/`      <- function(a, b) paste0("\\frac{", a, "}{", b, "}")
lenv$`^`      <- function(a, b) paste0(a, "^{", b, "}")
lenv$`:`      <- function(a, b) paste0(a, "}^{", b, "}")
lenv$`%%`     <- function(a, b) paste0(a, " mod ", b)
lenv$`==`     <- function(a, b) paste0(a, " = ", b)
lenv$`<=`     <- function(a, b) paste0(a, " \\leq ", b)
lenv$`>=`     <- function(a, b) paste0(a, " \\geq ", b)
lenv$`(`      <- function(a) paste0("\\left(", a, "\\right)")
lenv$sqrt     <- function(a) paste0("\\sqrt{", a, "}")
lenv$log      <- function(a) paste0("\\log{", a, "}")
lenv$abs      <- function(a) paste0("\\left|", a, "\\right|")
lenv$floor    <- function(a) paste0("\\lfloor", a, "\\rfloor")
lenv$ceil     <- function(a) paste0("\\lceil", a, "\\rceil")

lenv$sum_expr <- function(a, ...) {
  dots <- unlist(list(...))
  i <- dots[grep("\\}\\^\\{.*\\}$", dots)]
  sig <- paste0("\\sum\\limits_{", names(i), "=", i, collapse = "")
  paste0(sig, "{", a, "}")
}

lenv$`[`      <- function(a, ...) {
  dots <- list(...)
  paste0(a, "_{", paste(dots, collapse = ", "), "}")
}

unknown_op <- function(op) {

  force(op)

  function(...) {
    dots <- list(...)
    i <- paste(dots, collapse=", ")
    paste0("\\operatorname{", op, "}_{", i, "}")
  }

}
