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
  structure(list(expression = expression,
                 original_expression = original_expression,
                 sense = sense), class = "model_objective")
}
