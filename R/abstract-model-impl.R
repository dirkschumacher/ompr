add_variable.abstract_model <- function(.model, .variable, ...,
                                        type = "continuous",
                                        lb = -Inf, ub = Inf) {
  add_variable_(
    .model = .model,
    .variable = lazyeval::as.lazy(
      substitute(.variable), parent.frame()
    ),
    type = type,
    lb = lb,
    ub = ub,
    .dots = lazyeval::lazy_dots(...)
  )
}

set_objective.abstract_model <- function(model, expression,
                                         sense = c("max", "min")) {
  set_objective_(model,
    expression = lazyeval::as.lazy(
      substitute(expression),
      parent.frame()
    ),
    sense = sense
  )
}

add_constraint.abstract_model <- function(.model, .constraint_expr, ...,
                                          .show_progress_bar = TRUE) {
  add_constraint_(.model, lazyeval::as.lazy(
    substitute(.constraint_expr),
    parent.frame()
  ),
  .dots = lazyeval::lazy_dots(...),
  .show_progress_bar = .show_progress_bar
  )
}

set_bounds.abstract_model <- function(.model, .variable, ...,
                                      lb = NULL, ub = NULL) {
  set_bounds_(
    .model = .model,
    .variable = lazyeval::as.lazy(
      substitute(.variable), parent.frame()
    ),
    lb = lb,
    ub = ub,
    .dots = lazyeval::lazy_dots(...)
  )
}
