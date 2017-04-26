context("latex-math")

test_that("Evaluating in LaTeX environment gives correct output", {
  expect_error(latex_env("+"))
  expect_equal(latex_env("+ x"), "+x")
  expect_equal(latex_env("x + y"), "x + y")
  expect_equal(latex_env("x[i]"), "x_{i}")
  expect_equal(latex_env("x[i, j, k]"), "x_{i, j, k}")
  expect_equal(latex_env("2 * y"), "2y")
  expect_equal(latex_env("1 / 2"), "\\frac{1}{2}")
  expect_equal(latex_env("y + (x / 2)"), "y + \\left(\\frac{x}{2}\\right)")
  expect_equal(latex_env("sum_expr(v[i], i=1:n)"), "\\sum\\limits_{i=1}^{n}{v_{i}}")
  expect_equal(latex_env("sum_expr(x[i, j], i=1:n, j=1:m)"), "\\sum\\limits_{i=1}^{n}\\sum\\limits_{j=1}^{m}{x_{i, j}}")
  expect_equal(latex_env("cap[i]"), "cap_{i}")
  expect_equal(latex_env("cap(i)"), "\\operatorname{cap}_{i}")
})

test_that("LaTeX commands ouput as character or knit_asis class", {
  expr <- expression(v[i])
  expect_equal(class(latex_math(expr)), "character")
  expect_equal(class(knitr_math(expr)), "knit_asis")
  expect_equal(class(latex_math("v[i]")), "character")
  expect_equal(class(knitr_math("v[i]")), "knit_asis")
  expect_equal(latex_math(expr), "v_{i}")
  expect_equal(latex_math(expr, fmt = "\\[%s\\]"),"\\[v_{i}\\]")
})

n <- 3; v <- 1:3; w <- 4:6; W <- 3
model <- MIPModel() %>%
  add_variable(x[i], i = 1:n, type = "binary") %>%
  set_objective(sum_expr(v[i] * x[i], i = 1:n)) %>%
  add_constraint(sum_expr(w[i] * x[i], i = 1:n) <= W)

dir <- "\\text{max}\\hspace{1em}"
obj <- "1x_{1} + 2x_{2} + 3x_{3}"
orig_obj <- "\\sum\\limits_{i=1}^{n}{v_{i}x_{i}}"
con <- "4x_{1} + 5x_{2} + 6x_{3} \\leq 3"

test_that("Objective function output works", {
  expect_equal(class(latex_objective(model)), "character")
  expect_equal(class(knitr_objective(model)), "knit_asis")
  expect_equal(latex_objective(model, expand =  TRUE), paste(dir, obj))
  expect_equal(latex_objective(model, expand =  TRUE, sense = FALSE), obj)
  expect_equal(latex_objective(model), paste(dir, orig_obj))
  expect_equal(latex_objective(model, sense = FALSE), orig_obj)
  expect_equal(knitr_objective(model, expand = TRUE)[1], paste0("\\[", paste(dir, obj), "\\]"))
  expect_equal(knitr_objective(model)[1], paste0("\\[", paste(dir, orig_obj), "\\]"))
})

n <- 2; m <- 1
model2 <-
  MIPModel() %>%
  add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%
  add_constraint(sum_expr(x[i, j], i = 1:n) <= 5, j = 1:m) %>%
  add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n)

con2 <- "x_{1, 1} + x_{2, 1} \\leq 5, x_{1, 1} = 1, x_{2, 1} = 1"

test_that("Constraint output works", {
  expect_equal(class(latex_constraints(model)), "character")
  expect_equal(class(knitr_constraints(model)), "knit_asis")
  expect_equal(latex_constraints(model), con)
  expect_equal(latex_constraints(model2), con2)
  expect_equal(knitr_constraints(model)[1], paste0("\\[", con, "\\]"))
  expect_equal(knitr_constraints(model2)[1], paste0("\\[", con2, "\\]"))
})
