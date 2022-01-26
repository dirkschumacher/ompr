
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Mixed integer linear programming in R

<!-- badges: start -->

[![R build
status](https://github.com/dirkschumacher/ompr/workflows/R-CMD-check/badge.svg)](https://github.com/dirkschumacher/ompr/actions)
[![CRAN
Status](https://www.r-pkg.org/badges/version/ompr)](https://cran.r-project.org/package=ompr)
[![Codecov test
coverage](https://codecov.io/gh/dirkschumacher/ompr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/dirkschumacher/ompr?branch=master)
<!-- badges: end -->

OMPR (Optimization Modeling Package) is a DSL to model and solve Mixed
Integer Linear Programs. It is inspired by the excellent Jump project in
Julia.

Here are some problems you could solve with this package:

-   What is the cost minimal way to visit a set of clients and return
    home afterwards?
-   What is the optimal conference time table subject to certain
    constraints (e.g. availability of a projector)?
-   [Sudokus](https://github.com/dirkschumacher/r-sudoku)

The [Wikipedia](https://en.wikipedia.org/wiki/Integer_programming)
article gives a good starting point if you would like to learn more
about the topic.

I am always happy to get bug reports or feedback.

## Install

### CRAN

``` r
install.packages("ompr")
install.packages("ompr.roi")
```

### Development version

To install the current development version use devtools:

``` r
remotes::install_github("dirkschumacher/ompr")
remotes::install_github("dirkschumacher/ompr.roi")
```

## Available solver bindings

-   [ompr.roi](https://github.com/dirkschumacher/ompr.roi) - Bindings to
    ROI (GLPK, Symphony, CPLEX etc.)

## A simple example:

``` r
suppressPackageStartupMessages(library(dplyr, quietly = TRUE)) 
suppressPackageStartupMessages(library(ROI))
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

result <- MIPModel() |>
  add_variable(x, type = "integer") |>
  add_variable(y, type = "continuous", lb = 0) |>
  set_bounds(x, lb = 0) |>
  set_objective(x + y, "max") |>
  add_constraint(x + y <= 11.25) |>
  solve_model(with_ROI(solver = "glpk"))
get_solution(result, x)
#>  x 
#> 11
get_solution(result, y)
#>    y 
#> 0.25
```

## API

These functions currently form the public API. More detailed docs can be
found in the package function docs or on the
[website](https://dirkschumacher.github.io/ompr/)

### DSL

-   `MIPModel()` create an empty mixed integer linear model (the old
    way)
-   `add_variable()` adds variables to a model
-   `set_objective()` sets the objective function of a model
-   `set_bounds()` sets bounds of variables
-   `add_constraint()` add constraints
-   `solve_model()` solves a model with a given solver
-   `get_solution()` returns the column solution (primal or dual) of a
    solved model for a given variable or group of variables
-   `get_row_duals()` returns the row duals of a solution (only if it is
    an LP)
-   `get_column_duals()` returns the column duals of a solution (only if
    it is an LP)

### Backends

There are currently two backends. A backend is the function that
initializes an empty model.

-   `MIPModel()` is the standard MILP Model.
-   `MILPModel()` is another backend specifically optimized for linear
    models and is often faster than `MIPModel()`. It has different
    semantics, as it is vectorized. Currently experimental and might be
    deprecated in the future.

### Solvers

Solvers are in different packages. `ompr.ROI` uses the ROI package which
offers support for all kinds of solvers.

-   `with_ROI(solver = "glpk")` solve the model with GLPK. Install
    `ROI.plugin.glpk`
-   `with_ROI(solver = "symphony")` solve the model with Symphony.
    Install `ROI.plugin.symphony`
-   `with_ROI(solver = "cplex")` solve the model with CPLEX. Install
    `ROI.plugin.cplex`
-   … See the [ROI package](https://CRAN.R-project.org/package=ROI) for
    more plugins.

## Further Examples

Please take a look at the
[docs](https://dirkschumacher.github.io/ompr/articles/index.html) for
bigger examples.

### Knapsack

``` r
max_capacity <- 5
n <- 10
set.seed(1234)
weights <- runif(n, max = max_capacity)
MIPModel() |>
  add_variable(x[i], i = 1:n, type = "binary") |>
  set_objective(sum_over(weights[i] * x[i], i = 1:n), "max") |>
  add_constraint(sum_over(weights[i] * x[i], i = 1:n) <= max_capacity) |>
  solve_model(with_ROI(solver = "glpk")) |>
  get_solution(x[i]) |>
  filter(value > 0)
#>   variable i value
#> 1        x 1     1
#> 2        x 6     1
#> 3        x 7     1
#> 4        x 8     1
```

### Bin Packing

An example of a more difficult model solved by GLPK

``` r
max_bins <- 10
bin_size <- 3
n <- 10
weights <- runif(n, max = bin_size)
MIPModel() |>
  add_variable(y[i], i = 1:max_bins, type = "binary") |>
  add_variable(x[i, j], i = 1:max_bins, j = 1:n, type = "binary") |>
  set_objective(sum_over(y[i], i = 1:max_bins), "min") |>
  add_constraint(sum_over(weights[j] * x[i, j], j = 1:n) <= y[i] * bin_size, i = 1:max_bins) |>
  add_constraint(sum_over(x[i, j], i = 1:max_bins) == 1, j = 1:n) |>
  solve_model(with_ROI(solver = "glpk", verbose = TRUE)) |>
  get_solution(x[i, j]) |>
  filter(value > 0) |>
  arrange(i)
#> <SOLVER MSG>  ----
#> GLPK Simplex Optimizer, v4.65
#> 20 rows, 110 columns, 210 non-zeros
#>       0: obj =   0.000000000e+00 inf =   1.000e+01 (10)
#>      29: obj =   4.546337429e+00 inf =   0.000e+00 (0)
#> *    34: obj =   4.546337429e+00 inf =   0.000e+00 (0)
#> OPTIMAL LP SOLUTION FOUND
#> GLPK Integer Optimizer, v4.65
#> 20 rows, 110 columns, 210 non-zeros
#> 110 integer variables, all of which are binary
#> Integer optimization begins...
#> Long-step dual simplex will be used
#> +    34: mip =     not found yet >=              -inf        (1; 0)
#> +    62: >>>>>   5.000000000e+00 >=   5.000000000e+00   0.0% (13; 0)
#> +    62: mip =   5.000000000e+00 >=     tree is empty   0.0% (0; 25)
#> INTEGER OPTIMAL SOLUTION FOUND
#> <!SOLVER MSG> ----
#>    variable  i  j value
#> 1         x  1  2     1
#> 2         x  1  9     1
#> 3         x  1 10     1
#> 4         x  2  5     1
#> 5         x  2  7     1
#> 6         x  2  8     1
#> 7         x  3  6     1
#> 8         x  4  4     1
#> 9         x 10  1     1
#> 10        x 10  3     1
```

## License

MIT

## Contributing

Please post an issue first before sending a PR.

Please note that this project is released with a Contributor Code of
Conduct. By participating in this project you agree to abide by its
terms.

## Related Projects

-   [CVXR](https://cvxr.rbind.io/) - an excellent package for
    “object-oriented modeling language for convex optimization”. LP/MIP
    is a special case.
-   [ROML](https://r-forge.r-project.org/projects/roml/) follows a
    similar approach, but it seems the package is still under initial
    development.
