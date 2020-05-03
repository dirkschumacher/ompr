# Mixed integer linear programming in R

<!-- badges: start -->
  [![R build status](https://github.com/dirkschumacher/ompr/workflows/R-CMD-check/badge.svg)](https://github.com/dirkschumacher/ompr/actions)
[![GPL Licence](https://badges.frapsoft.com/os/gpl/gpl.svg?v=103)](https://opensource.org/licenses/GPL-3.0/)
[![CRAN Status](https://www.r-pkg.org/badges/version/ompr)](https://cran.r-project.org/package=ompr)
  <!-- badges: end -->
  
OMPR (Optimization Modelling Package) is a DSL to model and solve Mixed Integer Linear Programs. It is inspired by the excellent Jump project in Julia.

Here are some problems you could solve with this package:
  * What is the cost minimal way to visit a set of clients and return home afterwards?
  * What is the optimal conference time table subject to certain constraints (e.g. availability of a projector)?
  * [Sudokus](https://github.com/dirkschumacher/r-sudoku)
  
The [Wikipedia](https://en.wikipedia.org/wiki/Integer_programming) article gives a good starting point if you would like to learn more about the topic.

I am always happy to get bug reports or feedback. 

## Install

### CRAN

```R 
install.packages("ompr")
install.packages("ompr.roi")
```

### Development version

To install the current development version use devtools:

```R 
devtools::install_github("dirkschumacher/ompr")
devtools::install_github("dirkschumacher/ompr.roi")
```

## Available solver bindings

Package | Description | Build Linux | Build Windows | Test coverage
--- | --- | --- | --- | --- 
[ompr.roi](https://github.com/dirkschumacher/ompr.roi) | Bindings to ROI (GLPK, Symphony, CPLEX etc.) | [![Build Status](https://travis-ci.org/dirkschumacher/ompr.roi.svg?branch=master)](https://travis-ci.org/dirkschumacher/ompr.roi) | [![Build Status Windows](https://ci.appveyor.com/api/projects/status/github/dirkschumacher/ompr-roi?branch=master&svg=true)](https://ci.appveyor.com/project/dirkschumacher/ompr-roi) | [![Coverage Status](https://coveralls.io/repos/github/dirkschumacher/ompr.roi/badge.svg?branch=master)](https://coveralls.io/github/dirkschumacher/ompr.roi?branch=master)


## A simple example:

```R
library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

result <- MIPModel() %>%
  add_variable(x, type = "integer") %>%
  add_variable(y, type = "continuous", lb = 0) %>%
  set_bounds(x, lb = 0) %>%
  set_objective(x + y, "max") %>%
  add_constraint(x + y <= 11.25) %>%
  solve_model(with_ROI(solver = "glpk")) 
get_solution(result, x)
get_solution(result, y)
```

## API

These functions currently form the public API. More detailed docs can be found in the package function docs or on the [website](https://dirkschumacher.github.io/ompr)

### DSL
* `MIPModel()` create an empty mixed integer linear model (the old way)
* `MILPModel()` create an empty mixed integer linear model (the new way; experimental, especially suitable for large models)
* `add_variable()` adds variables to a model
* `set_objective()` sets the objective function of a model
* `set_bounds()` sets bounds of variables
* `add_constraint()` add constraints
* `solve_model()` solves a model with a given solver
* `get_solution()` returns the column solution (primal or dual) of a solved model for a given variable or group of variables
* `get_row_duals()` returns the row duals of a solution (only if it is an LP)
* `get_column_duals()` returns the column duals of a solution (only if it is an LP)

### Backends

There are currently two backends. A backend is the function that initializes an empty model. 

* `MIPModel()` is the standard MILP Model
* `MILPModel()` is a new backend specifically optimized for linear models and is about 1000 times faster than `MIPModel()`. It has slightly different semantics, as it is vectorized. Currently experimental, but it will replace the `MIPModel` eventually.

### Solver

Solvers are in different packages. `ompr.ROI` uses the ROI package which offers support for all kinds of solvers.

* `with_ROI(solver = "glpk")` solve the model with GLPK. Install `ROI.plugin.glpk`
* `with_ROI(solver = "symphony")` solve the model with Symphony. Install `ROI.plugin.symphony`
* `with_ROI(solver = "cplex")` solve the model with CPLEX. Install `ROI.plugin.cplex`
* ... See the [ROI package](https://CRAN.R-project.org/package=ROI) for more plugins.

 
## Further Examples

Please take a look at the [docs](https://dirkschumacher.github.io/ompr/articles/index.html) for bigger examples.

### Knapsack

```R
library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
max_capacity <- 5
n <- 10
weights <- runif(n, max = max_capacity)
MIPModel() %>%
  add_variable(x[i], i = 1:n, type = "binary") %>%
  set_objective(sum_expr(weights[i] * x[i], i = 1:n), "max") %>%
  add_constraint(sum_expr(weights[i] * x[i], i = 1:n) <= max_capacity) %>%
  solve_model(with_ROI(solver = "glpk")) %>% 
  get_solution(x[i]) %>% 
  filter(value > 0)
```

### Bin Packing
An example of a more difficult model solved by symphony.

```R
library(dplyr)
library(ROI)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)
max_bins <- 10
bin_size <- 3
n <- 10
weights <- runif(n, max = bin_size)
MIPModel() %>%
  add_variable(y[i], i = 1:max_bins, type = "binary") %>%
  add_variable(x[i, j], i = 1:max_bins, j = 1:n, type = "binary") %>%
  set_objective(sum_expr(y[i], i = 1:max_bins), "min") %>%
  add_constraint(sum_expr(weights[j] * x[i, j], j = 1:n) <= y[i] * bin_size, i = 1:max_bins) %>%
  add_constraint(sum_expr(x[i, j], i = 1:max_bins) == 1, j = 1:n) %>%
  solve_model(with_ROI(solver = "symphony", verbosity = 1)) %>% 
  get_solution(x[i, j]) %>%
  filter(value > 0) %>%
  arrange(i)
```

## License

Currently GPL.

## Contributing

Please post an issue first before sending a PR.

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

## Related Projects

* [CVXR](https://cvxr.rbind.io/) - an excellent package for "object-oriented modeling language for convex optimization". LP/MIP is a special case.
* [ROML](https://r-forge.r-project.org/projects/roml/) follows a similiar approach, but it seems the package is still under initial development.
