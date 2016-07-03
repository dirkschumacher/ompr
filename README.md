# Model MIPs in R

[![Build Status](https://travis-ci.org/dirkschumacher/ompr.png?branch=master)](https://travis-ci.org/dirkschumacher/ompr)
[![Coverage Status](https://coveralls.io/repos/github/dirkschumacher/ompr/badge.svg?branch=master)](https://coveralls.io/github/dirkschumacher/ompr?branch=master)

OMPR (Optimization Modelling Package in R) is a DSL to model and solve Mixed Integer Linear Programs. It is inspired by the excellent Jump project in Julia.

This is just a first pre-alpha version to test the DSL and with probably a lot of bugs. It is currently quite slow for realworld applications but I am working on it. Any feedback is greatly appreciated.

The documentation is incomplete.

Current version: 0.1.1

## Install

To install the current development version use devtools:

```R 
devtools::install_github("dirkschumacher/ompr")
devtools::install_github("dirkschumacher/ompr.roi")
```

## Quickstart

A simple problem:

```R
library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
result <- MIPModel() %>%
  add_variable(x, type = "integer") %>%
  add_variable(y, type = "continuous") %>%
  set_objective(x + y, "max") %>%
  add_constraint(x + y, "<=", 11.25) %>%
  solve_model(with_ROI(solver = "glpk")) 
get_solution(result, x)
get_solution(result, y)
```

Solve a Knapsack problem:

```R
library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
max_capacity <- 5
n <- 4
weights <- runif(n, max = max_capacity)
MIPModel() %>%
  add_variable(x[i], i = 1:n, type = "binary") %>%
  set_objective(sum_exp(weights[i] * x[i], i = 1:n), "max") %>%
  add_constraint(sum_exp(weights[i] * x[i], i = 1:n), "<=", max_capacity) %>%
  solve_model(with_ROI(solver = "glpk")) %>% 
  get_solution(x[i]) # this gives you a data.frame
```

## API

These functions currently form the public API. Anything else is even more unstable:

### DSL
* `MIPModel()` create an empty model
* `add_variable` adds variables to a model
* `set_objective` sets the objective function of a model
* `add_constraint` add constraints
* `solve_model` solves a model with a given solver
* `get_solution` returns the solution of a solved model for a given variable or group of variables

### Solver

Solvers are in different packages. `ompr.ROI` uses the ROI package which offers support for all kinds of solvers.

* `with_ROI(solver = "glpk")` solve the model with GLPK. Install `ROI.plugin.glpk`
* `with_ROI(solver = "symphony")` solve the model with Symphony. Install `ROI.plugin.symphony`
* `with_ROI(solver = "cplex")` solve the model with CPLEX. Install `ROI.plugin.cplex`
* ... See the [ROI package](https://cran.r-project.org/web/packages/ROI/index.html) for more plugins.

 
## Examples

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
  set_objective(sum_exp(weights[i] * x[i], i = 1:n), "max") %>%
  add_constraint(sum_exp(weights[i] * x[i], i = 1:n), "<=", max_capacity) %>%
  solve_model(with_ROI(solver = "glpk")) %>% 
  get_solution(x[i])
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
  set_objective(sum_exp(y[i], i = 1:max_bins), "min") %>%
  add_constraint(sum_exp(weights[j] * x[i, j], j = 1:n), "<=", y[i] * bin_size, i = 1:max_bins) %>%
  add_constraint(sum_exp(x[i, j], i = 1:max_bins), "==", 1, j = 1:n) %>%
  solve_model(with_ROI(solver = "symphony")) %>% 
  get_solution(x[i, j]) %>%
  filter(value > 0) %>%
  arrange(i)
```

### Traveling Salesman Problem

```R
library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
cities <- 6
distance_matrix <- as.matrix(dist(1:cities, diag = TRUE, upper = TRUE))
sub_tours <- Filter(function(x) length(x) > 0 & length(x) < cities, lapply(sets::cset_power(1:cities), as.double))
MIPModel() %>%
  add_variable(x[i, j], i = 1:cities, j = 1:cities, type = "binary") %>%
  set_objective(sum_exp(distance_matrix[i, j] * x[i, j], i = 1:cities, j = 1:cities), direction = "min") %>%
  add_constraint(x[i, i], "==", 0, i = 1:cities) %>%
  add_constraint(x[i, j] + x[j, i], "<=", 1, i = 1:cities, j = 1:cities) %>%
  add_constraint(sum_exp(x[i, j], j = 1:cities), "==", 1, i = 1:cities) %>%
  add_constraint(sum_exp(x[i, j], i = 1:cities), "==", 1, j = 1:cities) %>%
  add_constraint(sum_exp(x[i, j], i = sub_tours[[s]], j = sub_tours[[s]]), "<=",
                 length(sub_tours[[s]]) - 1, s = 1:length(sub_tours)) %>%
  solve_model(with_ROI(solver = "glpk")) %>% 
  get_solution(x[i, j]) %>%
  filter(value > 0)
```

## License

Currently GPL because I am not sure if I can license it MIT.
