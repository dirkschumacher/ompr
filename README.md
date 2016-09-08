# Model MIPs in R

[![Build Status](https://travis-ci.org/dirkschumacher/ompr.svg?branch=master)](https://travis-ci.org/dirkschumacher/ompr)
[![Coverage Status](https://coveralls.io/repos/github/dirkschumacher/ompr/badge.svg?branch=master)](https://coveralls.io/github/dirkschumacher/ompr?branch=master)
[![Build Status Windows](https://ci.appveyor.com/api/projects/status/github/dirkschumacher/ompr?branch=master&svg=true)](https://ci.appveyor.com/project/dirkschumacher/ompr)

OMPR (Optimization Modelling Package in R) is a DSL to model and solve Mixed Integer Linear Programs. It is inspired by the excellent Jump project in Julia.

Here are some problems you could solve with this package:
  * What is the cost minimal way to visit a set of clients and return home afterwards?
  * What is the optimal conference time table subject to certain constraints (e.g. availability of a projector)?
  * If you run a radio station :) What is the optimal way to play music such that your users do not have to listen to the same songs too often?
  
The [Wikipedia](https://en.wikipedia.org/wiki/Integer_programming) article gives a good starting point if you would like to learn more about the topic.

This is just a first pre-alpha version to test the DSL and with probably a lot of bugs. It is currently quite slow for realworld applications but I am working on it. Any feedback is greatly appreciated.

The documentation is incomplete.

Current version: 0.2.2

Please refer to the `vignettes` for more detailed examples (`browseVignettes("ompr")`).

## Install

To install the current development version use devtools:

```R 
devtools::install_github("dirkschumacher/ompr")
devtools::install_github("dirkschumacher/ompr.roi")
```

## Available solver bindings

Package | Description | Build Linux | Build Windows | Test coverage
--- | --- | --- | --- | --- 
[ompr.roi](https://github.com/dirkschumacher/ompr.roi) | Bindings to ROI (GLPK, Symphony, CPLEX etc.) | [![Build Status](https://travis-ci.org/dirkschumacher/ompr.roi.svg?branch=master)](https://travis-ci.org/dirkschumacher/ompr.roi) | [![Build Status Windows](https://ci.appveyor.com/api/projects/status/github/dirkschumacher/ompr.roi?branch=master&svg=true)](https://ci.appveyor.com/project/dirkschumacher/ompr.roi) | [![Coverage Status](https://coveralls.io/repos/github/dirkschumacher/ompr.roi/badge.svg?branch=master)](https://coveralls.io/github/dirkschumacher/ompr.roi?branch=master)


## A simple example:

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

## Roundtrip through the US

Inspired by Randal Olson's [blog post](http://www.randalolson.com/2015/03/10/computing-the-optimal-road-trip-across-europe/) let's calculate the *optimal* trip through a couple of US cities. The data comes from the `psyc` package and has distances between 11 US cities.

```R

library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
library(psych) # for the cities matrix
data(cities)
n <- ncol(cities)

# we model the TSP with the MTZ formulation
# https://www.unc.edu/~pataki/papers/teachtsp.pdf
result <- MIPModel() %>%
  # we create a variable that is 1 iff we travel from city i to j
  add_variable(x[i,j], i = 1:n, j = 1:n, type = "binary") %>%
  # a helper variable for the MTZ formulation of the tsp
  add_variable(u[i], i = 1:n, lb = 1, ub = n) %>% 
  # minimize travel time
  set_objective(sum_exp(cities[i,j] * x[i,j], i = 1:n, j = 1:n), "min") %>%
  # you cannot go to the same city
  add_constraint(x[i,i], "==", 0, i = 1:n) %>%
  # leave each city
  add_constraint(sum_exp(x[i,j], j = 1:n), "==", 1, i = 1:n) %>%
  # visit each city
  add_constraint(sum_exp(x[i,j], i = 1:n), "==", 1, j = 1:n) %>%
  # ensure no subtours (arc constraints)
  add_constraint(u[1], "==", 1) %>% 
  add_constraint(u[i], ">=", 2, i = 2:n) %>% 
  add_constraint(u[i] - u[j] + 1, "<=", n * (1 - x[i,j]), i = 2:n, j = 2:n) %>% 
  # solve it with GLPK
  solve_model(with_ROI(solver = "glpk", verbose = TRUE)) %>% 
  # get the solution
  get_solution(x[i,j]) %>% 
  # filter only arcs that are used
  filter(value > 0) %>% 
  # join it back with the city names
  mutate(i = as.integer(as.character(i)), j = as.integer(as.character(j)), 
         from = rownames(cities)[i], to = colnames(cities)[j]) %>% 
  select(from, to) %>% ungroup
```

Let's geo code the airports and put them on a map.
```R
# geo coded places
geocoded_cities <- Map(function(x) {
  Sys.sleep(0.5)
  if (x == "DEN") x <- "Denver" # DEN does not work
  cbind(city = x, ggmap::geocode(paste0(x, ", USA")))
}, colnames(cities)) %>% bind_rows %>% 
  mutate(city = ifelse(city == "Denver", "DEN", city))
trips <- result %>% 
  mutate(trip_id = row_number()) %>% 
  tidyr::gather(key, city, to, from) %>% 
  inner_join(geocoded_cities, by = c("city")) %>% 
  arrange(trip_id, key)

library(leaflet)
m <- leaflet(geocoded_cities) %>% 
  addTiles() %>% 
  addMarkers(popup = geocoded_cities$city)
for(trip in unique(trips$trip_id)) {
  m <- addPolylines(m, data = filter(trips, trip_id == trip), lng = ~lon, lat = ~lat, group = ~trip_id)
}
m
```
![Map of the optimal trip](https://s3.eu-central-1.amazonaws.com/b6196ceb34f793115675a4bdb7757770/optimal_trip.png)

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

 
## Further Examples

Please refer to the `vignettes` for more examples.

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

## License

Currently GPL.
