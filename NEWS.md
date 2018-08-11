# ompr 0.8.0.9000

* You can now assign coefficients to all column/row combinations using `colwise`
in the experimental `MILPModel` backend.
* Non-existent indexes in `sum_expr` now produce a warning instead of an error. The missing indexes will be ignored ([#202](https://github.com/dirkschumacher/ompr/issues/202)).

# ompr 0.8.0

## General changes

* Removed `dplyr` dependency
* Added `MILPModel`, a new, vectorized backend for mixed integer linear programs that can handle very large models. It will eventually replace `MIPModel`.
* Added two functions (`get_column_duals`, `get_row_duals`) to extract the dual (column and row) values from an LP.
* The minimum supported R version is now `3.2.0`
* `get_solution` now always return a solution, even if the solution status is not optimal.
* `get_solution` has a third argument `type` with permitted values being "primal" and "dual" to return the respective column primal or dual values.

## Bugfixes

* You can now extract solutions of indexed variables that have length one (#[198](https://github.com/dirkschumacher/ompr/issues/198))

# ompr 0.7.0

## Breaking changes

* `ompr` now uses sparse constraint matrices. `extract_constraints` now returns a sparse matrix and `objective_function` returns a sparse vector.
* The minimum supported R version is now `3.3.0`
* Fixed an issue with `Rcpp`. The minimum `Rcpp` version is now `0.12.12`

## Minor changes

* New progress bar based on the `progress` package.

# ompr 0.6.0

* First version on CRAN


