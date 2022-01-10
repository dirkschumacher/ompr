# ompr (development version)

## General changes

* Rewrote the `MIPModel`. It should now be faster, more maintainable,
  more stable and it has fewer bugs.
* Added `sum_over`, a replacement for `sum_expr` in the `MIPModel`

## Bugfixes

* Fixed a bug where using the index "e" in `sum_expr` failed (#327)
* Fixed a bug where coefficients that came after the variable in the
  expression would sometimes not be correctly parsed (#265)
* Fixed a bug where `add_variable` failed if indexes were in the wrong order
  (#266)

## Deprecations

All listed functions will likely be removed at some later point the future.

* `sum_expr` shall not be used anymore. Please use `sum_over` instead.
* `MIPLModel` will likely be removed from the package, as the vectorized
  approach did lead to some problems. Please use `MIPModel` instead.
* `add_variable_`, `add_constraint_`, `set_objective_`, `set_bounds` and
  `get_solution_` are not needed anymore with the new `MIPModel` as it is
  powered by `rlang`.
* The `.show_progress_bar` parameter is now deprecated in all functions.

## Licensing

* ompr is now licensed under the MIT license (#353).

# ompr 0.8.1

## General changes

* You can now assign coefficients to all column/row combinations using `colwise`
in the experimental `MILPModel` backend.
* Non-existent indexes in `sum_expr` now produce a warning instead of an error. The missing indexes will be ignored ([#202](https://github.com/dirkschumacher/ompr/issues/202)).

## Bugfixes

* Fixed a bug were `get_solution` could return mixed up values when variables had partially similar names (eg: `s[i]` and `bus[i]`) by @hugolarzabal ([#244](https://github.com/dirkschumacher/ompr/issues/244)).
* Fixed a bug on where an if-condition had an input with `length != 1`.
* Fixed some minor issues with newer `data.table` versions

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


