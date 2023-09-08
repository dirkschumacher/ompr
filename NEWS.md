# ompr 1.0.4

* Improves the package documentation to use the correct alias.
* Ompr now requires R 3.5.0 because some dependencies require that now.

# ompr 1.0.3

## Bugfixes

* `extract_constraints()` previously created explicit 0 values in the sparse
matrix. They are now implicit.

# ompr 1.0.2

## Bugfixes

* Fixed a bug where `get_solution` returns incorrect results on R
  version `< 4`. Affected package versions are `1.0.0` and `1.0.1`. (#404)

## General changes

* Model-building is now significantly faster
* Constraints without variables that evaluate to `TRUE` are not added to the
  model, as they are always satisfied. Likewise, constraints that evaluate to
  `FALSE` throw an error. Previously specifying a constraint without
  a variable would have caused a run time error.
* The minimum required R version is now `3.4` as {ompr.roi} has the same
  minimum R version.

# ompr 1.0.1

## Bugfixes

* `additional_solver_output()` is now an exported function.

# ompr 1.0.0

## General changes

* Rewrote the `MIPModel`. It should now be faster, more maintainable,
  more stable and it has fewer bugs.
* Added `sum_over`, a replacement for `sum_expr` in the `MIPModel`
* `set_bounds` for `MIPModel` now accepts (in)equalities as well (#365)
* `MIPModel` now supports characters as variable indexes
* A `solution` object has a new named entry called `additional_solver_output`.
  In that place solver packages, like `ompr.roi` can store arbitrary solver
  information. Including solver specific messages and status codes. It should
  be read using the function `additional_solver_output()`.
* A `solution` can now have the `solver_status = "success"` which is used
  by the most recent `ompr.roi` version.

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

## Breaking Changes

* `extract_constraints` now always returns a sparse matrix, even if there are 0
  constraints or variables.
* The row ordering of the `data.frame` returned with `get_solution(x[i, j])` has
  slightly changed in special cases, but for the majority of calls, it
  should stay the same. One of these special cases is if you created your
  variable similar to `add_variable(model, x[i, j], j = ..., i = ...)`, where
  the indexes in the variable and the quantifiers have different orderings.
  In general, please do not depend on the ordering of the rows, but use the
  indexes to retrieve the correct value. For example by sorting the `data.frame`
  , before reading.

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


