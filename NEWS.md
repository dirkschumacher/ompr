# ompr 0.6.0.9000

...

## Breaking changes

* ompr now uses sparse constraint matrices. `extract_constraints` now returns a sparse matrix and `objective_function` returns a sparse vector.
* The minimum supported R version is now `3.3.0`

## Minor changes

* New progress bar based on the `progress` package.


* `get_solution` always returns a solution, regardles of the solver status

# ompr 0.6.0

* First version on CRAN


