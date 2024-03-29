% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solution-api.R
\name{additional_solver_output}
\alias{additional_solver_output}
\title{Retrieve additional solver specific output}
\usage{
additional_solver_output(solution)
}
\arguments{
\item{solution}{a solution object}
}
\value{
A list of named entries. What is in that list is determined
by the solver function. For \code{ompr.roi} this is usually a solver specific
message and status information.
}
\description{
Retrieve additional solver specific output
}
