% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-api.R
\name{set_objective}
\alias{set_objective}
\alias{set_objective_}
\title{Set the model objective}
\usage{
set_objective(model, expression, sense = c("max", "min"))

set_objective_(model, expression, sense = c("max", "min"))
}
\arguments{
\item{model}{the model}

\item{expression}{the linear objective as a sum of variables and constants}

\item{sense}{the model sense. Must be either "max" or "min".}
}
\value{
a Model with a new objective function definition
}
\description{
Set the model objective
}
\examples{
library(magrittr)
MIPModel() \%>\%
  add_variable(x, lb = 2) \%>\%
  add_variable(y, lb = 40) \%>\%
  set_objective(x + y, sense = "min")
}
