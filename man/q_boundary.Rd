% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{q_boundary}
\alias{q_boundary}
\title{Quantile boundary}
\usage{
q_boundary(pi_width)
}
\arguments{
\item{pi_width}{Interval width as an integer}
}
\value{
Vector of quantiles corresponding to lower and upper bounds centered on median.
}
\description{
This unexported helper generates a vector of lower bound, median, and upper bound for the prediction interval of specified width. The function is used internally inside of \link{read_forecast}.
}
