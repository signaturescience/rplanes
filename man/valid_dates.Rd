% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{valid_dates}
\alias{valid_dates}
\title{Validate dates}
\usage{
valid_dates(seed_date, signal_date, resolution, warn_incomplete = FALSE)
}
\arguments{
\item{seed_date}{Last date available in seed object}

\item{signal_date}{First date available in signal object}

\item{resolution}{Character vector specifying the temporal resolution (e.g., "days", "weeks", "months")}

\item{warn_incomplete}{Logical as to whether or not the validation should warn for completeness of seed and signal; default is \code{FALSE}}
}
\value{
The validation will return with a \code{stop()} if there is an overlap or gap between seed and signal dates. Otherwise the function will invisibly return \code{TRUE} indicating that the date span is valid.
}
\description{
This function validates that there are no gaps or overlaps between dates specified in the "seed_date" and "signal_date". During plausibility component analyses, the function is called to validate the seed against the evaluated signal.
}
\examples{
seed_date <- as.Date("2023-03-08")
signal_date <- as.Date("2023-03-15")
valid_dates(seed_date = seed_date, signal_date = signal_date, resolution="weeks")
x <- try(valid_dates(seed_date = seed_date,
                          signal_date = signal_date,
                          resolution="days"), silent=TRUE)
x
x <- try(valid_dates(seed_date = seed_date,
                          signal_date = signal_date,
                          resolution="months"), silent=TRUE)
x
}
