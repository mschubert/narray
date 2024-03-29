% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.r
\name{filter}
\alias{filter}
\title{Function to discard subsets of an array (NA or drop)}
\usage{
filter(X, along, FUN, subsets = base::rep(1, dim(X)[along]),
  na.rm = FALSE)
}
\arguments{
\item{X}{An n-dimensional array}

\item{along}{Along which axis to apply \code{FUN}}

\item{FUN}{Function to apply, needs to return \code{TRUE} (keep) or \code{FALSE}}

\item{subsets}{Subsets that should be used when applying \code{FUN}}

\item{na.rm}{Whether to omit columns and rows with \code{NA}s}
}
\value{
An array where filtered values are \code{NA} or dropped
}
\description{
Function to discard subsets of an array (NA or drop)
}
