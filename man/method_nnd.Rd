% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method_nnd.R
\name{method_nnd}
\alias{method_nnd}
\title{An internal function to use the nnd algorithm via rnndescent package}
\usage{
method_nnd(x, y, k, distance, verbose, n_threads, control)
}
\arguments{
\item{x}{Deduplication or reference data.}

\item{y}{Query data.}

\item{k}{Number of neighbours to return.}

\item{distance}{Type of distance to calculate.}

\item{verbose}{If TRUE, log messages to the console.}

\item{n_threads}{Maximum number of threads to use.}

\item{control}{Controls for the NND algorithm}
}
\description{
See details of \link[rnndescent:rnnd_build]{rnndescent::rnnd_build} and \link[rnndescent:rnnd_query]{rnndescent::rnnd_query}
}
\author{
Maciej Beręsewicz
}