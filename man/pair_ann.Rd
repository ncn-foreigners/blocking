% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reclin2_pair_ann.R
\name{pair_ann}
\alias{pair_ann}
\title{Integration with the reclin2 package}
\usage{
pair_ann(
  x,
  y = NULL,
  on,
  on_blocking = NULL,
  deduplication = TRUE,
  keep_block = TRUE,
  add_xy = TRUE,
  ...
)
}
\arguments{
\item{x}{reference data (a data.frame or a data.table),}

\item{y}{query data  (a data.frame or a data.table, default NULL),}

\item{on}{a character vector with column names for the ANN search,}

\item{on_blocking}{blocking variables (currently not supported),}

\item{deduplication}{whether deduplication should be performed (default TRUE),}

\item{keep_block}{whether to keep the block variable in the set,}

\item{add_xy}{whether to add x and y,}

\item{...}{arguments passed to \code{\link[=blocking]{blocking()}} function.}
}
\value{
Returns a \link{data.table} with two columns \code{.x} and \code{.y}. Columns \code{.x} and \code{.y} are row numbers from data.frames x and y respectively. Returning data.table is also of a class \code{pairs} which allows for integration with the \code{\link[reclin2:compare_pairs]{reclin2::compare_pairs()}} package.
}
\description{
Function for the integration with the \code{reclin2} package. The function is based on \code{\link[reclin2:pair_minsim]{reclin2::pair_minsim()}} and reuses some of its source code.
}
\details{
Imports
}
\examples{

# example using two datasets from reclin2

library(reclin2)

data("linkexample1", "linkexample2", package = "reclin2")

linkexample1$txt <- with(linkexample1, tolower(paste0(firstname, lastname, address, sex, postcode)))
linkexample1$txt <- gsub("\\\\s+", "", linkexample1$txt)
linkexample2$txt <- with(linkexample2, tolower(paste0(firstname, lastname, address, sex, postcode)))
linkexample2$txt <- gsub("\\\\s+", "", linkexample2$txt)

# pairing records from linkexample2 to linkexample1 based on txt column

pair_ann(x = linkexample1, y = linkexample2, on = "txt", deduplication = FALSE) |>
compare_pairs(on = "txt", comparators = list(cmp_jarowinkler())) |>
score_simple("score", on = "txt") |>
select_threshold("threshold", score = "score", threshold = 0.75) |>
link(selection = "threshold")

}
\author{
Maciej Beręsewicz
}
