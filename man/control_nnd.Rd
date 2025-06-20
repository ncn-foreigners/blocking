% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/controls.R
\name{control_nnd}
\alias{control_nnd}
\title{Controls for the NND algorithm}
\usage{
control_nnd(
  k_build = 30,
  use_alt_metric = FALSE,
  init = "tree",
  n_trees = NULL,
  leaf_size = NULL,
  max_tree_depth = 200,
  margin = "auto",
  n_iters = NULL,
  delta = 0.001,
  max_candidates = NULL,
  low_memory = TRUE,
  n_search_trees = 1,
  pruning_degree_multiplier = 1.5,
  diversify_prob = 1,
  weight_by_degree = FALSE,
  prune_reverse = FALSE,
  progress = "bar",
  obs = "R",
  max_search_fraction = 1,
  epsilon = 0.1,
  ...
)
}
\arguments{
\item{k_build}{Number of nearest neighbors to build the index for.}

\item{use_alt_metric}{If \code{TRUE}, use faster metrics that maintain the ordering of distances internally (e.g. squared Euclidean distances if using \code{metric = "euclidean"}),
then apply a correction at the end.}

\item{init}{Name of the initialization strategy or initial data neighbor graph to optimize.}

\item{n_trees}{The number of trees to use in the RP forest.
Only used if \code{init = "tree"}.}

\item{leaf_size}{The maximum number of items that can appear in a leaf.
Only used if \code{init = "tree"}.}

\item{max_tree_depth}{The maximum depth of the tree to build (default = 200).
Only used if \code{init = "tree"}.}

\item{margin}{A character string specifying the method used to assign points to one side of the hyperplane or the other.}

\item{n_iters}{Number of iterations of nearest neighbor descent to carry out.}

\item{delta}{The minimum relative change in the neighbor graph allowed before early stopping. Should be a value between 0 and 1. The smaller the value, the smaller the amount of progress between iterations is allowed.}

\item{max_candidates}{Maximum number of candidate neighbors to try for each item in each iteration.}

\item{low_memory}{If \code{TRUE}, use a lower memory, but more computationally expensive approach to index construction. If set to \code{FALSE}, you should see a noticeable speed improvement, especially when using a smaller number of threads, so this is worth trying if you have the memory to spare.}

\item{n_search_trees}{The number of trees to keep in the search forest as part of index preparation. The default is 1.}

\item{pruning_degree_multiplier}{How strongly to truncate the final neighbor list for each item.}

\item{diversify_prob}{The degree of diversification of the search graph by removing unnecessary edges through occlusion pruning.}

\item{weight_by_degree}{If \code{TRUE}, then candidates for the local join are weighted according to their in-degree,
so that if there are more than \code{max_candidates} in a candidate list, candidates with a smaller degree are favored for retention.}

\item{prune_reverse}{If \code{TRUE}, prune the reverse neighbors of each item before the reverse graph diversification step using \code{pruning_degree_multiplier}.}

\item{progress}{Determines the type of progress information logged during the nearest neighbor descent stage.}

\item{obs}{set to \code{C} to indicate that the input data orientation stores each observation as a column.
The default \code{R} means that observations are stored in each row.}

\item{max_search_fraction}{Maximum fraction of the reference data to search.}

\item{epsilon}{Controls trade-off between accuracy and search cost.}

\item{...}{Additional arguments.}
}
\value{
Returns a list with parameters.
}
\description{
Controls for NND algorithm used in the package (see \link[rnndescent]{rnnd_build} and \link[rnndescent]{rnnd_query} for details).
}
