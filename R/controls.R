#' @title Controls for HNSW
#'
#' @description
#' Controls for HNSW algorithm used in the package (see [RcppHNSW::hnsw_build()] and [RcppHNSW::hnsw_search()] for details).
#'
#' @param M Controls the number of bi-directional links created for each element during index construction.
#' @param ef_c Size of the dynamic list used during construction.
#' @param ef_s Size of the dynamic list used during search.
#' @param grain_size Minimum amount of work to do (rows in the dataset to add) per thread.
#' @param byrow If \code{TRUE} (the default), this indicates that the items in the dataset to be indexed are stored in each row.
#' Otherwise, the items are stored in the columns of the dataset.
#' @param ... Additional arguments.
#'
#' @returns Returns a list with parameters.
#'
#' @export
control_hnsw <- function(M = 25,
                          ef_c = 200,
                          ef_s = 200,
                          grain_size = 1,
                          byrow = TRUE,
                          ...){
  append(list(M = M,
              ef_c = ef_c,
              ef_s = ef_s,
              grain_size = grain_size,
              byrow = byrow),
         list(...))
}

#' @title Controls for NND
#'
#' @description
#' Controls for NND algorithm used in the package (see \link[rnndescent]{rnnd_build} and \link[rnndescent]{rnnd_query} for details).
#'
#' @param k_build Number of nearest neighbors to build the index for.
#' @param use_alt_metric If \code{TRUE}, use faster metrics that maintain the ordering of distances internally (e.g. squared Euclidean distances if using \code{metric = "euclidean"}),
#' then apply a correction at the end.
#' @param init Name of the initialization strategy or initial data neighbor graph to optimize.
#' @param n_trees The number of trees to use in the RP forest.
#' Only used if \code{init = "tree"}.
#' @param leaf_size The maximum number of items that can appear in a leaf.
#' Only used if \code{init = "tree"}.
#' @param max_tree_depth The maximum depth of the tree to build (default = 200).
#' Only used if \code{init = "tree"}.
#' @param margin A character string specifying the method used to assign points to one side of the hyperplane or the other.
#' @param n_iters Number of iterations of nearest neighbor descent to carry out.
#' @param delta The minimum relative change in the neighbor graph allowed before early stopping. Should be a value between 0 and 1. The smaller the value, the smaller the amount of progress between iterations is allowed.
#' @param max_candidates 	Maximum number of candidate neighbors to try for each item in each iteration.
#' @param low_memory If \code{TRUE}, use a lower memory, but more computationally expensive approach to index construction. If set to \code{FALSE}, you should see a noticeable speed improvement, especially when using a smaller number of threads, so this is worth trying if you have the memory to spare.
#' @param n_search_trees The number of trees to keep in the search forest as part of index preparation. The default is 1.
#' @param pruning_degree_multiplier How strongly to truncate the final neighbor list for each item.
#' @param diversify_prob The degree of diversification of the search graph by removing unnecessary edges through occlusion pruning.
#' @param weight_by_degree If \code{TRUE}, then candidates for the local join are weighted according to their in-degree,
#' so that if there are more than \code{max_candidates} in a candidate list, candidates with a smaller degree are favored for retention.
#' @param prune_reverse If \code{TRUE}, prune the reverse neighbors of each item before the reverse graph diversification step using \code{pruning_degree_multiplier}.
#' @param progress Determines the type of progress information logged during the nearest neighbor descent stage.
#' @param obs set to \code{C} to indicate that the input data orientation stores each observation as a column.
#' The default \code{R} means that observations are stored in each row.
#' @param max_search_fraction Maximum fraction of the reference data to search.
#' @param epsilon Controls trade-off between accuracy and search cost.
#' @param ... Additional arguments.
#'
#' @returns Returns a list with parameters.
#'
#' @export
control_nnd <- function(k_build = 30,
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
                         ...){
  append(list(k_build = k_build,
              use_alt_metric = use_alt_metric,
              init = init,
              n_trees = n_trees,
              leaf_size = leaf_size,
              max_tree_depth = max_tree_depth,
              margin = margin,
              n_iters = n_iters,
              delta = delta,
              max_candidates = max_candidates,
              low_memory = low_memory,
              n_search_trees = n_search_trees,
              pruning_degree_multiplier = pruning_degree_multiplier,
              diversify_prob = diversify_prob,
              weight_by_degree = weight_by_degree,
              prune_reverse = prune_reverse,
              progress = progress,
              obs = obs,
              max_search_fraction = max_search_fraction,
              epsilon = epsilon),
         list(...))
}

#' @title Controls for LSH
#'
#' @description
#' Controls for LSH algorithm used in the package (see \link[mlpack]{lsh} for details).
#'
#' @param bucket_size The size of a bucket in the second level hash.
#' @param hash_width The hash width for the first-level hashing in the LSH preprocessing.
#' @param num_probes Number of additional probes for multiprobe LSH.
#' @param projections The number of hash functions for each table.
#' @param tables The number of hash tables to be used.
#' @param ... Additional arguments.
#'
#' @returns Returns a list with parameters.
#'
#' @export
control_lsh <- function(bucket_size = 10,
                        hash_width = 6,
                        num_probes = 5,
                        projections = 10,
                        tables = 30,
                        ...){
  append(list(bucket_size = bucket_size,
              hash_width = hash_width,
              num_probes = num_probes,
              projections = projections,
              tables = tables),
         list(...))
}

#' @title Controls for Annoy
#'
#' @description
#' Controls for Annoy algorithm used in the package (see \link[RcppAnnoy]{RcppAnnoy} for details).
#'
#' @param n_trees An integer specifying the number of trees to build in the Annoy index.
#' @param build_on_disk A logical value indicating whether to build the Annoy index on disk instead of in memory.
#' @param ... Additional arguments.
#'
#' @returns Returns a list with parameters.
#'
#' @export
control_annoy <- function(n_trees = 250,
                           build_on_disk = FALSE,
                           ...){
  append(list(n_trees = n_trees,
              build_on_disk = build_on_disk),
         list(...))
}

#' @title Controls for KD
#'
#' @description
#' Controls for KD algorithm used in the package (see \link[mlpack]{knn} for details).
#'
#' @param algorithm Type of neighbor search: \code{'naive'}, \code{'single_tree'}, \code{'dual_tree'}, \code{'greedy'}.
#' @param epsilon If specified, will do approximate nearest neighbor search with given relative error.
#' @param leaf_size Leaf size for tree building
#' (used for kd-trees, vp trees, random projection trees, UB trees, R trees, R* trees, X trees, Hilbert R trees, R+ trees, R++ trees, spill trees, and octrees).
#' @param random_basis Before tree-building, project the data onto a random orthogonal basis.
#' @param rho Balance threshold (only valid for spill trees).
#' @param tau Overlapping size (only valid for spill trees).
#' @param tree_type Type of tree to use: \code{'kd'}, \code{'vp'}, \code{'rp'}, \code{'max-rp'}, \code{'ub'}, \code{'cover'}, \code{'r'}, \code{'r-star'},
#' \code{'x'}, \code{'ball'}, \code{'hilbert-r'}, \code{'r-plus'}, \code{'r-plus-plus'}, \code{'spill'}, \code{'oct'}.
#' @param ... Additional arguments.
#'
#' @returns Returns a list with parameters.
#'
#' @export
control_kd <- function(algorithm = "dual_tree",
                        epsilon = 0,
                        leaf_size = 20,
                        random_basis = FALSE,
                        rho = 0.7,
                        tau = 0,
                        tree_type = "kd",
                        ...){
  append(list(algorithm = algorithm,
              epsilon = epsilon,
              leaf_size = leaf_size,
              random_basis = random_basis,
              rho = rho,
              tau = tau,
              tree_type = tree_type),
         list(...))
}

#' @title Controls for approximate nearest neighbours algorithms
#'
#' @author Maciej Beręsewicz
#'
#' @description
#' Controls for ANN algorithms used in the package.
#'
#' @param sparse whether sparse data should be used as an input for algorithms,
#' @param k_search number of neighbours to search,
#' @param nnd parameters for \link[rnndescent]{rnnd_build} and \link[rnndescent]{rnnd_query} (should be inside [control_nnd] function),
#' @param hnsw parameters for \link[RcppHNSW]{hnsw_build} and \link[RcppHNSW]{hnsw_search} (should be inside [control_hnsw] function),
#' @param lsh parameters for \link[mlpack]{lsh} function (should be inside [control_lsh] function),
#' @param kd kd parameters for \link[mlpack]{knn} function (should be inside [control_kd] function),
#' @param annoy parameters for \link[RcppAnnoy]{RcppAnnoy} package (should be inside [control_annoy] function).
#'
#' @returns Returns a list with parameters.
#'
#' @export
controls_ann <- function(
    sparse = FALSE,
    k_search = 30,
    nnd = control_nnd(),
    hnsw = control_hnsw(),
    lsh = control_lsh(),
    kd = control_kd(),
    annoy = control_annoy()
    ) {

   list(sparse = sparse,
        k_search = k_search,
        nnd = nnd,
        hnsw = hnsw,
        lsh = lsh,
        annoy = annoy,
        kd = kd)
}

#' @title Controls for processing text data
#'
#' @author Maciej Beręsewicz
#'
#' @description
#' Controls for text data used in the \code{blocking} function (if \code{representation = shingles}), passed to \link[tokenizers]{tokenize_character_shingles}.
#'
#' @param n_shingles length of shingles (default `2L`),
#' @param n_chunks passed to (default `10L`),
#' @param lowercase should the characters be made lowercase? (default `TRUE`),
#' @param strip_non_alphanum should punctuation and white space be stripped? (default `TRUE`).
#'
#' @returns Returns a list with parameters.
#'
#' @export
controls_txt <- function(
    n_shingles = 2L,
    n_chunks = 10L,
    lowercase = TRUE,
    strip_non_alphanum = TRUE
    ) {

  list(n_shingles = n_shingles,
       n_chunks = n_chunks,
       lowercase = lowercase,
       strip_non_alphanum = strip_non_alphanum)
}
