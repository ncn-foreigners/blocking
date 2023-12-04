#' @title Controls for approximate nearest neighbours algorithms
#'
#' @author Maciej Beręsewicz
#'
#' @description
#' Controls for ANN algorithms used in the package
#'
#' @param sparse whether sparse data should be used as an input for algorithms,
#' @param nnd parameters for [rnndescent::rnnd_build()] and [rnndescent::rnnd_query()],
#' @param hnsw parameters for [RcppHNSW::hnsw_build()] and [RcppHNSW::hnsw_search()],
#' @param lsh parameters for [mlpack::lsh()],
#' @param annoy parameters for [RcppAnnoy] package,
#' @param kd parameters for [mlpack::knn()] function.
#'
#' @returns Returns a list with parameters
#'
#' @export
controls_ann <- function(
    sparse = FALSE,
    nnd = list(k_build = 30,
               use_alt_metric = TRUE,
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
               progress = "bar",
               obs = "R"),
    hnsw = list(M = 25,
                ef_c = 200,
                ef_s = 200,
                grain_size = 1,
                byrow = TRUE),
    lsh = list(bucket_size = 500,
               hash_width = 10,
               num_probes = 0,
               projections = 10,
               tables = 30),
    annoy = list(n_trees = 250,
                 build_on_disk = FALSE),
    kd = list(algorithm = "dual_tree",
              epsilon = 0,
              leaf_size = 20,
              random_basis = FALSE,
              rho = 0.7,
              tau = 0,
              tree_type = "kd")
    ) {

   list(sparse = sparse,
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
#' Controls for text data used in the \code{blocking} functions
#'
#' @param n_shingles length of shingles (default 2L), passed to [tokenizers::tokenize_character_shingles],
#' @param n_chunks passed to (default 10L) [tokenizers::tokenize_character_shingles].
#'
#' @returns Returns a list with parameters.
#'
#' @export
controls_txt <- function(
    n_shingles = 2L,
    n_chunks = 10L
    ) {

  list(n_shingles = n_shingles,
       n_chunks = n_chunks)
}
