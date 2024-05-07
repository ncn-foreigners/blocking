#' @title Controls for approximate nearest neighbours algorithms
#'
#' @author Maciej Beręsewicz
#'
#' @description
#' Controls for ANN algorithms used in the package
#'
#' @param sparse whether sparse data should be used as an input for algorithms,
#' @param k_search number of neighbours to search,
#' @param nnd list of parameters for [rnndescent::rnnd_build()] and [rnndescent::rnnd_query()],
#' @param hnsw list of parameters for [RcppHNSW::hnsw_build()] and [RcppHNSW::hnsw_search()],
#' @param lsh list of parameters for [mlpack::lsh()] function,
#' @param kd list of kd parameters for [mlpack::knn()] function,
#' @param annoy list of parameters for [RcppAnnoy] package.
#'
#' @returns Returns a list with parameters
#'
#' @export
controls_ann <- function(
    sparse = FALSE,
    k_search = 30,
    nnd = list(k_build = 30,
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
    kd = list(algorithm = "dual_tree",
              epsilon = 0,
              leaf_size = 20,
              random_basis = FALSE,
              rho = 0.7,
              tau = 0,
              tree_type = "kd"),
    annoy = list(n_trees = 250,
                 build_on_disk = FALSE)
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
#' Controls for text data used in the \code{blocking} functions, passed to [tokenizers::tokenize_character_shingles].
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
