#' @title Controls for approximate nearest neighbours algoritms
#'
#' @author Maciej Beręsewicz
#'
#' @description
#' Controls for ANN algorithms used in the package
#'
#' @param sparse whether sparse data should be used as an input for algorithms.
#' @param hnsw parameters for [RcppHNSW::hnsw_build()] and [RcppHNSW::hnsw_search()].
#' @param lsh parameters for [mlpack::lsh()].
#' @param annoy parameters for [RcppAnnoy] package.
#' @param kd parameters for [mlpack::knn()] function.
#'
#' @returns Returns a list with parameters
#'
#' @export
controls_ann <- function(
    sparse = FALSE,
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
        hnsw = hnsw,
        lsh = lsh,
        annoy = annoy,
        kd = kd)
}

#' @title Controls for text data
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
