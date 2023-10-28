#' @title Controls for approximate nearest neighbours algoritms
#'
#' @author Maciej Beręsewicz
#'
#' @description
#' Controls for ANN algorithms used in the package
#'
#' @param k k nearest neighbours
#' @param distance distance metric for \code{RcppHNSW}
#' @param seed seed for the algorithms
#' @param hnsw parameters for \code{RcppHNSW}
#' @param lsh parameters for \code{mlpack::lsh} (not yet supported)
#' @param annoy parameters for \code{RcppAnnoy} (not yet supported)
#'
#' @returns Returns a list with parameters
#'
#' @export
controls_ann <- function(
    k = 2L,
    distance = c("cosine", "euclidean", "l2", "ip"), ## manhattan, angular, hamming
    seed = 2023,
    hnsw = list(M = 25, ef_c = 200, ef_s = 200, grain_size = 1, byrow = TRUE),
    lsh = list(),
    annoy = list()
    ) {

   list(k = k,
        distance = distance,
        seed = seed,
        hnsw = hnsw,
        lsh = lsh,
        annoy = annoy)
}

#' @title Controls for text data
#'
#' @author Maciej Beręsewicz
#'
#' @description
#' Controls for text data used in the \code{blocking} functions
#'
#' @param n_shingles length of shingles (default 2), passed to \code{tokenizers::tokenize_character_shingles}
#' @param n_chunks passed to \code{tokenizers::tokenize_character_shingles}
#'
#' @returns Returns a list with parameters
#'
#' @export
controls_txt <- function(
    n_shingles = 2L,
    n_chunks = 10L
    ) {

  list(n_shingles = n_shingles,
       n_chunks = n_chunks)
}
