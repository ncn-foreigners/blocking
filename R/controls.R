#' @title Controls for approximate nearest neighbours algoritms
#'
#' @author Maciej Beręsewicz
#'
#' @description
#' Controls for ANN algorithms used in the package
#'
#' @param k k nearest neighbours
#' @param distance distance metric for \code{RcppHNSW}
#' @param hsnw_M see \code{RcppHNSW}
#' @param hnsw_ef see \code{RcppHNSW}
#'
#' @returns Returns a list with parameters
#'
#' @export
controls_ann <- function(
    k = 2L,
    distance = "cosine",
    hsnw_M = 25,
    hnsw_ef = 200) {

   list(distance = distance,
        hsnw_M = hsnw_M,
        hnsw_ef = hnsw_ef,
        k = k)
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
