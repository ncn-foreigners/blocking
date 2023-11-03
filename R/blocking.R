#' Imports
#' @importFrom text2vec itoken_parallel
#' @importFrom text2vec create_vocabulary
#' @importFrom text2vec vocab_vectorizer
#' @importFrom text2vec create_dtm
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph components
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph make_clusters
#' @importFrom igraph compare
#'
#'
#' @title Main function for blocking records given text data
#'
#' @author Maciej Beręsewicz
#'
#' @description
#' Function that creates shingles (strings with 2 characters), applies approximate nearest neighbour search using
#' [RcppHNSW], [RcppAnnoy] and [mlpack] and creates blocks using [igraph].
#'
#' @param x input text or matrix data,
#' @param y input text or matrix data (default NULL),
#' @param deduplication whether deduplication should be applied (default TRUE as y is set to NULL),
#' @param block initial blocking to reduce comparisons (currently not supported),
#' @param ann algorithm to be used for searching for ann (possible, \code{c("hnsw", "lsh", "annoy", "kd")},
#' default \code{"hnsw"}),
#' @param distance distance metric (default \code{cosine}),
#' @param ann_read reading index from file (currently not supported),
#' @param ann_save saving index to file. Two files will be created: 1) with index, 2) with column names (currently not supported),
#' @param true_blocks matrix with true blocks to calculate evaluation metrics (all metrics from [igraph::compare()] are returned).
#' @param verbose whether log should be provided (0 = none, 1 = main, 2 = ann algorithms),
#' @param seed seed for the algorithms,
#' @param n_threads number of threads used for the ann,
#' @param control_txt list of controls for text data,
#' @param control_ann list of controls for ann algorithms.
#'
#' @returns Returns a list with containing:\cr
#' \itemize{
#' \item{\code{result} -- \code{data.frame} with indices (rows) of x, y and block}
#' \item{\code{ann} -- name of the ann algorithm used,}
#' \item{\code{metrics} -- metrics, if \code{true_blocks} is provided,}
#' \item{\code{colnames} -- variable names (colnames) used for search.}
#' }
#'
#' @examples
#'
#' ## general example
#' df_example <- data.frame(txt = c("jankowalski", "kowalskijan", "kowalskimjan",
#' "kowaljan", "montypython", "pythonmonty", "cyrkmontypython", "monty"))
#'
#' result <- blocking(x = df_example$txt,
#'                    ann = "hnsw",
#'                    control_ann = controls_ann(hnsw = list(M = 5, ef_c = 10, ef_s = 10)))
#'
#' result
#'
#' ## an example with true blocks
#'
#' @export
blocking <- function(x,
                     y = NULL,
                     deduplication = TRUE,
                     block = NULL,
                     ann = c("hnsw", "lsh", "annoy", "kd"),
                     distance = c("cosine", "euclidean", "l2", "ip", "manhatan", "hamming", "angular"),
                     ann_read = NULL,
                     ann_save = NULL,
                     true_blocks = NULL,
                     verbose = c(0, 1, 2),
                     n_threads = 1,
                     seed = 2023,
                     control_txt = controls_txt(),
                     control_ann = controls_ann()) {

  ## checks
  stopifnot("Only character or matrix x is supported" = is.character(x) | is.matrix(x))

  #stopifnot("Distance for Annoy should be `euclidean, manhatan, hamming, angular`" =
  #            distance %in% c("euclidean", "manhatan", "hamming", "angular") & ann == "annoy")


  if (!is.null(true_blocks)) {
    stopifnot("`true block` should be a data.frame with columns: x, y, block" =
                !is.null(true_blocks) &
                is.data.frame(true_blocks) &
                length(colnames(true_blocks)) == 3,
              all(colnames(true_blocks) == c("x", "y", "block")))
  }

  ## defaults
  if (missing(verbose)) verbose <- 0
  if (missing(ann)) ann <- "hnsw"
  if (missing(distance)) distance <- "cosine"
  if (!is.null(y)) {
    deduplication <- FALSE
    y_default <- FALSE
  }

  k <- 1L

  if (is.null(y)) {
    y_default <- y
    y <- x
    k <- 2L
  }

  if (is.matrix(x)) {
    l_dtm <- x
    l_dtm_y <- y
  } else {

    if (verbose %in% 1:2) cat("===== creating tokens =====\n")

    ## tokens for x
    l_tokens <- text2vec::itoken_parallel(
      iterable = x,
      tokenizer = function(x) tokenizers::tokenize_character_shingles(x, n = control_txt$n_shingles),
      n_chunks = control_txt$n_chunks,
      progressbar = verbose)

    l_voc <- text2vec::create_vocabulary(l_tokens)
    l_vec <- text2vec::vocab_vectorizer(l_voc)
    l_dtm <- text2vec::create_dtm(l_tokens, l_vec)
    l_dtm <- base::as.matrix(l_dtm)

    if (is.null(y_default)) {
      l_dtm_y <- l_dtm
    } else {
      l_tokens_y <- text2vec::itoken_parallel(
        iterable = y,
        tokenizer = function(x) tokenizers::tokenize_character_shingles(x, n = control_txt$n_shingles),
        n_chunks = control_txt$n_chunks,
        progressbar = verbose)

      l_voc_y <- text2vec::create_vocabulary(l_tokens_y)
      l_vec_y <- text2vec::vocab_vectorizer(l_voc_y)
      l_dtm_y <- text2vec::create_dtm(l_tokens_y, l_vec_y)
      l_dtm_y <- base::as.matrix(l_dtm_y)
    }
  }

  colnames_xy <- intersect(colnames(l_dtm), colnames(l_dtm_y))

  if (verbose %in% 1:2) {
    cat(sprintf("===== starting search (%s, x, y: %d, %d, t: %d) =====\n",
                ann, nrow(l_dtm), nrow(l_dtm_y), length(colnames_xy)))
  }

  l_df <- switch(ann,
                 "hnsw" = method_hnsw(x = l_dtm[, colnames_xy],
                                      y = l_dtm_y[, colnames_xy],
                                      k = k,
                                      distance = distance,
                                      verbose = if (verbose == 2) TRUE else FALSE,
                                      n_threads = n_threads,
                                      control = control_ann),
                 "lsh" = method_mlpack(x = l_dtm[, colnames_xy],
                                       y = l_dtm_y[, colnames_xy],
                                       algo = "lsh",
                                       k = k,
                                       verbose = if (verbose == 2) TRUE else FALSE,
                                       seed = seed,
                                       control = control_ann),
                 "kd" = method_mlpack(x = l_dtm[, colnames_xy],
                                      y = l_dtm_y[, colnames_xy],
                                      algo = "kd",
                                      k = k,
                                      verbose = if (verbose == 2) TRUE else FALSE,
                                      seed = seed,
                                      control = control_ann),
                 "annoy" = method_annoy(x = l_dtm[, colnames_xy],
                                        y = l_dtm_y[, colnames_xy],
                                        k = k,
                                        distance  = distance,
                                        verbose = if (verbose == 2) TRUE else FALSE,
                                        seed = seed,
                                        control = control_ann))


  if (verbose %in% 1:2) cat("===== creating graph =====\n")

  ## this should be switched to data.table
  if (deduplication) {
    l_df$query_g <- paste0("q", l_df$y)
    l_df$index_g <- paste0("q", l_df$x)
  } else {
    l_df$query_g <- paste0("q", l_df$y)
    l_df$index_g <- paste0("i", l_df$x)
  }

  ## wybor kolumn powinien zalezec od tego czy jest tylko x czy x, y
  l_gr <- igraph::graph_from_data_frame(l_df[, c("query_g", "index_g")], directed = F)
  l_block <- igraph::components(l_gr, "weak")$membership

  l_df$block <- l_block[names(l_block) %in% l_df$query_g] ## to data.table

  ## if true are given
  if (!is.null(true_blocks)) {
    ## commom part
    eval_blocks <- merge(x = l_df[, c("x", "y", "block")],
                         y = true_blocks,
                         by = c("x", "y"),
                         all = F)

    eval_g1 <- igraph::graph_from_data_frame(eval_blocks[, c("x", "y")], directed = FALSE)
    eval_g2 <- igraph::graph_from_data_frame(eval_blocks[, c("x", "y")], directed = FALSE)

    eval_g1_cl <- igraph::make_clusters(eval_g1, membership = eval_blocks$block.x)
    eval_g2_cl <- igraph::make_clusters(eval_g2, membership = eval_blocks$block.y)

    eval_metrics <- base::sapply(c("vi", "nmi", "split.join", "rand", "adjusted.rand"),
                                 igraph::compare, comm1=eval_g1_cl, comm2=eval_g2_cl)

  }

  list(
    result = as.data.frame(l_df[, c("x", "y", "block")]), ## will be further changed to data.table
    method = ann,
    metrics = if (is.null(true_blocks)) NULL else eval_metrics,
    colnames = colnames_xy
   )
}
