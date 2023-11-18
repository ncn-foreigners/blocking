#' Imports
#' @importFrom text2vec itoken
#' @importFrom text2vec itoken_parallel
#' @importFrom text2vec create_vocabulary
#' @importFrom text2vec vocab_vectorizer
#' @importFrom text2vec create_dtm
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph components
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph make_clusters
#' @importFrom igraph compare
#' @importFrom utils combn
#'
#'
#' @title Main function for blocking records given text data
#'
#' @author Maciej Beręsewicz
#'
#' @description
#' Function that creates shingles (strings with 2 characters), applies approximate nearest neighbour search using
#' RcppHNSW, [RcppAnnoy] and [mlpack] and creates blocks using [igraph].
#'
#' @param x reference data (character vector or a matrix),
#' @param y query data (types the same), if not provided NULL by default,
#' @param deduplication whether deduplication should be applied (default TRUE as y is set to NULL),
#' @param on variables for ann search (currently not supported),
#' @param on_blocking variables for blocking (currently not supported),
#' @param ann algorithm to be used for searching for ann (possible, \code{c("hnsw", "lsh", "annoy", "kd")}, default \code{"hnsw"}),
#' @param distance distance metric (default \code{cosine}),
#' @param ann_write writing an index to file. Two files will be created: 1) an index, 2) and txt file with column names (currently not supported),
#' @param ann_colnames testing
#' @param true_blocks matrix with true blocks to calculate evaluation metrics (all metrics from [igraph::compare()] are returned).
#' @param verbose whether log should be provided (0 = none, 1 = main, 2 = ann algorithms),
#' @param graph whether a graph should be returned,
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
#' ## an example using RcppHNSW
#' df_example <- data.frame(txt = c("jankowalski", "kowalskijan", "kowalskimjan",
#' "kowaljan", "montypython", "pythonmonty", "cyrkmontypython", "monty"))
#'
#' result <- blocking(x = df_example$txt,
#'                    ann = "hnsw",
#'                    control_ann = controls_ann(hnsw = list(M = 5, ef_c = 10, ef_s = 10)))
#'
#' result
#'
#' ## an example using RcppAnnoy
#'
#' result_annoy <- blocking(x = df_example$txt,
#'                          ann = "annoy",
#'                          distance = "angular")
#'
#' result_annoy
#'
#' ## an example using mlpack::lsh
#'
#' result_lsh <- blocking(x = df_example$txt,
#'                        ann = "lsh")
#'
#' result_lsh
#' @export
blocking <- function(x,
                     y = NULL,
                     deduplication = TRUE,
                     on = NULL,
                     on_blocking = NULL,
                     ann = c("hnsw", "annoy", "lsh", "kd", "nnd"),
                     distance = c("cosine", "euclidean", "l2", "ip", "manhatan", "hamming", "angular"),
                     ann_write = NULL,
                     ann_colnames = NULL,
                     true_blocks = NULL,
                     verbose = c(0, 1, 2),
                     graph = FALSE,
                     seed = 2023,
                     n_threads = 1,
                     control_txt = controls_txt(),
                     control_ann = controls_ann()) {

  ## defaults
  if (missing(verbose)) verbose <- 0
  if (missing(ann)) ann <- "hnsw"
  if (missing(distance)) distance <- switch(ann,
                                            "hnsw"="cosine",
                                            "annoy"="angular",
                                            "lsh"=NULL,
                                            "kd"=NULL)

  stopifnot("Only character or matrix x is supported" = is.character(x) | is.matrix(x))
  if (!is.null(ann_write)) {
    stopifnot("Path provided in the `ann_write` is incorrect" = file.exists(ann_write) )
  }

  if (ann == "hnsw") {
    stopifnot("Distance for HNSW should be `l2, euclidean, cosine, ip`" =
                distance %in% c("l2", "euclidean", "cosine", "ip"))
  }

  if (ann == "annoy") {
    stopifnot("Distance for Annoy should be `euclidean, manhatan, hamming, angular`" =
                distance %in% c("euclidean", "manhatan", "hamming", "angular"))
  }


  if (!is.null(true_blocks)) {
    if (is.data.frame(true_blocks)) {
      stopifnot("`true blocks` should be a data.frame with columns: x, y, block" =
                  !is.null(true_blocks) &
                  is.data.frame(true_blocks) &
                  length(colnames(true_blocks)) == 3,
                all(colnames(true_blocks) == c("x", "y", "block")))
    }
    if (is.vector(true_blocks)) {
      stopifnot("`true blocks` should be a vector with elements equal to nrow(x)" =
                  NROW(true_blocks) == NROW(x))
    }
  }


  if (!is.null(y)) {
    deduplication <- FALSE
    y_default <- FALSE
    k <- 1L
  } else {
    y_default <- y
    y <- x
    k <- 2L
  }

  ## add verification if x and y is a sparse matrix
  if (is.matrix(x)) {
    l_dtm <- x
    l_dtm_y <- y
  } else {

    if (verbose %in% 1:2) cat("===== creating tokens =====\n")

    ## tokens for x
    if (.Platform$OS.type == "unix") {
      l_tokens <- text2vec::itoken_parallel(
        iterable = x,
        tokenizer = function(x) tokenizers::tokenize_character_shingles(x, n = control_txt$n_shingles),
        n_chunks = control_txt$n_chunks,
        progressbar = verbose)
    } else {
      l_tokens <- text2vec::itoken(
        iterable = x,
        tokenizer = function(x) tokenizers::tokenize_character_shingles(x, n = control_txt$n_shingles),
        n_chunks = control_txt$n_chunks,
        progressbar = verbose)
    }


    l_voc <- text2vec::create_vocabulary(l_tokens)
    l_vec <- text2vec::vocab_vectorizer(l_voc)
    l_dtm <- text2vec::create_dtm(l_tokens, l_vec)


    if (is.null(y_default)) {
      l_dtm_y <- l_dtm
    } else {
      if (.Platform$OS.type == "unix") {
      l_tokens_y <- text2vec::itoken_parallel(
        iterable = y,
        tokenizer = function(x) tokenizers::tokenize_character_shingles(x, n = control_txt$n_shingles),
        n_chunks = control_txt$n_chunks,
        progressbar = verbose)
      } else {
        l_tokens_y <- text2vec::itoken(
          iterable = y,
          tokenizer = function(x) tokenizers::tokenize_character_shingles(x, n = control_txt$n_shingles),
          n_chunks = control_txt$n_chunks,
          progressbar = verbose)
      }
      l_voc_y <- text2vec::create_vocabulary(l_tokens_y)
      l_vec_y <- text2vec::vocab_vectorizer(l_voc_y)
      l_dtm_y <- text2vec::create_dtm(l_tokens_y, l_vec_y)

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
                                      path = ann_write,
                                      control = control_ann),
                 "lsh" = method_mlpack(x = l_dtm[, colnames_xy],
                                       y = l_dtm_y[, colnames_xy],
                                       algo = "lsh",
                                       k = k,
                                       verbose = if (verbose == 2) TRUE else FALSE,
                                       seed = seed,
                                       path = ann_write,
                                       control = control_ann),
                 "kd" = method_mlpack(x = l_dtm[, colnames_xy],
                                      y = l_dtm_y[, colnames_xy],
                                      algo = "kd",
                                      k = k,
                                      verbose = if (verbose == 2) TRUE else FALSE,
                                      seed = seed,
                                      path = ann_write,
                                      control = control_ann),
                 "annoy" = method_annoy(x = l_dtm[, colnames_xy],
                                        y = l_dtm_y[, colnames_xy],
                                        k = k,
                                        distance  = distance,
                                        verbose = if (verbose == 2) TRUE else FALSE,
                                        seed = seed,
                                        path = ann_write,
                                        control = control_ann))


  if (verbose %in% 1:2) cat("===== creating graph =====\n")

  ## this should be switched to data.table
  if (deduplication) {
    l_df[, `:=`("query_g", paste0("q", y))]
    l_df[, `:=`("index_g", paste0("q", x))]
  } else {
    l_df[, `:=`("query_g", paste0("q", y))]
    l_df[, `:=`("index_g", paste0("i", x))]
  }

  l_gr <- igraph::graph_from_data_frame(l_df[, c("query_g", "index_g")], directed = F)
  l_block <- igraph::components(l_gr, "weak")$membership

  l_df[, `:=`(block, l_block[names(l_block) %in% l_df$query_g])]

  ## if true are given
  if (!is.null(true_blocks)) {

    ## if true_block is a vector
    if (is.vector(true_blocks)) {
      true_blocks <- data.table(y=1:NROW(l_dtm_y), block = true_blocks)
      eval_blocks <- merge(x = l_df[, c("x", "y", "block")],
                         y = true_blocks,
                         by = "y",
                         all = F)
    } else {
      eval_blocks <- merge(x = l_df[, c("x", "y", "block")],
                           y = true_blocks,
                           by = c("x", "y"),
                           all = F)
    }
    ## Graph metrics


    eval_g1 <- igraph::graph_from_data_frame(eval_blocks[, c("x", "y")], directed = FALSE)
    eval_g2 <- igraph::graph_from_data_frame(eval_blocks[, c("x", "y")], directed = FALSE)

    eval_g1_cl <- igraph::make_clusters(eval_g1, membership = eval_blocks$block.x)
    eval_g2_cl <- igraph::make_clusters(eval_g2, membership = eval_blocks$block.y)

    eval_metrics <- base::sapply(c("vi", "nmi", "split.join", "rand", "adjusted.rand"),
                                 igraph::compare, comm1=eval_g1_cl, comm2=eval_g2_cl)

    ## standard metrics based on klsh::confusion.from.blocking
    block_ids <- eval_g1_cl$membership
    true_ids <- eval_g2_cl$membership
    candidate_pairs <- utils::combn(length(block_ids), 2)
    same_block <- block_ids[candidate_pairs[1, ]] == block_ids[candidate_pairs[2, ]]
    same_truth <- true_ids[candidate_pairs[1, ]] == true_ids[candidate_pairs[2, ]]
    confusion <- table(same_block, same_truth)

    fp <- confusion[2, 1]
    fn <- confusion[1, 2]
    tp <- confusion[2, 2]
    tn <- confusion[1, 1]
    recall <- tp/(fn + tp)

    eval_metrics2 <- c(recall = tp/(fn + tp), precision = tp/(tp + fp),
                       fpr = fp/(fp + tn), fnr = fn/(fn + tp),
                       accuracy = (tp + tn)/(tp + tn + fn + fp),
                       specificity = tn/(tn + fp))
    eval_metrics <- c(eval_metrics, eval_metrics2)
  }

  setorderv(l_df, c("x", "y", "block"))

  structure(
    list(
      result = l_df[, c("x", "y", "block", "dist")],
      method = ann,
      metrics = if (is.null(true_blocks)) NULL else eval_metrics,
      colnames = colnames_xy,
      graph = if (graph) {
        igraph::graph_from_data_frame(l_df[, c("x", "y")], directed = F)
        } else NULL
   ),
   class = "blocking"
  )
}
