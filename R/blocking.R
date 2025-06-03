#'
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
#' @importFrom RcppAlgos comboGeneral
#' @importFrom stats dist
#' @importFrom readr read_table
#' @importFrom utils download.file
#' @importFrom utils unzip
#'
#'
#' @title Block records based on text data.
#'
#' @author Maciej Beręsewicz, Adam Struzik
#'
#' @description
#' Function creates shingles (strings with 2 characters, default) or vectors using a given model (e.g., GloVe),
#' applies approximate nearest neighbour (ANN) algorithms via the \link[rnndescent]{rnndescent}, \link[RcppHNSW]{RcppHNSW}, \link[RcppAnnoy]{RcppAnnoy} and \link[mlpack]{mlpack} packages,
#' and creates blocks using graphs via \link[igraph]{igraph}.
#'
#' @param x reference data (a character vector or a matrix),
#' @param y query data (a character vector or a matrix), if not provided NULL by default and thus deduplication is performed,
#' @param representation method of representing input data (possible \code{c("shingles", "vectors")}; default \code{"shingles"}),
#' @param model a matrix containing word embeddings (e.g., GloVe), required only when \code{representation = "vectors"},
#' @param deduplication whether deduplication should be applied (default TRUE as y is set to NULL),
#' @param on variables for ANN search (currently not supported),
#' @param on_blocking variables for blocking records before ANN search (currently not supported),
#' @param ann algorithm to be used for searching for ann (possible, \code{c("nnd", "hnsw", "annoy", "lsh", "kd")}, default \code{"nnd"} which corresponds to nearest neighbour descent method),
#' @param distance distance metric (default \code{cosine}, more options are possible see details),
#' @param ann_write writing an index to file. Two files will be created: 1) an index, 2) and text file with column names,
#' @param ann_colnames file with column names if \code{x} or \code{y} are indices saved on the disk (currently not supported),
#' @param true_blocks matrix with true blocks to calculate evaluation metrics (standard metrics based on confusion matrix as well as all metrics from \link[igraph]{compare} are returned).
#' @param verbose whether log should be provided (0 = none, 1 = main, 2 = ANN algorithm verbose used),
#' @param graph whether a graph should be returned (default FALSE),
#' @param seed seed for the algorithms (for reproducibility),
#' @param n_threads number of threads used for the ANN algorithms and adding data for index and query,
#' @param control_txt list of controls for text data (passed only to \link[text2vec]{itoken_parallel} or \link[text2vec]{itoken}), used only when \code{representation = "shingles"},
#' @param control_ann list of controls for the ANN algorithms.
#'
#' @returns Returns a list containing:\cr
#' \itemize{
#' \item{\code{result} -- \code{data.table} with indices (rows) of x, y, block and distance between points}
#' \item{\code{method} -- name of the ANN algorithm used,}
#' \item{\code{deduplication} -- information whether deduplication was applied,}
#' \item{\code{representation} -- information whether shingles or vectors were used,}
#' \item{\code{metrics} -- metrics for quality assessment, if \code{true_blocks} is provided,}
#' \item{\code{confusion} -- confusion matrix, if \code{true_blocks} is provided,}
#' \item{\code{colnames} -- variable names (colnames) used for search,}
#' \item{\code{graph} -- \code{igraph} class object.}
#' }
#'
#' @examples
#' ## an example using RcppHNSW
#'
#' df_example <- data.frame(txt = c("jankowalski", "kowalskijan", "kowalskimjan",
#' "kowaljan", "montypython", "pythonmonty", "cyrkmontypython", "monty"))
#'
#' result <- blocking(x = df_example$txt,
#'                    ann = "hnsw",
#'                    control_ann = controls_ann(hnsw = control_hnsw(M = 5, ef_c = 10, ef_s = 10)))
#'
#' result
#'
#' ## an example using mlpack::lsh
#'
#' result_lsh <- blocking(x = df_example$txt,
#'                        ann = "lsh")
#'
#' result_lsh
#'
#' ## an example using GloVe and RcppAnnoy
#' \dontrun{
#' options(timeout = 500)
#' utils::download.file("https://nlp.stanford.edu/data/glove.6B.zip", destfile = "glove.6B.zip")
#' utils::unzip("glove.6B.zip")
#'
#' glove_6B_50d <- readr::read_table("glove.6B.50d.txt",
#'                                   col_names = FALSE,
#'                                   show_col_types = FALSE)
#' data.table::setDT(glove_6B_50d)
#'
#' glove_vectors <- glove_6B_50d[,-1]
#' glove_vectors <- as.matrix(glove_vectors)
#' rownames(glove_vectors) <- glove_6B_50d$X1
#'
#' ## spaces between words are required
#' df_example_spaces <- data.frame(txt = c("jan kowalski", "kowalski jan", "kowalskim jan",
#' "kowal jan", "monty python", "python monty", "cyrk monty python", "monty"))
#'
#' result_annoy <- blocking(x = df_example_spaces$txt,
#'                          ann = "annoy",
#'                          representation = "vectors",
#'                          model = glove_vectors)
#'
#' result_annoy
#' }
#'
#' ## an example with the NN descent algorithm and true blocks
#'
#' data(census)
#' data(cis)
#'
#' set.seed(2024)
#' census <- census[sample(nrow(census), floor(nrow(census) / 2)), ]
#' set.seed(2024)
#' cis <- cis[sample(nrow(cis), floor(nrow(cis) / 2)), ]
#'
#' census[, txt:=paste0(pername1, pername2, sex,
#'        dob_day, dob_mon, dob_year, enumcap, enumpc)]
#' cis[, txt:=paste0(pername1, pername2, sex,
#'     dob_day, dob_mon, dob_year, enumcap, enumpc)]
#'
#' matches <- merge(x = census[, .(x=1:.N, person_id)],
#'                  y = cis[, .(y = 1:.N, person_id)],
#'                  by = "person_id")
#' matches[, block:=1:.N]
#'
#' result_true_blocks <- blocking(x = census$txt, y = cis$txt, verbose = 1,
#'                                true_blocks = matches[, .(x, y, block)],
#'                                seed = 2024)
#'
#' result_true_blocks
#'
#' @export
blocking <- function(x,
                     y = NULL,
                     representation = c("shingles", "vectors"),
                     model,
                     deduplication = TRUE,
                     on = NULL,
                     on_blocking = NULL,
                     ann = c("nnd", "hnsw", "annoy", "lsh", "kd"),
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
  if (missing(representation)) representation <- "shingles"
  if (missing(verbose)) verbose <- 0
  if (missing(ann)) ann <- "nnd"
  if (missing(distance)) distance <- switch(ann,
                                            "nnd" = "cosine",
                                            "hnsw" = "cosine",
                                            "annoy" = "angular",
                                            "lsh" = NULL,
                                            "kd" = NULL)

  stopifnot("Only character, dense or sparse (dgCMatrix) matrix x is supported" =
              is.character(x) | is.matrix(x) | inherits(x, "Matrix"))


  if (!is.null(ann_write)) {
    stopifnot("Path provided in the `ann_write` is incorrect" = file.exists(ann_write) )
  }

  if (ann == "nnd") {
    stopifnot("Distance for NND should be `euclidean, cosine, manhatan, hamming`" =
                distance %in% c("euclidean", "cosine","manhatan", "hamming"))
  }

  if ((ann == "nnd") && (distance == "manhatan")) {
    distance <- "manhattan"
  }

  if (ann == "hnsw") {
    stopifnot("Distance for HNSW should be `l2, euclidean, cosine, ip`" =
                distance %in% c("l2", "euclidean", "cosine", "ip"))
  }

  if (ann == "annoy") {
    stopifnot("Distance for Annoy should be `euclidean, manhatan, hamming, angular`" =
                distance %in% c("euclidean", "manhatan", "hamming", "angular"))
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


  if (!is.null(true_blocks)) {

    stopifnot("`true_blocks` should be a data.frame" = is.data.frame(true_blocks))

    if (deduplication == FALSE) {
      stopifnot("`true blocks` should be a data.frame with columns: x, y, block" =
                  length(colnames(true_blocks)) == 3,
                all(colnames(true_blocks) == c("x", "y", "block")))
    }
    if (deduplication) {
      stopifnot("`true blocks` should be a data.frame with columns: x, block" =
                  length(colnames(true_blocks)) == 2,
                all(colnames(true_blocks) == c("x", "block")))
    }
  }


  ## add verification if x and y is a sparse matrix
  if (is.matrix(x) | inherits(x, "Matrix")) {
    x_dtm <- x
    y_dtm <- y
  } else {

    if (verbose %in% 1:2) cat("===== creating tokens =====\n")

    ## vectors

    if (representation == "vectors"){
      x_embeddings <- sentence_to_vector(x, model)

      if (is.null(y_default)) {
        y_embeddings <- x_embeddings
      } else {
        y_embeddings <- sentence_to_vector(y, model)
      }
    } else{

      ## shingles

      ## tokens for x
      if (.Platform$OS.type == "unix") {
        x_tokens <- text2vec::itoken_parallel(
          iterable = x,
          tokenizer = function(x) tokenizers::tokenize_character_shingles(x,
                                                                          n = control_txt$n_shingles,
                                                                          lowercase = control_txt$lowercase,
                                                                          strip_non_alphanum = control_txt$strip_non_alphanum),
          n_chunks = control_txt$n_chunks,
          progressbar = verbose)
      } else {
        x_tokens <- text2vec::itoken(
          iterable = x,
          tokenizer = function(x) tokenizers::tokenize_character_shingles(x,
                                                                          n = control_txt$n_shingles,
                                                                          lowercase = control_txt$lowercase,
                                                                          strip_non_alphanum = control_txt$strip_non_alphanum),
          n_chunks = control_txt$n_chunks,
          progressbar = verbose)
      }

      x_voc <- text2vec::create_vocabulary(x_tokens)
      x_vec <- text2vec::vocab_vectorizer(x_voc)
      x_dtm <- text2vec::create_dtm(x_tokens, x_vec)

      if (is.null(y_default)) {
        y_dtm <- x_dtm
      } else {
        if (.Platform$OS.type == "unix") {
          y_tokens <- text2vec::itoken_parallel(
            iterable = y,
            tokenizer = function(x) tokenizers::tokenize_character_shingles(x,
                                                                            n = control_txt$n_shingles,
                                                                            lowercase = control_txt$lowercase,
                                                                            strip_non_alphanum = control_txt$strip_non_alphanum),
            n_chunks = control_txt$n_chunks,
            progressbar = verbose)
        } else {
          y_tokens <- text2vec::itoken(
            iterable = y,
            tokenizer = function(x) tokenizers::tokenize_character_shingles(x,
                                                                            n = control_txt$n_shingles,
                                                                            lowercase = control_txt$lowercase,
                                                                            strip_non_alphanum = control_txt$strip_non_alphanum),
            n_chunks = control_txt$n_chunks,
            progressbar = verbose)
        }
        y_voc <- text2vec::create_vocabulary(y_tokens)
        y_vec <- text2vec::vocab_vectorizer(y_voc)
        y_dtm <- text2vec::create_dtm(y_tokens, y_vec)

      }
    }


  }

  if (representation == "shingles"){
    colnames_xy <- intersect(colnames(x_dtm), colnames(y_dtm))
  }

  if (verbose %in% 1:2) {

    if (representation == "shingles") {
      cat(sprintf("===== starting search (%s, x, y: %d, %d, t: %d) =====\n",
                  ann, nrow(x_dtm), nrow(y_dtm), length(colnames_xy)))
    } else {
      cat("===== starting search =====")
    }

  }

  x_df <- switch(ann,
                 "nnd" = method_nnd(x = if (representation == "shingles") x_dtm[, colnames_xy] else x_embeddings,
                                    y = if (representation == "shingles") y_dtm[, colnames_xy] else y_embeddings,
                                    k = k,
                                    distance = distance,
                                    deduplication = deduplication,
                                    verbose = if (verbose == 2) TRUE else FALSE,
                                    n_threads = n_threads,
                                    control = control_ann),
                 "hnsw" = method_hnsw(x = if (representation == "shingles") x_dtm[, colnames_xy] else x_embeddings,
                                      y = if (representation == "shingles") y_dtm[, colnames_xy] else y_embeddings,
                                      k = k,
                                      distance = distance,
                                      verbose = if (verbose == 2) TRUE else FALSE,
                                      n_threads = n_threads,
                                      path = ann_write,
                                      control = control_ann),
                 "lsh" = method_mlpack(x = if (representation == "shingles") x_dtm[, colnames_xy] else x_embeddings,
                                       y = if (representation == "shingles") y_dtm[, colnames_xy] else y_embeddings,
                                       algo = "lsh",
                                       k = k,
                                       verbose = if (verbose == 2) TRUE else FALSE,
                                       seed = seed,
                                       path = ann_write,
                                       control = control_ann),
                 "kd" = method_mlpack(x = if (representation == "shingles") x_dtm[, colnames_xy] else x_embeddings,
                                      y = if (representation == "shingles") y_dtm[, colnames_xy] else y_embeddings,
                                      algo = "kd",
                                      k = k,
                                      verbose = if (verbose == 2) TRUE else FALSE,
                                      seed = seed,
                                      path = ann_write,
                                      control = control_ann),
                 "annoy" = method_annoy(x = if (representation == "shingles") x_dtm[, colnames_xy] else x_embeddings,
                                        y = if (representation == "shingles") y_dtm[, colnames_xy] else y_embeddings,
                                        k = k,
                                        distance  = distance,
                                        verbose = if (verbose == 2) TRUE else FALSE,
                                        seed = seed,
                                        path = ann_write,
                                        control = control_ann))


  if (verbose %in% 1:2) cat("===== creating graph =====\n")

  ## remove duplicated pairs

  if (deduplication) {
    setDT(x_df)
    setorder(x_df, x)
    x_df[, "pair" := sapply(seq_len(.N), function(i) paste(sort(c(x[i], y[i])), collapse = "_"))]
    x_df <- x_df[, .SD[dist == min(dist)], by = "pair"]
    x_df <- x_df[, .SD[1], by = "pair"]
    x_df[, "pair" := NULL]
    x_df <- x_df[x != y]
  }

  x_df[, x := as.integer(x)]

  if (deduplication) {
    x_df[, `:=`("query_g", paste0("q", y))]
    x_df[, `:=`("index_g", paste0("q", x))]
  } else {
    x_df[, `:=`("query_g", paste0("q", y))]
    x_df[, `:=`("index_g", paste0("i", x))]
  }

  x_gr <- igraph::graph_from_data_frame(x_df[, c("query_g", "index_g")], directed = F)
  x_block <- igraph::components(x_gr, "weak")$membership

  x_df[, `:=`("block", x_block[names(x_block) %in% x_df$query_g])]

  ## if true are given
  if (!is.null(true_blocks)) {

    setDT(true_blocks)

    if (!deduplication) {

      eval <- eval_reclin(x_df, true_blocks)
      eval_metrics <- unlist(get_metrics(TP = eval$TP,
                                  FP = eval$FP,
                                  FN = eval$FN,
                                  TN = eval$TN))
      confusion <- get_confusion(TP = eval$TP,
                                 FP = eval$FP,
                                 FN = eval$FN,
                                 TN = eval$TN)

    } else {

      eval <- eval_dedup(x_df, true_blocks)
      eval_metrics <- unlist(get_metrics(TP = eval$TP,
                                         FP = eval$FP,
                                         FN = eval$FN,
                                         TN = eval$TN))
      confusion <- get_confusion(TP = eval$TP,
                                 FP = eval$FP,
                                 FN = eval$FN,
                                 TN = eval$TN)

    }

  }

  if (deduplication){
    x_df[, `:=`(x = pmin(x, y), y = pmax(x, y))]
  }

  setorderv(x_df, c("x", "y", "block"))

  structure(
    list(
      result = x_df[, c("x", "y", "block", "dist")],
      method = ann,
      deduplication = deduplication,
      representation = representation,
      metrics = if (is.null(true_blocks)) NULL else eval_metrics,
      confusion = if (is.null(true_blocks)) NULL else confusion,
      colnames = if (exists("colnames_xy", where = environment())) colnames_xy else NULL,
      graph = if (graph) {
        igraph::graph_from_data_frame(x_df[, c("x", "y")], directed = F)
        } else NULL
   ),
   class = "blocking"
  )
}

