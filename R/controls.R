#' @title Controls for HNSW
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
