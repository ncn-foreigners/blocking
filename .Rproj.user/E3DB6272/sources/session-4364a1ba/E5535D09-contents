block_for_reclin <- function(text,
                             distance = "cosine",
                             M = 25,
                             ef = 200,
                             verbose = T,
                             progress = "bar",
                             n_threads = 4) {

  l_tokens <- text2vec::itoken_parallel(iterable = text,
                              tokenizer = function(x) tokenizers::tokenize_character_shingles(x, n = 2L),
                              n_chunks = 10,
                              progressbar = T)

  l_voc <- text2vec::create_vocabulary(l_tokens)
  l_vec <- text2vec::vocab_vectorizer(l_voc)
  l_dtm <- text2vec::create_dtm(l_tokens, l_vec)

  l_dtm <- base::as.matrix(l_dtm)

  l_ind <- RcppHNSW::hnsw_build(X = l_dtm,
                      distance = distance,
                      M = M,
                      ef = ef,
                      verbose = verbose,
                      progress = progress,
                      n_threads = n_threads)

  l_1nn <- RcppHNSW::hnsw_search(X = l_dtm, ann = l_ind,
                                 k = 2, ef = ef, verbose = verbose, progress = progress, n_threads = n_threads)

  l_df <- data.table::setDT(base::data.frame(l_1nn$idx))
  l_df[, id := 1:NROW(text)]
  l_df[, selected:=base::ifelse(X1 == id, X2, X1)]
  l_df_to_graph <- data.table::merge.data.table(x = l_df[, .(id, selected)],
                                                y = l_df[, .(id, selected)],
                                                by = "selected",
                                                allow.cartesian = TRUE)

  l_m_sp <- Matrix::sparseMatrix(i = l_df_to_graph$id.x, j = l_df_to_graph$id.y, x = 1)

  l_gr <- igraph::graph_from_adjacency_matrix(l_m_sp, mode = "undirected")
  l_clust <- igraph::components(l_gr, "weak")

  l_clust$membership
}
