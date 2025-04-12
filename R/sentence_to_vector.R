#' Imports
#' @importFrom text2vec space_tokenizer
#'
#' @title Sentence to vector with GloVe
#'
#' @export
sentence_to_vector <- function(sentences) {
  tokens <- text2vec::space_tokenizer(tolower(sentences))

  dim <- ncol(glove_vectors)
  result <- matrix(0, nrow = length(sentences), ncol = dim)

  for (i in seq_along(sentences)) {
    words <- tokens[[i]]

    valid_words <- words[words %in% rownames(glove_vectors)]

    if (length(valid_words) > 0) {
      word_vectors <- glove_vectors[valid_words, , drop = FALSE]
      result[i, ] <- colMeans(word_vectors)
    }
    else {
      result[i, ] <- 0
    }
  }

  return(result)
}



