#'
#' @importFrom text2vec space_tokenizer
#'
#' @title Sentence to vector
#'
#' @description
#' Function creates a matrix with word embeddings using a given model.
#'
#' @param sentences a character vector,
#' @param model a matrix containing word embeddings (e.g., GloVe).
#'
sentence_to_vector <- function(sentences, model) {
  tokens <- text2vec::space_tokenizer(tolower(sentences))

  dim <- ncol(model)
  result <- matrix(0, nrow = length(sentences), ncol = dim)

  for (i in seq_along(sentences)) {
    words <- tokens[[i]]

    valid_words <- words[words %in% rownames(model)]

    if (length(valid_words) > 0) {
      word_vectors <- model[valid_words, , drop = FALSE]
      result[i, ] <- colMeans(word_vectors)
    }
    else {
      result[i, ] <- 0
    }
  }

  return(result)
}



