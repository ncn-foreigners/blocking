.blocking_print_stats <- function(result, deduplication, n_x = NULL, n_y = NULL) {

  ## Reduction ratio is based on block membership, not on the number of edges.
  if (deduplication) {
    n_records <- if (!is.null(n_x)) {
      n_x
    } else if (nrow(result) > 0L) {
      max(c(result$x, result$y))
    } else {
      0L
    }

    membership <- if (nrow(result) > 0L) {
      unique(data.frame(record = c(result$x, result$y),
                        block = c(result$block, result$block)))
    } else {
      data.frame(record = integer(), block = integer())
    }

    ## Printed block sizes describe records assigned to candidate-pair blocks.
    block_sizes <- as.integer(table(membership$block))
    total_pairs <- choose(n_records, 2)
    rr <- if (total_pairs > 0) {
      1 - sum(choose(block_sizes, 2)) / total_pairs
    } else {
      NA_real_
    }

    return(list(rr = rr, block_sizes = block_sizes))
  }

  n_x <- if (!is.null(n_x)) {
    n_x
  } else if (nrow(result) > 0L) {
    max(result$x)
  } else {
    0L
  }
  n_y <- if (!is.null(n_y)) {
    n_y
  } else if (nrow(result) > 0L) {
    max(result$y)
  } else {
    0L
  }

  if (nrow(result) == 0L) {
    rr <- if (n_x * n_y > 0) 1 else NA_real_
    return(list(rr = rr, block_sizes = integer()))
  }

  x_membership <- unique(data.frame(record = result$x, block = result$block))
  y_membership <- unique(data.frame(record = result$y, block = result$block))

  x_block_sizes <- table(x_membership$block)
  y_block_sizes <- table(y_membership$block)
  block_names <- union(names(x_block_sizes), names(y_block_sizes))

  x_counts <- integer(length(block_names))
  y_counts <- integer(length(block_names))
  names(x_counts) <- block_names
  names(y_counts) <- block_names
  x_counts[names(x_block_sizes)] <- as.integer(x_block_sizes)
  y_counts[names(y_block_sizes)] <- as.integer(y_block_sizes)

  ## Printed block sizes exclude records with no candidate pair.
  block_sizes <- x_counts + y_counts
  total_pairs <- n_x * n_y
  rr <- if (total_pairs > 0) {
    1 - sum(x_counts * y_counts) / total_pairs
  } else {
    NA_real_
  }

  list(rr = rr, block_sizes = as.integer(block_sizes))
}

#' @method print blocking
#' @exportS3Method
print.blocking <- function(x, ...) {

  print_stats <- .blocking_print_stats(
    result = x$result,
    deduplication = x$deduplication,
    n_x = attr(x, "n_x", exact = TRUE),
    n_y = attr(x, "n_y", exact = TRUE)
  )
  rr <- print_stats$rr
  block_sizes <- print_stats$block_sizes

  cat("========================================================\n")
  cat("Blocking based on the", x$method, "method.\n")
  cat("Number of blocks: ", length(block_sizes), ".\n",sep="")
  if (x$representation %in% c("shingles", "custom_matrix")) {
    cat("Number of columns used for blocking: ", NROW(x$colnames), ".\n",sep="")
  }
  cat("Reduction ratio: ", sprintf("%.4f", rr), ".\n",sep="")

  cat("========================================================\n")
  cat("Distribution of the size of the blocks:\n")

  if (length(block_sizes) > 0L) {
    block_size_distribution <- table(block_sizes)
    names(dimnames(block_size_distribution)) <- NULL
    print(block_size_distribution)
  } else {
    cat("No blocks.\n")
  }

  if (!is.null(x$metrics)) {
    cat("========================================================\n")
    cat("Evaluation metrics (standard, in %):\n" )
    metrics <- as.numeric(sprintf("%.4f", x$metrics*100))
    names(metrics)  <- names(x$metrics)
    print(metrics)

  }
  invisible(x)
}

#' @method print est_block_error
#' @exportS3Method
print.est_block_error <- function(x, ...) {

  cat("Estimated FPR: ", sprintf("%.4f", x$FPR * 100), "%\n", sep = "")
  cat("Estimated FNR: ", sprintf("%.4f", x$FNR * 100), "%\n", sep = "")
  cat("Number of classes in the model: ", x$G, "\n")

  cat("========================================================\n")

  if (x$convergence) {
    cat("EM algorithm converged successfully within", x$iter, "iterations.")
  } else {
    cat("EM algorithm did not converge within", x$iter, "iterations.")
  }
}

#' @method logLik est_block_error
#' @exportS3Method
logLik.est_block_error <- function(object, ...) {

  val <- object$log_lik
  if (object$equal_p) {
    k <- 2 * object$G
  } else {
    k <- 3 * object$G - 1
  }

  attr(val, "df") <- k
  class(val) <- "logLik"

  val

}
