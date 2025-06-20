#' @method print blocking
#' @exportS3Method
print.blocking <- function(x, ...) {

  blocks_tab <- table(x$result$block)
  block_ids <- rep(as.numeric(names(blocks_tab)), blocks_tab+1)

  rr <- 1 - sum(choose(table(block_ids), 2))/choose(length(block_ids), 2)
  cat("========================================================\n")
  cat("Blocking based on the", x$method, "method.\n")
  cat("Number of blocks: ", length(unique(block_ids)), ".\n",sep="")
  if (x$representation == "shingles") {
    cat("Number of columns used for blocking: ", NROW(x$colnames), ".\n",sep="")
  }
  cat("Reduction ratio: ", sprintf("%.4f", rr), ".\n",sep="")

  cat("========================================================\n")
  cat("Distribution of the size of the blocks:")

  print(table(table(block_ids)))

  if (!is.null(x$metrics)) {
    cat("========================================================\n")
    cat("Evaluation metrics (standard):\n" )
    metrics <- as.numeric(sprintf("%.4f", x$metrics*100))
    names(metrics)  <- names(x$metrics)
    print(metrics)

  }
  invisible(x)
}

#' @method print est_block_error
#' @exportS3Method
print.est_block_error <- function(x, ...) {

  cat("FPR: ", x$FPR, "\n")
  cat("FNR: ", x$FNR, "\n")

  cat("========================================================\n")

  if (x$convergence) {
    cat("EM algorithm converged successfully within", x$iter, "iterations.")
  } else {
    cat("EM algorithm did not converge within", x$iter, "iterations.")
  }
}

