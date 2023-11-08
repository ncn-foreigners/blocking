#' @method print blocking
#' @exportS3Method
print.blocking <- function(x,...) {

  blocks_summ <- x$result$block
  rr <- 1 - sum(choose(table(blocks_summ), 2))/choose(length(blocks_summ), 2)
  cat("========================================================\n")
  cat("Blocking based on the", x$method, "method.\n")
  cat("Number of blocks: ", length(unique(blocks_summ)), ".\n",sep="")
  cat("Number of columns used for blocking: ", NROW(x$colnames), ".\n",sep="")
  cat("Reduction ratio: ", round(rr, 4), ".\n",sep="")

  cat("========================================================\n")
  cat("Distribution of the size of the blocks:")

  print(table(table(blocks_summ)))

  if (!is.null(x$metrics)) {
    cat("========================================================\n")
    cat("Evaluation metrics (standard):\n" )
    print(x$metrics[6:11])
    cat("\nEvaluation metrics (graph-based):\n" )
    print(x$metrics[1:5])

  }
  invisible(x)
}
