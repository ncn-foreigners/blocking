#' @method print blocking
#' @exportS3Method
print.blocking <- function(x,...) {

  blocks_summ <- x$result$block

  cat("Blocking based on the", x$method, "method.\n")
  cat("Number of blocks: ", length(unique(blocks_summ)), ".\n",sep="")
  cat("Number of columns used for blocking: ", NROW(x$colnames), ".\n",sep="")
  cat("Distribution of the size of the blocks:")

  print(table(table(unique(blocks_summ))))

  invisible(x)
}
