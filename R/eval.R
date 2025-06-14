#' @importFrom Matrix sparseMatrix
#' @importFrom Matrix rowSums
#' @importFrom Matrix colSums
#'
#' @title Evaluation for record linkage
#'
#' @description
#' Function calculates TP, FP, FN and TN for record linkage.
#'
#' @param pred_df Output from the blocking algorithm.
#' @param true_df Ground-truth links (may be subset).
#'
#' @returns
#' Returns a list containing TP, FP, FN and TN.
#'
#' @keywords internal
#' @noRd
eval_reclin <- function(pred_df, true_df) {

  pred_x_map <- unique(pred_df[, c("x", "block"), with = FALSE])
  pred_y_map <- unique(pred_df[, c("y", "block"), with = FALSE])

  true_x <- unique(true_df[, c("x", "block"), with = FALSE])
  true_y <- unique(true_df[, c("y", "block"), with = FALSE])

  pred_x <- true_x
  pred_x$block <- pred_x_map$block[match(true_x$x, pred_x_map$x)]

  pred_y <- true_y
  pred_y$block <- pred_y_map$block[match(true_y$y, pred_y_map$y)]

  max_block <- max(c(pred_df$block, true_df$block), na.rm = TRUE)
  if (any(is.na(pred_x$block))) {
    count_na <- sum(is.na(pred_x$block))
    pred_x$block[is.na(pred_x$block)] <- seq(max_block + 1, length.out = count_na)
  }
  if (any(is.na(pred_y$block))) {
    pred_y$block[is.na(pred_y$block)] <- seq(max_block + 1 + count_na,
                                             length.out = sum(is.na(pred_y$block)))
  }

  n1 <- max(c(pred_x$block, pred_y$block))
  n2 <- max(c(true_x$block, true_y$block))

  cx <- Matrix::sparseMatrix(i = pred_x$block, j = true_x$block, x = 1, dims = c(n1, n2))
  cy <- Matrix::sparseMatrix(i = pred_y$block, j = true_y$block, x = 1, dims = c(n1, n2))

  TP <- sum(cx * cy)

  row_sum_x <- Matrix::rowSums(cx)
  row_sum_y <- Matrix::rowSums(cy)
  true_pairs <- sum(row_sum_x * row_sum_y)

  col_sum_x <- Matrix::colSums(cx)
  col_sum_y <- Matrix::colSums(cy)
  pred_pairs <- sum(col_sum_x * col_sum_y)

  FP <- pred_pairs - TP
  FN <- true_pairs - TP
  NX <- nrow(true_x)
  NY <- nrow(true_y)
  TN <- NX * NY - TP - FP - FN

  return(list(TP = TP,
              FP = FP,
              FN = FN,
              TN = TN))
}

#' @title Evaluation for deduplication
#'
#' @description
#' Function calculates  TP, FP, FN and TN for deduplication.
#'
#' @param pred_df Output from the blocking algorithm.
#' @param true_df Ground-truth links (may be subset).
#'
#' @returns
#' Returns a list containing TP, FP, FN and TN.
#'
#' @keywords internal
#' @noRd
eval_dedup <- function(pred_df, true_df) {

  pred_lbl <- melt(pred_df,
                   id.vars = "block",
                   measure.vars = c("x", "y"),
                   value.name = "rec")[, c("rec", "block"), with = FALSE]
  pred_lbl <- pred_lbl[!duplicated(pred_lbl[["rec"]])]

  true_lbl <- true_df[, c("x", "block"), with = FALSE]
  setnames(true_lbl, "x", "rec")

  setkey(pred_lbl, "rec")
  setkey(true_lbl, "rec")

  pred_lbl <- true_lbl[pred_lbl]
  pred_lbl <- pred_lbl[!is.na(pred_lbl$block)]

  if (any(is.na(pred_lbl$i.block))) {
    max_block <- max(c(pred_lbl$i.block, true_lbl$block), na.rm = TRUE)
    pred_lbl$i.block[is.na(pred_lbl$i.block)] <- seq(max_block + 1,
                                                 length.out = length(sum(is.na(pred_lbl$i.block))))
  }

  grouped <- pred_lbl[, list(N = .N), by = c("block", "i.block")]

  TP <- sum(grouped$N * (grouped$N - 1) / 2)

  row_sum <- grouped[, list(row_sum = sum(N)), by = "i.block"]
  col_sum <- grouped[, list(col_sum = sum(N)), by = "block"]

  pred_pairs <- sum(row_sum$row_sum * (row_sum$row_sum - 1) / 2)
  true_pairs <- sum(col_sum$col_sum * (col_sum$col_sum - 1) / 2)

  FP <- pred_pairs - TP
  FN <- true_pairs - TP

  N <- nrow(pred_lbl)
  total_pairs <- N * (N - 1) / 2
  TN <- total_pairs - TP - FP - FN

  return(list(TP = TP,
    FP = FP,
    FN = FN,
    TN = TN
  ))
}

#' @title Metrics for evaluating dedupliaction and record linkage
#'
#' @description
#' Function calculates standard evaluation metrics.
#'
#' @param TP TP
#' @param FP FP
#' @param FN FN
#' @param TN TN
#'
#' @returns
#' Returns a list containing evaluation metrics.
#'
#' @keywords internal
#' @noRd
get_metrics <- function(TP, FP, FN, TN) {

  recall <- if (TP + FN != 0) TP / (TP + FN) else 0
  precision <- if (TP + FP != 0) TP / (TP + FP) else 0
  fpr <- if (FP + TN != 0) FP / (FP + TN) else 0
  fnr <- if (FN + TP != 0) FN / (FN + TP) else 0
  accuracy <- if (TP + FP + FN + TN != 0) (TP + TN) / (TP + FP + FN + TN) else 0
  specificity <- if (TN + FP != 0) TN / (TN + FP) else 0
  f1_score <- if (precision + recall != 0) 2 * (precision * recall) / (precision + recall) else 0

  return(list(recall = recall,
              precision = precision,
              fpr = fpr,
              fnr = fnr,
              accuracy = accuracy,
              specificity = specificity,
              f1_score = f1_score))
}

#' @title Confusion matrix
#'
#' @description
#' Function creates a confusion matrix from raw counts.
#'
#' @param TP TP
#' @param FP FP
#' @param FN FN
#' @param TN TN
#'
#' @returns
#' Returns a confusion matrix.
#'
#' @keywords internal
#' @noRd
get_confusion <- function(TP, FP, FN, TN) {

  cm <- matrix(c(TP, FP, FN, TN), nrow = 2)
  colnames(cm) <- c("Predicted Positive",
                    "Predicted Negative")
  rownames(cm) <- c("Actual Positive",
                    "Actual Negative")
  return(cm)
}
