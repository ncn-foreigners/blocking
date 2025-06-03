#' @importFrom stats setNames
#' @import data.table
#'
#' @title Integration with the reclin2 package
#' @author Maciej Beręsewicz
#'
#' @description
#' Function for the integration with the \pkg{reclin2} package. The function is based on \link[reclin2]{pair_minsim} and reuses some of its source code.
#'
#' @param x reference data (a data.frame or a data.table),
#' @param y query data  (a data.frame or a data.table, default NULL),
#' @param on a character with column name or a character vector with column names for the ANN search,
#' @param deduplication whether deduplication should be performed (default TRUE),
#' @param keep_block whether to keep the block variable in the set,
#' @param add_xy whether to add x and y,
#' @param ... arguments passed to [blocking] function.
#'
#'
#' @returns Returns a \link[data.table]{data.table} with two columns \code{.x} and \code{.y}. Columns \code{.x} and \code{.y} are row numbers from data.frames x and y respectively.
#' Returned `data.table` is also of a class \code{pairs} which allows for integration with the \link[reclin2]{compare_pairs} function.
#'
#' @examples
#'
#' # example using two datasets from reclin2
#'
#' library(reclin2)
#'
#' data("linkexample1", "linkexample2", package = "reclin2")
#'
#' linkexample1$txt <- with(linkexample1, tolower(paste0(firstname, lastname, address, sex, postcode)))
#' linkexample1$txt <- gsub("\\s+", "", linkexample1$txt)
#' linkexample2$txt <- with(linkexample2, tolower(paste0(firstname, lastname, address, sex, postcode)))
#' linkexample2$txt <- gsub("\\s+", "", linkexample2$txt)
#'
#' # pairing records from linkexample2 to linkexample1 based on txt column
#'
#' pair_ann(x = linkexample1, y = linkexample2, on = "txt", deduplication = FALSE) |>
#' compare_pairs(on = "txt", comparators = list(cmp_jarowinkler())) |>
#' score_simple("score", on = "txt") |>
#' select_threshold("threshold", score = "score", threshold = 0.75) |>
#' link(selection = "threshold")
#'
#' @export
pair_ann <- function(x,
                     y = NULL,
                     on,
                     deduplication = TRUE,
                     keep_block = TRUE,
                     add_xy = TRUE,
                     ...) {

  stopifnot("Only data.frame or data.table is supported" =
              is.data.frame(x) | is.data.table(x))

  if (!is.null(y)) deduplication <- FALSE

  if (!is.null(y)){
    stopifnot("Only data.frame or data.table is supported" =
                is.data.frame(y) | data.table::is.data.table(y))
  }

  y <- if (deduplication) x else y

  x <- data.table::as.data.table(x)
  y <- data.table::as.data.table(y)

  if (length(on) > 1) {
    x[, txt := do.call(paste0, .SD), .SDcols = on]
    y[, txt := do.call(paste0, .SD), .SDcols = on]
    temp_on <- on
    on <- "txt"
  }

  block_result  <- blocking::blocking(x = x[[on]],
                                      y = if (deduplication) NULL else y[[on]],
                                      deduplication = deduplication,
                                      ...)

  a <- x[, c(on), with = FALSE]
  a[, `:=`(".x", .I)]
  a <- a[unique(block_result$result[, c("x", "block"), with = FALSE]), on = setNames("x", ".x")]
  a[, `:=`((on), NULL)]

  b <- y[, c(on), with = FALSE]
  b[, `:=`(".y", .I)]
  b <- b[unique(block_result$result[, c("y", "block"), with = FALSE]), on = setNames("y", ".y")]
  b[, `:=`((on), NULL)]

  pairs <- merge(a, b,
                 by = "block",
                 all.x = FALSE,
                 all.y = FALSE,
                 allow.cartesian = TRUE)

  if (deduplication) pairs <- pairs[pairs[[".y"]] > pairs[[".x"]]]

  data.table::setkey(pairs, NULL)
  data.table::setattr(pairs, "class", c("pairs", class(pairs)))
  on <- temp_on
  setattr(pairs, "blocking_on", on)

  if (!keep_block) {
    pairs[, `:=`("block", NULL)]
    setcolorder(pairs, c(".x", ".y"))
  } else {
    setcolorder(pairs, c(".x", ".y", "block"))
  }

  if (deduplication) data.table::setattr(pairs, "deduplication", TRUE)

  if (add_xy) {
    data.table::setattr(pairs, "x", x)
    data.table::setattr(pairs, "y", y)
  }

  pairs
}

