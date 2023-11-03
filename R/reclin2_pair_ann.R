#' Imports
#'
#' @import data.table
#'
#' @title Integration with the reclin2 package
#' @author Maciej Beręsewicz
#'
#' @description
#' Function for the integration with the reclin2 package. The function is based on [reclin2::pair_minsim()] and reuses some of its source code.
#'
#' @param x x
#' @param y y
#' @param on variable
#' @param deduplication deduplication
#' @param keep_block whether to keep block in the set
#' @param add_xy whether to add x and y
#' @param ... arguments passed to [blocking::blocking()] function.
#'
#'
#' @returns Returns a [data.table] with two columns \code{.x} and \code{.y}. Columns \code{.x} and \code{.y} are row numbers from data.frames x and y respectively. This data.table is also of a class \code{pairs} which allows for integration witn the [reclin2::compare_pairs()] package.
#'
#' @examples
#' data("linkexample1", "linkexample2", package = "reclin2")
#'
#'
#' @export
pair_ann <- function(x,
                     y = NULL,
                     on,
                     deduplication = TRUE,
                     keep_block = TRUE,
                     add_xy = TRUE,
                     ...) {

  stopifnot("Only one `on` is possible" = length(on) == 1)

  y <- if (deduplication) x else y

  block_result  <- blocking::blocking(x = x[, on],
                                      y = y[, on],
                                      deduplication = deduplication,
                                      ...)

  block_ann <- data.table::as.data.table(block_result$result)

  x <- data.table::as.data.table(x)
  y <- data.table::as.data.table(y)

  a <- x[, ..on]
  a[, data.table::`:=`(.x, data.table::.I)]
  a <- a[unique(block_result[,.(.x=x, block)]), on = ".x"]
  a[, data.table::`:=`((on), NULL)]

  b <- y[, ..on]
  b[, data.table::`:=`(.y, data.table::.I)]
  b <- b[unique(block_result[,.(.y=y, block)]), on = ".y"]
  b[, data.table::`:=`((on), NULL)]

  pairs <- merge(a, b,
                 by = "block",
                 all.x = FALSE,
                 all.y = FALSE,
                 allow.cartesian = TRUE)

  data.table::setkey(pairs, NULL)
  data.table::setattr(pairs, "class", c("pairs", class(pairs)))

  if (!keep_block) pairs[, data.table::`:=`("block", NULL)]

  if (deduplication) data.table::setattr(pairs, "deduplication", TRUE)

  if (add_xy) {
    data.table::setattr(pairs, "x", x)
    data.table::setattr(pairs, "y", y)
  }

  pairs
}

