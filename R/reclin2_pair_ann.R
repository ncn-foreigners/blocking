#' @title Integration with reclin2 package
#' @author Maciej Beręsewicz
#'
#' @param x x
#' @param y y
#' @param on variable
#' @param deduplication deduplication
#' @param keep_block whether to keep block in the set
#' @param add_xy whether to add x and y
#' @param ... arguments passed to [blocking::blocking()] function.
#'
#' @returns Returns a [data.table] with two columns \code{.x} and \code{.y}. Columns \code{.x} and \code{.y} are row numbers from data.frames x and y respectively. This data.table is also of a class \code{pairs} which allows for integration witn the [reclin2] package.
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

  x <- as.data.table(x)
  y <- as.data.table(y)

  a <- x[, ..on]
  a[, `:=`(.x, .I)]
  a <- a[unique(block_ann[,.(.x=x, block)]), on = ".x"]
  a[, `:=`((on), NULL)]

  b <- y[, ..on]
  b[, `:=`(.y, .I)]
  b <- b[unique(block_ann[,.(.y=y, block)]), on = ".y"]
  b[, `:=`((on), NULL)]

  pairs <- merge(a, b,
                 by = "block",
                 all.x = FALSE,
                 all.y = FALSE,
                 allow.cartesian = TRUE)

  setkey(pairs, NULL)
  setattr(pairs, "class", c("pairs", class(pairs)))

  if (!keep_block) pairs[, `:=`("block", NULL)]

  if (deduplication) setattr(pairs, "deduplication", TRUE)

  if (add_xy) {
    setattr(pairs, "x", x)
    setattr(pairs, "y", y)
  }

  pairs
}

