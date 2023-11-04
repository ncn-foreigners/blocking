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

  stopifnot("Only one `on` is possible" = length(on) == 1)

  if (!is.null(y)) deduplication <- FALSE

  y <- if (deduplication) x else y

  block_result  <- blocking::blocking(x = x[, on],
                                      y = if (deduplication) NULL else y[, on],
                                      deduplication = deduplication,
                                      ...)

  block_ann <- data.table::as.data.table(block_result$result)

  x <- data.table::as.data.table(x)
  y <- data.table::as.data.table(y)

  a <- x[, ..on]
  a[, `:=`(".x", .I)]
  a <- a[unique(block_ann[,.(".x"=x, block)]), on = ".x"]
  a[, `:=`((on), NULL)]

  b <- y[, `..on`]
  b[, `:=`(".y", .I)]
  b <- b[unique(block_ann[,.(".y"=y, block)]), on = ".y"]
  b[, `:=`((on), NULL)]

  pairs <- merge(a, b,
                 by = "block",
                 all.x = FALSE,
                 all.y = FALSE,
                 allow.cartesian = TRUE)

  if (deduplication)  pairs <- pairs[.y > .x]

  data.table::setkey(pairs, NULL)
  data.table::setattr(pairs, "class", c("pairs", class(pairs)))
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

