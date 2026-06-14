.blocking_ann_input <- function(x, on) {
  input <- data.table::as.data.table(x)[, on, with = FALSE]
  input <- input[, lapply(.SD, function(value) {
    value <- as.character(value)
    value[is.na(value)] <- ""
    value
  })]

  do.call(paste0, input)
}

.blocking_validate_on <- function(x, y, on) {
  stopifnot("`x` should be a data.frame or data.table when `on` is used." =
              is.data.frame(x) | data.table::is.data.table(x))
  stopifnot("`on` should be a character vector." =
              is.character(on) && length(on) > 0L)
  stopifnot("All `on` variables should be included in `x`." =
              all(on %in% names(x)))

  if (!is.null(y)) {
    stopifnot("`y` should be a data.frame or data.table when `on` is used." =
                is.data.frame(y) | data.table::is.data.table(y))
    stopifnot("All `on` variables should be included in `y`." =
                all(on %in% names(y)))
  }

  invisible(TRUE)
}

.blocking_validate_on_blocking <- function(x, y, on_blocking) {
  stopifnot("`on_blocking` should be a character vector." =
              is.character(on_blocking) && length(on_blocking) > 0L)
  stopifnot("All `on_blocking` variables should be included in `x`." =
              all(on_blocking %in% names(x)))

  if (!is.null(y)) {
    stopifnot("All `on_blocking` variables should be included in `y`." =
                all(on_blocking %in% names(y)))
  }

  x_blocking <- x[, on_blocking, with = FALSE]
  stopifnot("Missing values in `on_blocking` variables are not supported." =
              !anyNA(x_blocking))

  if (!is.null(y)) {
    y_blocking <- y[, on_blocking, with = FALSE]
    stopifnot("Missing values in `on_blocking` variables are not supported." =
                !anyNA(y_blocking))
  }

  invisible(TRUE)
}

.blocking_group_ids <- function(x, y, on_blocking) {
  source_col <- "..blocking_source"
  row_col <- "..blocking_row"
  block_col <- "..blocking_block"

  x_key <- data.table::copy(x[, on_blocking, with = FALSE])
  y_key <- data.table::copy(y[, on_blocking, with = FALSE])

  x_key[, (row_col) := seq_len(.N)]
  y_key[, (row_col) := seq_len(.N)]

  keys <- data.table::rbindlist(list(x = x_key, y = y_key),
                                idcol = source_col,
                                use.names = TRUE)
  keys[, (block_col) := .GRP, by = on_blocking]

  x_ids <- keys[keys[[source_col]] == "x"]
  y_ids <- keys[keys[[source_col]] == "y"]
  data.table::setorderv(x_ids, row_col)
  data.table::setorderv(y_ids, row_col)

  list(
    x = x_ids[[block_col]],
    y = y_ids[[block_col]]
  )
}

.blocking_empty_result <- function() {
  data.table::data.table(x = integer(),
                         y = integer(),
                         dist = numeric())
}

.blocking_subset_rows <- function(x, rows) {
  x[rows, , drop = FALSE]
}

.blocking_run_ann <- function(ann,
                              x,
                              y,
                              k,
                              distance,
                              deduplication,
                              verbose,
                              seed,
                              n_threads,
                              path,
                              control) {

  ann_verbose <- if (verbose == 2) TRUE else FALSE

  switch(ann,
         "nnd" = method_nnd(x = x,
                            y = y,
                            k = k,
                            distance = distance,
                            deduplication = deduplication,
                            seed = seed,
                            verbose = ann_verbose,
                            n_threads = n_threads,
                            control = control),
         "hnsw" = method_hnsw(x = x,
                              y = y,
                              k = k,
                              distance = distance,
                              seed = seed,
                              verbose = ann_verbose,
                              n_threads = n_threads,
                              path = path,
                              control = control),
         "lsh" = method_mlpack(x = x,
                               y = y,
                               algo = "lsh",
                               k = k,
                               verbose = ann_verbose,
                               seed = seed,
                               path = path,
                               control = control),
         "kd" = method_mlpack(x = x,
                              y = y,
                              algo = "kd",
                              k = k,
                              verbose = ann_verbose,
                              seed = seed,
                              path = path,
                              control = control),
         "annoy" = method_annoy(x = x,
                                y = y,
                                k = k,
                                distance  = distance,
                                verbose = ann_verbose,
                                seed = seed,
                                path = path,
                                control = control))
}

.blocking_run_grouped_ann <- function(ann,
                                      x,
                                      y,
                                      x_blocking,
                                      y_blocking,
                                      k,
                                      distance,
                                      deduplication,
                                      verbose,
                                      seed,
                                      n_threads,
                                      control) {

  x_groups <- split(seq_len(nrow(x)), x_blocking)
  y_groups <- split(seq_len(nrow(y)), y_blocking)
  common_groups <- intersect(names(y_groups), names(x_groups))
  result <- vector("list", length(common_groups))

  for (i in seq_along(common_groups)) {
    group_id <- common_groups[[i]]
    x_rows <- x_groups[[group_id]]
    y_rows <- y_groups[[group_id]]

    if (length(x_rows) < k || length(y_rows) == 0L) {
      next
    }

    x_group <- .blocking_subset_rows(x, x_rows)
    y_group <- .blocking_subset_rows(y, y_rows)

    group_result <- .blocking_run_ann(ann = ann,
                                      x = x_group,
                                      y = y_group,
                                      k = k,
                                      distance = distance,
                                      deduplication = deduplication,
                                      verbose = verbose,
                                      seed = seed,
                                      n_threads = n_threads,
                                      path = NULL,
                                      control = control)

    if (nrow(group_result) == 0L) {
      next
    }

    group_result[, `:=`(x = x_rows[x],
                        y = y_rows[y])]
    result[[i]] <- group_result
  }

  result <- data.table::rbindlist(result, use.names = TRUE)
  if (nrow(result) == 0L) {
    return(.blocking_empty_result())
  }

  result
}
