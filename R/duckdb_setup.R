#' @title Set Up DuckDB VSS
#'
#' @description
#' Install, update and check the DuckDB `vss` extension outside
#' [blocking()]. This helper prepares the extension once and reports
#' whether the installed build exposes DuckDB's `HNSW_INDEX_JOIN`
#' optimizer path for batched nearest-neighbour joins.
#'
#' @param dbdir path to the DuckDB database used for the setup check
#'   (default `":memory:"`).
#' @param update should `UPDATE EXTENSIONS (vss)` be executed after the
#'   extension is installed or detected?
#' @param repository DuckDB extension repository to use when installation
#'   is required. Use `"core"` for the stable repository or
#'   `"core_nightly"` to try a newer nightly build.
#' @param force should installation be forced from the selected
#'   repository?
#' @param check_index_join should a small probe query be run to check
#'   whether `HNSW_INDEX_JOIN` is available?
#' @param verbose should progress information be printed?
#'
#' @returns Returns a list describing the installed extension and whether
#'   `HNSW_INDEX_JOIN` was detected. The result also includes the DuckDB
#'   version and, when relevant, a diagnostic note about known upstream
#'   regressions.
#'
#' @export
duckdb_setup_vss <- function(dbdir = ":memory:",
                             update = TRUE,
                             repository = c("core", "core_nightly"),
                             force = FALSE,
                             check_index_join = TRUE,
                             verbose = interactive()) {

  repository <- match.arg(repository)

  con <- DBI::dbConnect(
    duckdb::duckdb(dbdir = dbdir),
    array = "matrix"
  )
  on.exit({
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
  }, add = TRUE)

  duckdb_version <- tryCatch({
    DBI::dbGetQuery(con, "SELECT version() AS version")$version[[1]]
  }, error = function(e) {
    NA_character_
  })

  extension_info <- try(
    DBI::dbGetQuery(
      con,
      paste(
        "SELECT extension_name, installed, extension_version, installed_from",
        "FROM duckdb_extensions()",
        "WHERE extension_name = 'vss'"
      )
    ),
    silent = TRUE
  )

  install_stmt <- NULL
  installed <- FALSE
  if (!inherits(extension_info, "try-error") && nrow(extension_info)) {
    installed <- isTRUE(extension_info$installed[[1]])
  }

  if (!installed || isTRUE(force)) {
    install_stmt <- if (isTRUE(force)) {
      sprintf("FORCE INSTALL vss FROM %s", repository)
    } else if (identical(repository, "core")) {
      "INSTALL vss"
    } else {
      sprintf("INSTALL vss FROM %s", repository)
    }

    if (verbose) {
      cat(sprintf("DuckDB VSS setup: %s\n", install_stmt))
    }
    DBI::dbExecute(con, install_stmt)
  }

  if (isTRUE(update)) {
    if (verbose) {
      cat("DuckDB VSS setup: UPDATE EXTENSIONS (vss)\n")
    }
    withCallingHandlers(
      DBI::dbExecute(con, "UPDATE EXTENSIONS (vss)"),
      warning = function(w) {
        if (grepl("NAs introduced by coercion",
                  conditionMessage(w),
                  fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  DBI::dbExecute(con, "LOAD vss")

  extension_info <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT extension_name, installed, extension_version, installed_from",
      "FROM duckdb_extensions()",
      "WHERE extension_name = 'vss'"
    )
  )

  hnsw_index_join <- if (isTRUE(check_index_join)) {
    DBI::dbExecute(con,
                   "CREATE TEMP TABLE x_vectors AS SELECT 1 AS id, [1.0, 1.0]::FLOAT[2] AS vec UNION ALL SELECT 2, [0.0, 1.0]::FLOAT[2]")
    DBI::dbExecute(con,
                   "CREATE TEMP TABLE y_vectors AS SELECT 1 AS id, [1.0, 1.0]::FLOAT[2] AS vec UNION ALL SELECT 2, [0.0, 1.0]::FLOAT[2]")
    DBI::dbExecute(con,
                   "CREATE INDEX x_vectors_hnsw_probe_idx ON x_vectors USING HNSW (vec) WITH (metric = 'cosine')")
    duckdb_vss_uses_index_join(con,
                               distance_fun = "array_cosine_distance",
                               k_search = 1L)
  } else {
    NA
  }

  diagnostic <- NULL
  if (identical(hnsw_index_join, FALSE)) {
    if (is.character(duckdb_version) &&
        grepl("^v?1\\.5\\.0([.-]|$)", duckdb_version)) {
      diagnostic <- paste(
        "DuckDB 1.5.0 is affected by an upstream VSS",
        "`HNSW_INDEX_JOIN` regression (duckdb/duckdb-vss#80).",
        "`UPDATE EXTENSIONS (vss)` cannot fix this if the latest",
        "compatible 1.5.0 build is still regressed.",
        "If you need `HNSW_INDEX_JOIN` today, try DuckDB 1.4.4."
      )
    } else {
      diagnostic <- paste(
        "`HNSW_INDEX_JOIN` was not detected for this DuckDB/VSS build.",
        "Try `duckdb_setup_vss(update = TRUE)` first; if that still",
        "reports `FALSE`, check whether your DuckDB version has an",
        "upstream VSS optimizer regression."
      )
    }
  }

  result <- list(
    extension = extension_info$extension_name[[1]],
    installed = isTRUE(extension_info$installed[[1]]),
    extension_version = extension_info$extension_version[[1]],
    installed_from = extension_info$installed_from[[1]],
    duckdb_version = duckdb_version,
    hnsw_index_join = hnsw_index_join,
    diagnostic = diagnostic,
    dbdir = dbdir,
    repository = repository,
    install_statement = install_stmt
  )

  if (verbose) {
    cat(sprintf("DuckDB version: %s\n", result$duckdb_version))
    cat(sprintf("DuckDB VSS version: %s (%s)\n",
                result$extension_version,
                result$installed_from))
    if (!is.na(result$hnsw_index_join)) {
      cat(sprintf("DuckDB VSS HNSW_INDEX_JOIN available: %s\n",
                  if (result$hnsw_index_join) "TRUE" else "FALSE"))
    }
    if (!is.null(result$diagnostic)) {
      cat(sprintf("DuckDB VSS note: %s\n", result$diagnostic))
    }
  }

  invisible(result)
}
