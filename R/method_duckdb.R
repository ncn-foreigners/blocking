#' @title Internal DuckDB backend
#'
#' @description
#' Internal helpers for DuckDB-based blocking using the `vss` or `faiss`
#' extensions.
#'
#' @keywords internal
#' @noRd

duckdb_escape_string <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
}

duckdb_normalize_sql <- function(column, control_txt) {
  expr <- sprintf("coalesce(%s, '')", column)

  if (isTRUE(control_txt$strip_non_alphanum)) {
    expr <- sprintf("regexp_replace(%s, '[^[:alnum:]]', '', 'g')", expr)
  }

  if (isTRUE(control_txt$lowercase)) {
    expr <- sprintf("lower(%s)", expr)
  }

  expr
}

duckdb_vector_literal <- function(x, n_features) {
  values <- format(as.numeric(x), scientific = FALSE, trim = TRUE)
  paste0("[", paste(values, collapse = ", "), "]::FLOAT[", n_features, "]")
}

duckdb_list_literal <- function(x) {
  values <- format(as.numeric(x), scientific = FALSE, trim = TRUE)
  paste0("[", paste(values, collapse = ", "), "]::FLOAT[]")
}

duckdb_extension_install_instructions <- function(engine) {
  install_sql <- switch(engine,
                        "vss" = "DBI::dbExecute(con, \"INSTALL vss\")",
                        "faiss" = "DBI::dbExecute(con, \"INSTALL faiss FROM community\")")

  paste(
    "con <- DBI::dbConnect(duckdb::duckdb(), dbdir = \":memory:\")",
    install_sql,
    "DBI::dbDisconnect(con, shutdown = TRUE)",
    sep = "\n"
  )
}

duckdb_load_extension <- function(con, control_duckdb) {
  engine <- control_duckdb$engine

  load_sql <- switch(engine,
                     "vss" = "LOAD vss",
                     "faiss" = "LOAD faiss")

  if (isTRUE(control_duckdb$install_extension)) {
    warning(paste(
      "`control_duckdb(install_extension = TRUE)` is deprecated.",
      "Install DuckDB extensions once before calling `blocking()`;",
      "this backend now only loads them at runtime."
    ), call. = FALSE)
  }

  tryCatch({
    DBI::dbExecute(con, load_sql)
  }, error = function(e) {
    extension_state <- try(
      DBI::dbGetQuery(
        con,
        sprintf(
          paste(
            "SELECT installed",
            "FROM duckdb_extensions()",
            "WHERE extension_name = '%s'"
          ),
          engine
        )
      ),
      silent = TRUE
    )

    installed_hint <- ""
    if (!inherits(extension_state, "try-error") &&
        nrow(extension_state) &&
        !isTRUE(extension_state$installed[[1]])) {
      installed_hint <- " It is not installed in this DuckDB environment."
    }

    stop(paste0(
      "DuckDB `", engine, "` extension could not be loaded: ",
      conditionMessage(e), ".", installed_hint,
      "\n\nInstall it once before calling `blocking()`:\n",
      duckdb_extension_install_instructions(engine)
    ),
         call. = FALSE)
  })
}

duckdb_create_shingles <- function(con,
                                   input_table,
                                   shingles_table,
                                   n_shingles) {

  shingle_span <- as.integer(n_shingles) - 1L
  shingle_limit <- as.integer(n_shingles)

  DBI::dbExecute(
    con,
    sprintf(
      paste(
        "CREATE TEMP TABLE %s AS",
        "SELECT id, txt[pos:pos + %d] AS shingle",
        "FROM %s, range(1, greatest(length(txt) - %d, 0) + 1) pos_tbl(pos)"
      ),
      shingles_table,
      shingle_span,
      input_table,
      shingle_limit - 1L
    )
  )
}

duckdb_create_vector_table <- function(con,
                                       input_table,
                                       shingles_table,
                                       vectors_table,
                                       vector_cast) {

  DBI::dbExecute(
    con,
    sprintf(
      paste(
        "CREATE TEMP TABLE %s AS",
        "WITH ids AS (SELECT id FROM %s),",
        "counts AS (",
        "  SELECT id, v.pos, count(*)::FLOAT AS value",
        "  FROM %s s",
        "  JOIN vocab v USING (shingle)",
        "  GROUP BY id, v.pos",
        ")",
        "SELECT ids.id,",
        "       list(coalesce(counts.value, 0.0) ORDER BY vocab.pos)%s AS vec",
        "FROM ids",
        "CROSS JOIN vocab",
        "LEFT JOIN counts ON counts.id = ids.id AND counts.pos = vocab.pos",
        "GROUP BY ids.id",
        "ORDER BY ids.id"
      ),
      vectors_table,
      input_table,
      shingles_table,
      vector_cast
    )
  )
}

duckdb_create_vector_table_from_entries <- function(con,
                                                    ids_table,
                                                    entries_table,
                                                    vectors_table,
                                                    n_features,
                                                    vector_cast) {

  DBI::dbExecute(
    con,
    sprintf(
      paste(
        "CREATE TEMP TABLE %s AS",
        "WITH positions AS (",
        "  SELECT pos",
        "  FROM range(1, %d + 1) pos_tbl(pos)",
        ")",
        "SELECT ids.id,",
        "       list(coalesce(entries.value, 0.0) ORDER BY positions.pos)%s AS vec",
        "FROM %s ids",
        "CROSS JOIN positions",
        "LEFT JOIN %s entries",
        "  ON entries.id = ids.id",
        " AND entries.pos = positions.pos",
        "GROUP BY ids.id",
        "ORDER BY ids.id"
      ),
      vectors_table,
      as.integer(n_features),
      vector_cast,
      ids_table,
      entries_table
    )
  )
}

duckdb_matrix_colnames <- function(x, n_features) {
  cols <- colnames(x)
  if (is.null(cols)) {
    cols <- paste0("feature_", seq_len(n_features))
  }
  cols
}

duckdb_matrix_triplets <- function(x) {
  sparse_x <- if (inherits(x, "Matrix")) {
    methods::as(x, "dgCMatrix")
  } else {
    methods::as(Matrix::Matrix(x, sparse = TRUE), "dgCMatrix")
  }

  sparse_x <- Matrix::drop0(sparse_x)
  entries <- Matrix::summary(sparse_x)

  if (!nrow(entries)) {
    return(data.frame(id = integer(),
                      pos = integer(),
                      value = numeric()))
  }

  data.frame(id = as.integer(entries$i),
             pos = as.integer(entries$j),
             value = as.numeric(entries$x))
}

duckdb_prepare_matrix_backend <- function(con,
                                          x,
                                          y,
                                          deduplication,
                                          control_duckdb) {
  x_n_features <- ncol(x)
  y_n_features <- ncol(y)

  if (deduplication) {
    colnames_xy <- duckdb_matrix_colnames(x, x_n_features)
    x_use <- x
    y_use <- y
  } else if (!is.null(colnames(x)) && !is.null(colnames(y))) {
    colnames_xy <- intersect(colnames(x), colnames(y))
    if (!length(colnames_xy)) {
      return(list(result = data.table::data.table(y = integer(),
                                                  x = integer(),
                                                  dist = numeric()),
                  colnames = character()))
    }
    x_use <- x[, colnames_xy, drop = FALSE]
    y_use <- y[, colnames_xy, drop = FALSE]
  } else {
    if (x_n_features != y_n_features) {
      stop(paste(
        "DuckDB matrix input requires matching column names on both matrices,",
        "or the same number of columns when names are missing."
      ), call. = FALSE)
    }

    colnames_xy <- duckdb_matrix_colnames(x, x_n_features)
    x_use <- x
    y_use <- y
  }

  n_features <- length(colnames_xy)
  if (!n_features) {
    return(list(result = data.table::data.table(y = integer(),
                                                x = integer(),
                                                dist = numeric()),
                colnames = character()))
  }

  vector_cast <- if (control_duckdb$engine == "vss") {
    sprintf("::FLOAT[%d]", n_features)
  } else {
    "::FLOAT[]"
  }

  DBI::dbWriteTable(
    con,
    "x_ids",
    data.frame(id = seq_len(nrow(x_use))),
    overwrite = TRUE,
    temporary = TRUE
  )
  DBI::dbWriteTable(
    con,
    "x_entries",
    duckdb_matrix_triplets(x_use),
    overwrite = TRUE,
    temporary = TRUE
  )

  duckdb_create_vector_table_from_entries(
    con,
    ids_table = "x_ids",
    entries_table = "x_entries",
    vectors_table = "x_vectors",
    n_features = n_features,
    vector_cast = vector_cast
  )

  DBI::dbWriteTable(
    con,
    "y_ids",
    data.frame(id = seq_len(nrow(y_use))),
    overwrite = TRUE,
    temporary = TRUE
  )
  DBI::dbWriteTable(
    con,
    "y_entries",
    duckdb_matrix_triplets(y_use),
    overwrite = TRUE,
    temporary = TRUE
  )

  duckdb_create_vector_table_from_entries(
    con,
    ids_table = "y_ids",
    entries_table = "y_entries",
    vectors_table = "y_vectors",
    n_features = n_features,
    vector_cast = vector_cast
  )

  list(colnames = colnames_xy,
       n_features = n_features)
}

duckdb_vss_search_query <- function(distance_fun,
                                    k_search) {
  sprintf(
    paste(
      "WITH ranked AS (",
      "  SELECT y.id AS y,",
      "         nn.x AS x,",
      "         nn.dist AS dist,",
      "         row_number() OVER (PARTITION BY y.id ORDER BY nn.dist, nn.x) AS rn",
      "  FROM y_vectors y,",
      "  LATERAL (",
      "    SELECT x_vectors.id AS x,",
      "           %s(x_vectors.vec, y.vec) AS dist",
      "    FROM x_vectors",
      "    ORDER BY dist",
      "    LIMIT %d",
      "  ) nn",
      ")",
      "SELECT y, x, dist",
      "FROM ranked",
      "WHERE rn = 1",
      "ORDER BY y"
    ),
    distance_fun,
    as.integer(k_search)
  )
}

duckdb_vss_uses_index_join <- function(con,
                                       distance_fun,
                                       k_search) {
  plan <- DBI::dbGetQuery(
    con,
    paste("EXPLAIN", duckdb_vss_search_query(distance_fun, k_search))
  )

  grepl("HNSW_INDEX_JOIN",
        paste(plan$explain_value, collapse = "\n"),
        fixed = TRUE)
}

duckdb_search_vss_classic <- function(con,
                                      distance_fun,
                                      n_features,
                                      k,
                                      k_search) {

  queries <- DBI::dbGetQuery(con, "SELECT * FROM y_vectors ORDER BY id")
  vec_cols <- setdiff(colnames(queries), "id")
  effective_k <- min(as.integer(k), as.integer(k_search))

  matches <- lapply(seq_len(nrow(queries)), function(i) {
    vec_literal <- duckdb_vector_literal(queries[i, vec_cols, drop = TRUE],
                                         n_features = n_features)

    res <- DBI::dbGetQuery(
      con,
      sprintf(
        paste(
          "SELECT id AS x, %s(vec, %s) AS dist",
          "FROM x_vectors",
          "ORDER BY dist",
          "LIMIT %d"
        ),
        distance_fun,
        vec_literal,
        as.integer(k_search)
      )
    )

    if (!nrow(res)) {
      return(data.table::data.table(y = integer(),
                                    x = integer(),
                                    dist = numeric()))
    }

    res <- res[min(effective_k, nrow(res)), , drop = FALSE]
    data.table::data.table(y = as.integer(queries$id[[i]]),
                           x = as.integer(res$x[[1]]),
                           dist = as.numeric(res$dist[[1]]))
  })

  data.table::rbindlist(matches)
}

duckdb_search_vss_lateral <- function(con,
                                      distance_fun,
                                      k_search) {
  data.table::as.data.table(
    DBI::dbGetQuery(con, duckdb_vss_search_query(distance_fun, k_search))
  )
}

duckdb_prepare_text_backend <- function(con,
                                        x,
                                        y,
                                        deduplication,
                                        control_txt,
                                        control_duckdb) {
  engine <- control_duckdb$engine

  DBI::dbWriteTable(con,
                    "x_raw",
                    data.frame(id = seq_along(x), txt = x, stringsAsFactors = FALSE),
                    overwrite = TRUE,
                    temporary = TRUE)
  DBI::dbWriteTable(con,
                    "y_raw",
                    data.frame(id = seq_along(y), txt = y, stringsAsFactors = FALSE),
                    overwrite = TRUE,
                    temporary = TRUE)

  norm_expr <- duckdb_normalize_sql("txt", control_txt)

  DBI::dbExecute(con,
                 sprintf("CREATE TEMP TABLE x_input AS SELECT id, %s AS txt FROM x_raw",
                         norm_expr))
  DBI::dbExecute(con,
                 sprintf("CREATE TEMP TABLE y_input AS SELECT id, %s AS txt FROM y_raw",
                         norm_expr))

  duckdb_create_shingles(con,
                         input_table = "x_input",
                         shingles_table = "x_shingles",
                         n_shingles = control_txt$n_shingles)
  duckdb_create_shingles(con,
                         input_table = "y_input",
                         shingles_table = "y_shingles",
                         n_shingles = control_txt$n_shingles)

  vocab_sql <- if (deduplication) {
    paste(
      "CREATE TEMP TABLE vocab AS",
      "SELECT shingle, row_number() OVER (ORDER BY shingle) AS pos",
      "FROM (SELECT DISTINCT shingle FROM x_shingles)"
    )
  } else {
    paste(
      "CREATE TEMP TABLE vocab AS",
      "SELECT shingle, row_number() OVER (ORDER BY shingle) AS pos",
      "FROM (",
      "  SELECT DISTINCT shingle FROM x_shingles",
      "  INTERSECT",
      "  SELECT DISTINCT shingle FROM y_shingles",
      ")"
    )
  }

  DBI::dbExecute(con, vocab_sql)

  n_features <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM vocab")$n[[1]]
  if (n_features == 0) {
    return(list(result = data.table::data.table(y = integer(),
                                                x = integer(),
                                                dist = numeric()),
                colnames = character()))
  }

  colnames_xy <- DBI::dbGetQuery(con,
                                 "SELECT shingle FROM vocab ORDER BY pos")$shingle

  vector_cast <- if (engine == "vss") {
    sprintf("::FLOAT[%d]", n_features)
  } else {
    "::FLOAT[]"
  }

  duckdb_create_vector_table(con,
                             input_table = "x_input",
                             shingles_table = "x_shingles",
                             vectors_table = "x_vectors",
                             vector_cast = vector_cast)
  duckdb_create_vector_table(con,
                             input_table = "y_input",
                             shingles_table = "y_shingles",
                             vectors_table = "y_vectors",
                             vector_cast = vector_cast)

  list(colnames = colnames_xy,
       n_features = n_features)
}

duckdb_search_vss <- function(con,
                              distance,
                              n_features,
                              k,
                              k_search,
                              control_duckdb,
                              verbose = FALSE) {

  metric <- switch(distance,
                   "euclidean" = "l2sq",
                   "l2" = "l2sq",
                   "cosine" = "cosine",
                   "ip" = "ip")
  distance_fun <- switch(distance,
                         "euclidean" = "array_distance",
                         "l2" = "array_distance",
                         "cosine" = "array_cosine_distance",
                         "ip" = "array_negative_inner_product")

  DBI::dbExecute(
    con,
    sprintf(
      paste(
        "CREATE INDEX x_vectors_hnsw_idx ON x_vectors USING HNSW (vec)",
        "WITH (metric = '%s', ef_construction = %d, ef_search = %d, M = %d, M0 = %d)"
      ),
      metric,
      as.integer(control_duckdb$vss_ef_construction),
      as.integer(control_duckdb$vss_ef_search),
      as.integer(control_duckdb$vss_M),
      as.integer(control_duckdb$vss_M0)
    )
  )

  can_use_lateral <- identical(as.integer(k), 1L)
  join_mode <- control_duckdb$join_mode
  if (is.null(join_mode)) {
    join_mode <- "auto"
  }

  use_lateral <- FALSE
  index_join_available <- NA
  if (can_use_lateral) {
    if (identical(join_mode, "lateral")) {
      use_lateral <- TRUE
      if (verbose) {
        index_join_available <- duckdb_vss_uses_index_join(con,
                                                           distance_fun = distance_fun,
                                                           k_search = k_search)
      }
    } else if (identical(join_mode, "auto")) {
      index_join_available <- duckdb_vss_uses_index_join(con,
                                                         distance_fun = distance_fun,
                                                         k_search = k_search)
      use_lateral <- index_join_available
    }
  }

  if (verbose) {
    availability <- if (is.na(index_join_available)) {
      "not checked"
    } else if (index_join_available) {
      "TRUE"
    } else {
      "FALSE"
    }

    cat(sprintf("DuckDB VSS HNSW_INDEX_JOIN available: %s\n", availability))
    cat(sprintf("DuckDB VSS HNSW_INDEX_JOIN used: %s\n",
                if (use_lateral && isTRUE(index_join_available)) "TRUE" else "FALSE"))
    cat(sprintf("DuckDB VSS query mode: %s\n",
                if (use_lateral) "lateral" else "classic"))
  }

  if (use_lateral) {
    duckdb_search_vss_lateral(con = con,
                              distance_fun = distance_fun,
                              k_search = k_search)
  } else {
    duckdb_search_vss_classic(con = con,
                              distance_fun = distance_fun,
                              n_features = n_features,
                              k = k,
                              k_search = k_search)
  }
}

duckdb_search_faiss <- function(con,
                                distance,
                                n_features,
                                k,
                                k_search,
                                control_duckdb) {

  metric <- switch(distance,
                   "euclidean" = "L2",
                   "l2" = "L2",
                   "ip" = "INNER_PRODUCT")
  distance_fun <- switch(distance,
                         "euclidean" = "list_distance",
                         "l2" = "list_distance",
                         "ip" = "list_negative_inner_product")

  index_name <- "blocking_faiss_idx"
  faiss_index <- duckdb_escape_string(control_duckdb$faiss_index)

  DBI::dbExecute(
    con,
    sprintf(
      "CALL FAISS_CREATE('%s', %d, '%s', metric_type := '%s')",
      index_name,
      as.integer(n_features),
      faiss_index,
      metric
    )
  )
  DBI::dbExecute(
    con,
    sprintf("CALL FAISS_ADD((SELECT id, vec FROM x_vectors), '%s')",
            index_name)
  )

  effective_rank <- min(as.integer(k), as.integer(k_search)) - 1L
  queries <- DBI::dbGetQuery(con, "SELECT id, vec FROM y_vectors ORDER BY id")

  matches <- lapply(seq_len(nrow(queries)), function(i) {
    query_values <- queries$vec[[i]]
    if (is.character(query_values)) {
      query_values <- strsplit(query_values, ",\\s*")[[1]]
    }
    query_literal <- duckdb_list_literal(query_values)

    res <- DBI::dbGetQuery(
      con,
      sprintf(
        paste(
          "WITH raw AS (",
          "  SELECT UNNEST(FAISS_SEARCH('%s', %d, %s)) AS match",
          ")",
          "SELECT raw.match.label AS x,",
          "       %s(%s, i.vec) AS dist",
          "FROM raw",
          "JOIN x_vectors i ON i.id = raw.match.label",
          "WHERE raw.match.rank = %d"
        ),
        index_name,
        as.integer(k_search),
        query_literal,
        distance_fun,
        query_literal,
        as.integer(effective_rank)
      )
    )

    if (!nrow(res)) {
      return(data.table::data.table(y = integer(),
                                    x = integer(),
                                    dist = numeric()))
    }

    data.table::data.table(y = as.integer(queries$id[[i]]),
                           x = as.integer(res$x[[1]]),
                           dist = as.numeric(res$dist[[1]]))
  })

  data.table::rbindlist(matches)
}

method_duckdb <- function(x,
                          y,
                          k,
                          distance,
                          verbose,
                          n_threads,
                          deduplication,
                          control_txt,
                          control) {
  if (identical(control$duckdb$engine, "faiss")) {
    stop(paste(
      "DuckDB `faiss` is not enabled in this backend yet.",
      "The current DuckDB community extension segfaulted on shingle-derived vectors",
      "during validation; use `control_duckdb(engine = \"vss\")` instead."
    ), call. = FALSE)
  }

  con <- DBI::dbConnect(
    duckdb::duckdb(dbdir = control$duckdb$dbdir),
    array = "matrix"
  )
  on.exit({
    try(DBI::dbDisconnect(con, shutdown = FALSE), silent = TRUE)
  }, add = TRUE)

  DBI::dbExecute(con, sprintf("SET threads = %d", max(1L, as.integer(n_threads))))

  duckdb_load_extension(con, control$duckdb)

  if (verbose) cat(sprintf("DuckDB backend (%s)\n", control$duckdb$engine))

  prepared <- if (is.character(x) && is.character(y)) {
    duckdb_prepare_text_backend(con = con,
                                x = x,
                                y = y,
                                deduplication = deduplication,
                                control_txt = control_txt,
                                control_duckdb = control$duckdb)
  } else if ((is.matrix(x) || inherits(x, "Matrix")) &&
             (is.matrix(y) || inherits(y, "Matrix"))) {
    duckdb_prepare_matrix_backend(con = con,
                                  x = x,
                                  y = y,
                                  deduplication = deduplication,
                                  control_duckdb = control$duckdb)
  } else {
    stop(paste(
      "DuckDB backend supports either character vectors",
      "or matrix/Matrix feature inputs."
    ), call. = FALSE)
  }

  if (!length(prepared$colnames)) {
    return(prepared)
  }

  allowed_distances <- switch(control$duckdb$engine,
                              "vss" = c("euclidean", "l2", "cosine", "ip"),
                              "faiss" = c("euclidean", "l2", "ip"))

  if (!(distance %in% allowed_distances)) {
    stop(sprintf("Distance for DuckDB `%s` should be `%s`",
                 control$duckdb$engine,
                 paste(allowed_distances, collapse = ", ")),
         call. = FALSE)
  }

  k_search <- max(1L,
                  min(length(x),
                      max(as.integer(control$k_search), as.integer(k))))

  result <- switch(
    control$duckdb$engine,
    "vss" = duckdb_search_vss(con = con,
                              distance = distance,
                              n_features = prepared$n_features,
                              k = k,
                              k_search = k_search,
                              control_duckdb = control$duckdb,
                              verbose = verbose),
    "faiss" = duckdb_search_faiss(con = con,
                                  distance = distance,
                                  n_features = prepared$n_features,
                                  k = k,
                                  k_search = k_search,
                                  control_duckdb = control$duckdb)
  )

  list(result = result,
       colnames = prepared$colnames)
}
