source("test_data.R")

if (!(requireNamespace("DBI", quietly = TRUE) &&
      requireNamespace("duckdb", quietly = TRUE))) {
  tinytest::exit_file("DuckDB packages not installed")
}

vss_ok <- try({
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "INSTALL vss")
  DBI::dbExecute(con, "LOAD vss")
  TRUE
}, silent = TRUE)

if (inherits(vss_ok, "try-error")) {
  tinytest::exit_file("DuckDB `vss` extension is unavailable")
}

expect_equal(
  blocking(
    x = df_example$txt,
    ann = "duckdb"
  )$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(
    x = df_base$txt,
    y = df_example$txt,
    ann = "duckdb"
  )$result$block,
  c(rep(2, 4), rep(1, 4))
)

expect_error(
  blocking(
    x = df_example$txt,
    ann = "duckdb",
    distance = "euclidean",
    control_ann = controls_ann(
      duckdb = control_duckdb(engine = "faiss")
    )
  )
)
