source("test_data.R")

expect_equal(
  blocking(x = df_example$txt,
           ann = "hnsw",
           control_ann = controls_ann(hnsw = list(M = 5, ef_c = 10, ef_s = 10)))$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)


expect_equal(
  blocking(x = df_base$txt,
           y = df_example$txt,
           ann = "hnsw",
           control_ann = controls_ann(hnsw = list(M = 5, ef_s = 10, ef_c = 10))),
  structure(list(result = structure(
    list(x = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
         y = c(5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L),
         block = c(2, 2, 2, 2, 1, 1, 1, 1)),
    row.names = c(NA, -8L),
    class = c("data.table", "data.frame")),
    method = "hnsw",
    metrics = NULL,
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls", "mo", "nt", "ow", "py",
                 "sk", "th", "ty", "wa", "yp", "yt", "on"),
    graph = NULL),
    class = "blocking")
)

expect_equal(
  blocking(x = mat_y,
           ann = "hnsw",
           control_ann = controls_ann(hnsw = list(M = 5, ef_c = 10, ef_s = 10)))$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)


expect_equal(
  blocking(x = mat_x,
           y = mat_y,
           ann = "hnsw",
           control_ann = controls_ann(hnsw = list(M = 5, ef_s = 10, ef_c = 10))),
  structure(list(result = structure(
    list(x = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
         y = c(5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L),
         block = c(2, 2, 2, 2, 1, 1, 1, 1)),
    row.names = c(NA, -8L),
    class = c("data.table", "data.frame")),
    method = "hnsw",
    metrics = NULL,
    colnames = c("al", "an","ho", "ij", "ja", "ki", "ko", "ls", "mo", "nt", "ow", "py",
                 "sk", "th", "ty", "wa", "yp", "yt", "on"),
    graph = NULL),
    class = "blocking")
)
