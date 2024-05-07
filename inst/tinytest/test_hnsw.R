source("test_data.R")

expect_equal(
  blocking(x = df_example$txt,
           ann = "hnsw",
           control_ann = controls_ann(hnsw = list(M = 5, ef_c = 10, ef_s = 10)))$result$block,
  c(1, 1, 1, 2, 2, 2)
)


expect_equal(
  blocking(x = df_base$txt,
           y = df_example$txt,
           ann = "hnsw",
           control_ann = controls_ann(hnsw = list(M = 5, ef_s = 10, ef_c = 10))),
  structure(list(result = structure(
    list(x = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
         y = c(5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L),
         block = c(2, 2, 2, 2, 1, 1, 1, 1),
         dist = c(1.19209289550781e-07, 0.0425729155540466,
                  1.19209289550781e-07, 0.278312206268311, 0.0513166785240173,
                  -1.19209289550781e-07, 0.0513166785240173, 0.225403368473053)),
    row.names = c(NA, -8L),
    class = c("data.table", "data.frame")),
    method = "hnsw",
    deduplication = FALSE,
    metrics = NULL,
    confusion = NULL,
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls", "mo", "ow",
                 "py", "sk", "ty", "wa", "yp", "yt", "nt", "on", "th"),
    graph = NULL),
    class = "blocking")
)

expect_equal(
  blocking(x = mat_y,
           ann = "hnsw",
           control_ann = controls_ann(hnsw = list(M = 5, ef_c = 10, ef_s = 10)))$result$block,
  c(1, 1, 1, 2, 2, 2)
)


expect_equal(
  blocking(x = mat_x,
           y = mat_y,
           ann = "hnsw",
           control_ann = controls_ann(hnsw = list(M = 5, ef_s = 10, ef_c = 10))),
  structure(list(result = structure(
    list(x = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
         y = c(5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L),
         block = c(2, 2, 2, 2, 1, 1, 1, 1),
         dist = c(1.19209289550781e-07, 0.0425729155540466,
                  1.19209289550781e-07, 0.278312206268311, 0.0513166785240173,
                  -1.19209289550781e-07, 0.0513166785240173, 0.225403368473053)),
    row.names = c(NA, -8L),
    class = c("data.table", "data.frame")),
    method = "hnsw",
    deduplication = FALSE,
    metrics = NULL,
    confusion = NULL,
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls", "mo", "nt",
                 "ow", "py", "sk", "ty", "wa", "yp", "yt", "on", "th"),
    graph = NULL),
    class = "blocking")
)


## testing saving

expect_error({
  blocking(x = mat_y,
           ann = "hnsw",
           ann_write = "./plik")
})

expect_true({
  blocking(x = mat_y,
           ann = "hnsw",
           ann_write = ".")
  file.exists("./index.hnsw") &
    file.exists("./index-colnames.txt")
})

expect_true({
  blocking(x = mat_y,
           ann = "hnsw",
           ann_write = "./")
  file.exists("./index.hnsw") &
    file.exists("./index-colnames.txt")
})


expect_equal({
  ncols <- length(readLines("./index-colnames.txt"))
  ann_hnsw <- methods::new(RcppHNSW::HnswCosine, ncols, "./index.hnsw")
  ann_hnsw$size()
},  8)


## check verbose
expect_stdout(
  blocking(x = mat_y,
           ann = "hnsw",
           verbose = 2)
)

expect_stdout(
  blocking(x = mat_y,
           ann = "hnsw",
           verbose = 2,
           ann_write = ".")
)


### checks sparse data

expect_silent(
  blocking(x = df_example$txt,
           ann = "hnsw",
           control_ann = controls_ann(sparse=TRUE))
)

expect_silent(
  blocking(x = Matrix::Matrix(mat_y),
           ann = "hnsw",
           control_ann = controls_ann(sparse=TRUE))
)

