source("test_data.R")

set.seed(2024)

expect_equal(
  blocking(x = df_example$txt,
           ann = "annoy",
           distance = "euclidean")$result$block,
  c(1, 1, 1, 2, 2, 2)
)


expect_equal(
  blocking(x = mat_x,
           y = mat_y,
           ann = "annoy",
           seed = 2023,
           distance = "euclidean"),
  structure(list(result = structure(
    list(x = c(1, 1, 1, 2, 2, 2, 2, 3),
         y = c(5L, 6L, 7L, 1L, 2L, 3L, 4L, 8L),
         block = c(2, 2, 2, 1, 1, 1, 1, 3),
         dist = c(0, 1, 0, 1, 0, 1, 2, 2.236068)),
    row.names = c(NA, -8L),
    class = c("data.table", "data.frame")),
    method = "annoy",
    deduplication = FALSE,
    representation = "custom_matrix",
    metrics = NULL,
    confusion = NULL,
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls", "mo",
                 "nt", "ow", "py", "sk", "ty", "wa", "yp", "yt", "on", "th"),
    graph = NULL),
    class = "blocking")
)




### RcppAnnoy ----------------------------------------------------------------

expect_equal(
  blocking(x = df_base$txt,
           y = df_example$txt,
           ann = "annoy",
           distance = "euclidean"),
  structure(list(result = structure(
    list(x = c(1, 1, 1, 2, 2, 2, 2, 3),
         y = c(5L, 6L, 7L, 1L, 2L, 3L, 4L, 8L),
         block = c(2, 2, 2, 1, 1, 1, 1, 3),
         dist = c(0, 1, 0, 1, 0, 1, 2, 2)),
    row.names = c(NA, -8L),
    class = c("data.table", "data.frame")),
    method = "annoy",
    deduplication = FALSE,
    representation = "shingles",
    metrics = NULL,
    confusion = NULL,
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls", "mo", "ow",
                 "py", "sk", "ty", "wa", "yp", "yt", "nt", "on", "th"),
    graph = NULL),
    class = "blocking")
)


expect_equal(
  blocking(x = mat_y,
           ann = "annoy",
           distance = "euclidean")$result$block,
  c(1, 1, 1, 2, 2, 2)
)


## file saving
# expect_error(
#   blocking(x = mat_y,
#            ann = "annoy",
#            distance = "euclidean",
#            ann_write = "./plik")
#   )


# expect_true({
#   tmp_dir <- tempdir()
#   blocking(x = mat_y,
#            ann = "annoy",
#            distance = "euclidean",
#            ann_write = file.path(tmp_dir))
#   file.exists(file.path(tmp_dir, "index.annoy")) &
#     file.exists(file.path(tmp_dir, "index-colnames.txt"))
# })

# expect_true({
#   tmp_dir <- tempdir()
#   sub_dir <- file.path(tmp_dir, "sub")
#   dir.create(sub_dir, showWarnings = FALSE)
#   blocking(x = mat_y,
#            ann = "annoy",
#            distance = "euclidean",
#            ann_write = file.path(sub_dir))
#   file.exists(file.path(sub_dir, "index.annoy")) &
#     file.exists(file.path(sub_dir, "index-colnames.txt"))
# })

## testing reading saved index
# expect_equal({
#   ncols <- length(readLines(file.path(tmp_dir, "index-colnames.txt")))
#   ann_annoy <- methods::new(RcppAnnoy::AnnoyEuclidean, ncols)
#   ann_annoy$load(file.path(tmp_dir, "index.annoy"))
#   ann_annoy$getNItems()
# },  8)

## test verbose
expect_stdout(
  blocking(x = mat_y,
         ann = "annoy",
         distance = "euclidean",
         verbose = 2)
)

