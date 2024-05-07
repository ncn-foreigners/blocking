source("test_data.R")

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
         dist = c(0, 1, 0, 1, 0, 1, 4, 5)),
    row.names = c(NA, -8L),
    class = c("data.table", "data.frame")),
    method = "annoy",
    deduplication = FALSE,
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
         dist = c(0, 1, 0, 1, 0, 1, 4, 4)),
    row.names = c(NA, -8L),
    class = c("data.table", "data.frame")),
    method = "annoy",
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
           ann = "annoy",
           distance = "euclidean")$result$block,
  c(1, 1, 1, 2, 2, 2)
)


## file saving
expect_error(
  blocking(x = mat_y,
           ann = "annoy",
           distance = "euclidean",
           ann_write = "./plik")
  )


expect_true({
  blocking(x = mat_y,
           ann = "annoy",
           distance = "euclidean",
           ann_write = ".")
  file.exists("./index.annoy") &
    file.exists("./index-colnames.txt")
})

expect_true({
  blocking(x = mat_y,
           ann = "annoy",
           distance = "euclidean",
           ann_write = "./")
  file.exists("./index.annoy") &
    file.exists("./index-colnames.txt")
})

## testing reading saved index
expect_equal({
  ncols <- length(readLines("./index-colnames.txt"))
  ann_annoy <- methods::new(RcppAnnoy::AnnoyManhattan, ncols)
  ann_annoy$load("./index.annoy")
  ann_annoy$getNItems()
},  8)

## test verbose
expect_stdout(
  blocking(x = mat_y,
         ann = "annoy",
         distance = "euclidean",
         verbose = 2)
)

