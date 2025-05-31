source("test_data.R")

expect_equal(
  blocking(x = df_base$txt,
           y = df_example$txt,
           ann = "lsh"),
  structure(list(result = structure(list(
    x = c(1, 1, 1, 2, 2, 2, 2, 3),
    y = c(5L, 6L, 7L, 1L, 2L, 3L, 4L, 8L),
    block = c(2, 2, 2, 1, 1, 1, 1, 3),
    dist = c(0, 1, 0, 1, 0, 1, 2, 2)),
    row.names = c(NA, -8L),
    class = c("data.table", "data.frame")),
    method = "lsh",
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
  blocking(x = df_base$txt,
           y = df_example$txt,
           ann = "kd"),
  structure(list(result = structure(
    list(x = c(1, 1, 1, 2, 2, 2, 2, 3),
         y = c(5L, 6L, 7L, 1L, 2L, 3L, 4L, 8L),
         block = c(2, 2, 2, 1, 1, 1, 1, 3),
         dist = c(0, 1, 0, 1, 0, 1, 2, 2)),
    row.names = c(NA, -8L),
    class = c("data.table", "data.frame")),
    method = "kd",
    deduplication = FALSE,
    representation = "shingles",
    metrics = NULL,
    confusion = NULL,
    colnames =c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls", "mo", "ow",
                "py", "sk", "ty", "wa", "yp", "yt", "nt", "on", "th"),
    graph = NULL),
    class = "blocking")
)


# expect_equal(
#   blocking(x = mat_y, ann = "lsh")$result$block,
#   c(1, 1, 1, 2, 2, 2)
# )
#
# expect_equal(
#   blocking(x = mat_y, ann = "kd")$result$block,
#   c(1, 1, 1, 2, 2, 2)
# )


# expect_equal(
#   blocking(x = mat_x,
#            y = mat_y,
#            ann = "lsh",
#            seed = 2023),
#   structure(list(result = structure(
#     list(x = c(1, 1, 1, 2, 2, 2, 2, 3),
#          y = c(5L, 6L, 7L, 1L, 2L, 3L, 4L, 8L),
#          block = c(2, 2, 2, 1, 1, 1, 1, 3),
#          dist = c(0, 1, 0, 1, 0, 1, 2, 2.236068)),
#     row.names = c(NA, -8L),
#     class = c("data.table", "data.frame")),
#     method = "lsh",
#     deduplication = FALSE,
#     metrics = NULL,
#     confusion = NULL,
#     colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls", "mo", "nt", "ow",
#                  "py", "sk", "ty", "wa", "yp", "yt", "on", "th"),
#     graph = NULL),
#     class = "blocking")
# )

expect_equal(
  blocking(x = mat_x,
           y = mat_y,
           ann = "kd",
           seed = 2023),
  structure(list(result = structure(
    list(x = c(1, 1, 1, 2, 2, 2, 2, 3),
         y = c(5L, 6L, 7L, 1L, 2L, 3L, 4L, 8L),
         block = c(2, 2, 2, 1, 1, 1, 1, 3),
         dist = c(0, 1, 0, 1, 0, 1, 2, 2.236068)),
    row.names = c(NA, -8L),
    class = c("data.table", "data.frame")),
    method = "kd",
    deduplication = FALSE,
    representation = "shingles",
    metrics = NULL,
    confusion = NULL,
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls", "mo", "nt", "ow",
                 "py", "sk", "ty", "wa", "yp", "yt", "on", "th"),
    graph = NULL),
    class = "blocking")
)


## test verbose
expect_stdout(
  blocking(x = mat_y,
           ann = "lsh",
           verbose = 1)
)


