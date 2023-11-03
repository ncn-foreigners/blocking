

# vector imputs -----------------------------------------------------------

## duplication -------------------------------------------------------------

### generate data -----------------------------------------------------------

df_example <- data.frame(txt = c(
  "jankowalski",
  "kowalskijan",
  "kowalskimjan",
  "kowaljan",
  "montypython",
  "pythonmonty",
  "cyrkmontypython",
  "monty"
))

#### check arguments ---------------------------------------------------------

expect_silent(
  blocking(x = df_example$txt)
)


#### check functionalities ---------------------------------------------------

##### RcppHNSW ----------------------------------------------------------------

expect_equal(
  blocking(x = df_example$txt,
           ann = "hnsw",
           control_ann = controls_ann(hnsw = list(M = 5, ef_c = 10, ef_s = 10)))$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

##### mlpack ----------------------------------------------------------------

expect_equal(
  blocking(x = df_example$txt, ann = "lsh")$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "kd")$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)


##### RcppAnnoy ----------------------------------------------------------------

expect_equal(
  blocking(x = df_example$txt,
           ann = "annoy",
           distance = "euclidean")$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

## record linkage -------------------------------------------------------------

df_base <- data.frame(txt = c("montypython", "kowalskijan"))

expect_silent(
  blocking(x = df_base$txt, y = df_example$txt)
)


### RcppHNSW ----------------------------------------------------------------

expect_equal(
  blocking(x = df_base$txt,
           y = df_example$txt,
           ann = "hnsw",
           control_ann = controls_ann(hnsw = list(M = 5, ef_s = 10, ef_c = 10))),
  list(result = structure(
    list(x = c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L),
         y = 1:8,
         block = c(1, 1, 1, 1, 2, 2, 2, 2)),
    class = "data.frame", row.names = c(NA, -8L)),
    method = "hnsw",
    metrics = NULL,
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls",
                 "mo", "nt", "ow", "py", "sk", "th", "ty", "wa", "yp", "yt", "on"))
)

### mlpack ----------------------------------------------------------------

expect_equal(
  blocking(x = df_base$txt,
           y = df_example$txt,
           ann = "lsh"),
  list(result = structure(
    list(x = c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L),
         y = 1:8,
         block = c(1, 1, 1, 1, 2, 2, 2, 2)),
    class = "data.frame", row.names = c(NA, -8L)),
    method = "lsh",
    metrics = NULL,
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls",
                 "mo", "nt", "ow", "py", "sk", "th", "ty", "wa", "yp", "yt", "on"))
)

expect_equal(
  blocking(x = df_base$txt,
           y = df_example$txt,
           ann = "kd"),
  list(result = structure(
    list(x = c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L),
         y = 1:8,
         block = c(1, 1, 1, 1, 2, 2, 2, 2)),
    class = "data.frame", row.names = c(NA, -8L)),
    method = "kd",
    metrics = NULL,
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls",
                 "mo", "nt", "ow", "py", "sk", "th", "ty", "wa", "yp", "yt", "on"))
)


### RcppAnnoy ----------------------------------------------------------------

expect_equal(
  blocking(x = df_base$txt,
           y = df_example$txt,
           ann = "annoy",
           distance = "euclidean"),
  list(result = structure(
    list(x = c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L),
         y = 1:8,
         block = c(1, 1, 1, 1, 2, 2, 2, 2)),
    class = "data.frame", row.names = c(NA, -8L)),
    method = "annoy",
    metrics = NULL,
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls",
                 "mo", "nt", "ow", "py", "sk", "th", "ty", "wa", "yp", "yt", "on"))
)


# testing matrix input ----------------------------------------------------

mat_x <- structure(c(0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1,
                     0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 2, 0),
                   dim = c(2L, 19L), dimnames = list(c(`1` = "1", `2` = "2"),
                                     c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls", "mo", "nt",
                                       "ow", "py", "sk", "th", "ty", "wa", "yp", "yt", "on")))

mat_y <- structure(c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
            0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0,
            0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0,
            0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0,
            0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1,
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1,
            0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1,
            0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0,
            0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
            1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 1),
          dim = c(8L, 28L),
          dimnames = list(c(`1` = "1", `2` = "2", `3` = "3", `4` = "4",
                            `5` = "5", `6` = "6", `7` = "7", `8` = "8"),
                          c("cy", "ij", "im", "km", "lj", "mj", "nk", "nm", "rk", "yr", "yp", "ho", "ki", "ls",
                           "py", "sk", "th", "yt", "al", "an", "ja", "ko", "mo", "nt", "ow",
                           "ty", "wa", "on")))



## deduplication -----------------------------------------------------------

### basic

expect_silent(
  blocking(x = mat_y)
)

#### RcppHNSW ----------------------------------------------------------------

expect_equal(
  blocking(x = mat_y,
           ann = "hnsw",
           control_ann = controls_ann(hnsw = list(M = 5, ef_c = 10, ef_s = 10)))$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

##### mlpack ----------------------------------------------------------------

expect_equal(
  blocking(x = mat_y, ann = "lsh")$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

expect_equal(
  blocking(x = mat_y, ann = "kd")$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)


##### RcppAnnoy ----------------------------------------------------------------

expect_equal(
  blocking(x = mat_y,
           ann = "annoy",
           distance = "euclidean")$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)


## record linkage ----------------------------------------------------------



expect_silent(
  blocking(x = mat_x, y = mat_y)
)


### RcppHNSW ----------------------------------------------------------------

expect_equal(
  blocking(x = mat_x,
           y = mat_y,
           ann = "hnsw",
           control_ann = controls_ann(hnsw = list(M = 5, ef_s = 10, ef_c = 10))),
  list(result = structure(
    list(x = c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L),
         y = 1:8,
         block = c(1, 1, 1, 1, 2, 2, 2, 2)),
    class = "data.frame", row.names = c(NA, -8L)),
    method = "hnsw",
    metrics = NULL,
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls",
                 "mo", "nt", "ow", "py", "sk", "th", "ty", "wa", "yp", "yt", "on"))
)

### mlpack ----------------------------------------------------------------

expect_equal(
  blocking(x = mat_x,
           y = mat_y,
           ann = "lsh"),
  list(result = structure(
    list(x = c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L),
         y = 1:8,
         block = c(1, 1, 1, 1, 2, 2, 2, 2)),
    class = "data.frame", row.names = c(NA, -8L)),
    method = "lsh",
    metrics = NULL,
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls",
                 "mo", "nt", "ow", "py", "sk", "th", "ty", "wa", "yp", "yt", "on"))
)

expect_equal(
  blocking(x = mat_x,
           y = mat_y,
           ann = "kd"),
  list(result = structure(
    list(x = c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L),
         y = 1:8,
         block = c(1, 1, 1, 1, 2, 2, 2, 2)),
    class = "data.frame", row.names = c(NA, -8L)),
    method = "kd",
    metrics = NULL,
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls",
                 "mo", "nt", "ow", "py", "sk", "th", "ty", "wa", "yp", "yt", "on"))
)


### RcppAnnoy ----------------------------------------------------------------

expect_equal(
  blocking(x = mat_x,
           y = mat_y,
           ann = "annoy",
           distance = "euclidean"),
  list(result = structure(
    list(x = c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L),
         y = 1:8,
         block = c(1, 1, 1, 1, 2, 2, 2, 2)),
    class = "data.frame", row.names = c(NA, -8L)),
    method = "annoy",
    metrics = NULL,
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls",
                 "mo", "nt", "ow", "py", "sk", "th", "ty", "wa", "yp", "yt", "on"))
)


# testing evaluation matrices ---------------------------------------------

result <- blocking(x = df_example$txt)

expect_silent(
  blocking(x = df_example$txt,
           true_blocks = result$result)
)

expect_error(
  blocking(x = df_example$txt,
           true_blocks = result$result[, c("x", "y")])
)

expect_equal(
  blocking(x = df_example$txt,
           true_blocks = result$result)$metrics,
  c(vi = 0, nmi = 1, split.join = 0, rand = 1, adjusted.rand = 1)
)



# reclin2 testing ---------------------------------------------------------


