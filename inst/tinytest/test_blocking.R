

# duplication -------------------------------------------------------------

## generate data -----------------------------------------------------------

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

### check arguments ---------------------------------------------------------

expect_silent(
  blocking(x = df_example$txt)
)


### check functionalities ---------------------------------------------------

#### RcppHNSW ----------------------------------------------------------------

expect_equal(
  blocking(x = df_example$txt,
           ann = "hnsw",
           control_ann = controls_ann(hnsw = list(M = 5, ef_c = 10, ef_s = 10)))$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

#### mlpack ----------------------------------------------------------------

expect_equal(
  blocking(x = df_example$txt, ann = "lsh")$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "kd")$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)


#### RcppAnnoy ----------------------------------------------------------------

expect_equal(
  blocking(x = df_example$txt,
           ann = "annoy",
           distance = "euclidean")$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

# record linkage -------------------------------------------------------------

df_base <- data.frame(true = c("montypython", "kowalskijan"))

expect_silent(
  blocking(x = df_base$true, y = df_example$txt)
)


#### RcppHNSW ----------------------------------------------------------------

expect_equal(
  blocking(x = df_base$true,
           y = df_example$txt,
           ann = "hnsw",
           control_ann = controls_ann(hnsw = list(M = 5, ef_s = 10, ef_c = 10))),
  list(result = structure(
    list(x = c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L),
         y = 1:8,
         block = c(1, 1, 1, 1, 2, 2, 2, 2)),
    class = "data.frame", row.names = c(NA, -8L)),
    method = "hnsw",
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls",
                 "mo", "nt", "ow", "py", "sk", "th", "ty", "wa", "yp", "yt", "on"))
)

#### mlpack ----------------------------------------------------------------

expect_equal(
  blocking(x = df_base$true,
           y = df_example$txt,
           ann = "lsh"),
  list(result = structure(
    list(x = c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L),
         y = 1:8,
         block = c(1, 1, 1, 1, 2, 2, 2, 2)),
    class = "data.frame", row.names = c(NA, -8L)),
    method = "lsh",
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls",
                 "mo", "nt", "ow", "py", "sk", "th", "ty", "wa", "yp", "yt", "on"))
)

expect_equal(
  blocking(x = df_base$true,
           y = df_example$txt,
           ann = "kd"),
  list(result = structure(
    list(x = c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L),
         y = 1:8,
         block = c(1, 1, 1, 1, 2, 2, 2, 2)),
    class = "data.frame", row.names = c(NA, -8L)),
    method = "kd",
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls",
                 "mo", "nt", "ow", "py", "sk", "th", "ty", "wa", "yp", "yt", "on"))
)


#### RcppAnnoy ----------------------------------------------------------------

expect_equal(
  blocking(x = df_base$true,
           y = df_example$txt,
           ann = "annoy",
           distance = "euclidean"),
  list(result = structure(
    list(x = c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L),
         y = 1:8,
         block = c(1, 1, 1, 1, 2, 2, 2, 2)),
    class = "data.frame", row.names = c(NA, -8L)),
    method = "annoy",
    colnames = c("al", "an", "ho", "ij", "ja", "ki", "ko", "ls",
                 "mo", "nt", "ow", "py", "sk", "th", "ty", "wa", "yp", "yt", "on"))
)
