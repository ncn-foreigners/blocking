

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
           control_ann = controls_ann(hnsw = list(M = 5, ef_c = 10, ef_s = 10)))$x,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

#### mlpack ----------------------------------------------------------------

expect_equal(
  blocking(x = df_example$txt, ann = "lsh")$x,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "kd")$x,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)


#### RcppAnnoy ----------------------------------------------------------------

expect_equal(
  blocking(x = df_example$txt,
           ann = "annoy",
           distance = "euclidean")$x,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

# record linkage -------------------------------------------------------------

df_base <- data.frame(true = c("kowalskijan", "montypython"))

expect_silent(
  blocking(x = df_base$true, y = df_example$txt)
)


#### RcppHNSW ----------------------------------------------------------------

expect_equal(
  blocking(x = df_base$true,
           y = df_example$txt,
           ann = "hnsw",
           control_ann = controls_ann(hnsw = list(M = 5, ef_s = 10, ef_c = 10))),
  list(x = c(1, 2),
       y = c(1, 1, 1, 1, 2, 2, 2, 2),
       method = "hnsw",
       colnames = c("cy", "im", "km", "lj", "mj", "nk", "nm", "rk",
                    "yr", "ij", "yp", "ho", "ki", "ls", "py", "sk", "th", "yt",
                    "al", "an", "ja", "ko", "mo", "nt", "ow", "ty", "wa", "on"
       ))
)

#### mlpack ----------------------------------------------------------------

expect_equal(
  blocking(x = df_base$true,
           y = df_example$txt,
           ann = "lsh"),
  list(x = c(1, 2),
       y = c(1, 1, 1, 1, 2, 2, 2, 2),
       method = "lsh",
       colnames = c("cy", "im", "km", "lj", "mj", "nk", "nm", "rk",
                    "yr", "ij", "yp", "ho", "ki", "ls", "py", "sk", "th", "yt",
                    "al", "an", "ja", "ko", "mo", "nt", "ow", "ty", "wa", "on"
       ))
)

expect_equal(
  blocking(x = df_base$true,
           y = df_example$txt,
           ann = "kd"),
  list(x = c(1, 2),
       y = c(1, 1, 1, 1, 2, 2, 2, 2),
       method = "kd",
       colnames = c("cy", "im", "km", "lj", "mj", "nk", "nm", "rk",
                    "yr", "ij", "yp", "ho", "ki", "ls", "py", "sk", "th", "yt",
                    "al", "an", "ja", "ko", "mo", "nt", "ow", "ty", "wa", "on"
       ))
)


#### RcppAnnoy ----------------------------------------------------------------

expect_equal(
  blocking(x = df_base$true,
           y = df_example$txt,
           ann = "annoy",
           distance = "euclidean"),
  list(x = c(1, 2),
       y = c(1, 1, 1, 1, 2, 2, 2, 2),
       method = "annoy",
       colnames = c("cy", "im", "km", "lj", "mj", "nk", "nm", "rk",
                    "yr", "ij", "yp", "ho", "ki", "ls", "py", "sk", "th", "yt",
                    "al", "an", "ja", "ko", "mo", "nt", "ow", "ty", "wa", "on"
       ))
)
