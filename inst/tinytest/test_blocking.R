source("test_data.R")

set.seed(2024)

expect_silent(
  blocking(x = df_example$txt)
)

expect_equal(
  blocking(x = df_example$txt)$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, distance = "euclidean")$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, distance = "manhatan")$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, distance = "hamming")$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "hnsw")$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "hnsw", distance = "l2")$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "hnsw", distance = "ip")$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "hnsw", distance = "euclidean")$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "annoy")$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "annoy", distance = "euclidean")$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "annoy", distance = "manhatan")$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "annoy", distance = "hamming")$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "kd")$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "lsh")$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_silent(
  blocking(x = df_base$txt, y = df_example$txt)
)

expect_equal(
  blocking(x = df_base$txt, y = df_example$txt)$result$block,
  c(rep(2,4),rep(1,4))
)

expect_equal(
  blocking(x = df_base$txt, y = df_example$txt, distance = "euclidean")$result$block,
  c(rep(2,3),rep(1,4),3)
)

expect_equal(
  blocking(x = df_base$txt, y = df_example$txt, distance = "manhatan")$result$block,
  c(rep(2,3),rep(1,4),3)
)

expect_equal(
  blocking(x = df_base$txt, y = df_example$txt, distance = "hamming")$result$block,
  c(rep(2,3),rep(1,4),3)
)

expect_equal(
  blocking(x = df_base$txt, y = df_example$txt, ann = "hnsw")$result$block,
  c(rep(2,4),rep(1,4))
)

expect_equal(
  blocking(x = df_base$txt, y = df_example$txt, ann = "hnsw", distance = "l2")$result$block,
  c(rep(2,3),rep(1,4),3)
)

expect_equal(
  blocking(x = df_base$txt, y = df_example$txt, ann = "hnsw", distance = "euclidean")$result$block,
  c(rep(2,3),rep(1,4),3)
)

expect_equal(
  blocking(x = df_base$txt, y = df_example$txt, ann = "hnsw", distance = "ip")$result$block,
  c(rep(2,4),rep(1,4))
)

expect_equal(
  blocking(x = df_base$txt, y = df_example$txt, ann = "annoy")$result$block,
  c(rep(2,4),rep(1,4))
)

expect_equal(
  blocking(x = df_base$txt, y = df_example$txt, ann = "annoy", distance = "euclidean")$result$block,
  c(rep(2,3),rep(1,4),3)
)

expect_equal(
  blocking(x = df_base$txt, y = df_example$txt, ann = "annoy", distance = "hamming")$result$block,
  c(rep(2,3),rep(1,4),3)
)

expect_equal(
  blocking(x = df_base$txt, y = df_example$txt, ann = "annoy", distance = "manhatan")$result$block,
  c(rep(2,3),rep(1,4),3)
)

expect_silent(
  blocking(x = mat_y)
)


expect_silent(
  blocking(x = mat_x, y = mat_y)
)


# test parameters ---------------------------------------------------------

expect_error(
  blocking(x = df_example$txt, ann = "hnsw", distance = "manhatan")
)

expect_error(
  blocking(x = df_example$txt, ann = "annoy", distance = "cosine")
)


## printing

expect_silent(
  print(blocking(x = df_example$txt))
)
