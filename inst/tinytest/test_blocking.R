source("test_data.R")

expect_silent(
  blocking(x = df_example$txt)
)


expect_equal(
  blocking(x = df_example$txt)$result$block,
  c(1, 1, 1, 2, 2, 2)
)


expect_equal(
  blocking(x = df_example$txt, ann = "hnsw")$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "annoy")$result$block,
  c(1, 1, 1, 2, 2, 2)
)


expect_equal(
  blocking(x = df_example$txt, ann = "lsh")$result$block,
  c(1, 1, 1, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "kd")$result$block,
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
  blocking(x = df_base$txt, y = df_example$txt, ann = "hnsw")$result$block,
  c(rep(2,4),rep(1,4))
)

expect_equal(
  blocking(x = df_base$txt, y = df_example$txt, ann = "annoy")$result$block,
  c(rep(2,4),rep(1,4))
)

expect_equal(
  blocking(x = df_base$txt, y = df_example$txt, ann = "lsh")$result$block,
  c(rep(2,3),rep(1,4), 3)
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


# testing evaluation matrices ---------------------------------------------

result <- blocking(x = df_example$txt)

expect_silent(
  blocking(x = df_example$txt,
           true_blocks = result$result[, c("x", "y", "block")])
)

expect_error(
  blocking(x = df_example$txt,
           true_blocks = result$result)
)

expect_equal(
  blocking(x = df_example$txt,
           true_blocks = result$result[, c("x", "y", "block")])$metrics,
  c(recall = 1, precision = 1, fpr = 0, fnr = 0, accuracy = 1, specificity = 1)
)

# check if true_block is a vector

# expect_silent(
#   blocking(x = df_example$txt,
#            #true_blocks = result$result$block)
#            true_blocks = result$result[, c("x", "y", "block")])
# )


## printing

expect_silent(
  print(blocking(x = df_example$txt))
)
