source("test_data.R")

expect_silent(
  blocking(x = df_example$txt)
)


expect_equal(
  blocking(x = df_example$txt, ann = "lsh")$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

expect_equal(
  blocking(x = df_example$txt, ann = "kd")$result$block,
  c(1, 1, 1, 1, 2, 2, 2, 2)
)


expect_silent(
  blocking(x = df_base$txt, y = df_example$txt)
)


expect_silent(
  blocking(x = mat_y)
)


expect_silent(
  blocking(x = mat_x, y = mat_y)
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
  c(vi = 0, nmi = 1, split.join = 0, rand = 1, adjusted.rand = 1,
    recall = 1, precision = 1, fpr = 0, fnr = 0, accuracy = 1, specificity = 1
  )
)

# check if true_block is a vector

expect_silent(
  blocking(x = df_example$txt,
           true_blocks = result$result$block)
)


