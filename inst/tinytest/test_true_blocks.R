# testing evaluation matrices ---------------------------------------------

options(text2vec.mc.cores = 1L)

mat_example <- matrix(
  c(1, 0,
    1, 0.01,
    0.99, -0.01,
    1.01, 0.02,
    0, 1,
    0.01, 1,
    -0.01, 0.99,
    0.02, 1.01),
  ncol = 2,
  byrow = TRUE
)

mat_base <- matrix(
  c(1, 0,
    0, 1,
    -1, 0),
  ncol = 2,
  byrow = TRUE
)

mat_link <- matrix(
  c(1, 0.01,
    0.99, -0.01,
    1.01, 0.02,
    0.98, -0.02,
    0.01, 1,
    -0.01, 0.99,
    0.02, 1.01,
    -1, 0.01),
  ncol = 2,
  byrow = TRUE
)

dedup_true_blocks <- data.frame(x = 1:8, block = rep(1:2, each = 4))
link_true_blocks <- data.frame(
  x = c(rep(1L, 4), rep(2L, 3), 3L),
  y = 1:8,
  block = c(rep(1L, 4), rep(2L, 3), 3L)
)

expected_metrics <- c(recall = 1, precision = 1, fpr = 0, fnr = 0,
                      accuracy = 1, specificity = 1, f1_score = 1)

expect_silent(
  blocking(x = mat_example, true_blocks = dedup_true_blocks)
)

expect_equal(
  blocking(x = mat_example, true_blocks = dedup_true_blocks)$metrics,
  expected_metrics
)

expect_equal(
  blocking(x = mat_example, true_blocks = dedup_true_blocks)$confusion,
  matrix(c(12, 0, 0, 16),
         nrow = 2,
         dimnames = list(c("Actual Positive", "Actual Negative"),
                         c("Predicted Positive", "Predicted Negative")))
)

expect_silent(
  blocking(x = mat_base,
           y = mat_link,
           deduplication = FALSE,
           true_blocks = link_true_blocks)
)

expect_equal(
  blocking(x = mat_base,
           y = mat_link,
           deduplication = FALSE,
           true_blocks = link_true_blocks)$metrics,
  expected_metrics
)

expect_equal(
  blocking(x = mat_base,
           y = mat_link,
           deduplication = FALSE,
           true_blocks = link_true_blocks)$confusion,
  matrix(c(8, 0, 0, 16),
         nrow = 2,
         dimnames = list(c("Actual Positive", "Actual Negative"),
                         c("Predicted Positive", "Predicted Negative")))
)

expect_error(
  blocking(x = mat_example,
           true_blocks = data.frame(x = 1:8))
)

expect_error(
  blocking(x = mat_base,
           y = mat_link,
           deduplication = FALSE,
           true_blocks = data.frame(x = 1:8, block = rep(1:2, each = 4)))
)

