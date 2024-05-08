# testing evaluation matrices ---------------------------------------------

result <- blocking(x = df_example$txt)

# expect_silent(
#   blocking(x = df_example$txt,
#            true_blocks = result$result[, c("x", "y", "block")])
# )
#
# expect_error(
#   blocking(x = df_example$txt,
#            true_blocks = result$result)
# )

# expect_equal(
#   blocking(x = df_example$txt,
#            true_blocks = result$result[, c("x", "y", "block")])$metrics,
#   c(recall = 1, precision = 1, fpr = 0, fnr = 0, accuracy = 1, specificity = 1)
# )

# check if true_block is a vector

# expect_silent(
#   blocking(x = df_example$txt,
#            #true_blocks = result$result$block)
#            true_blocks = result$result[, c("x", "y", "block")])
# )
