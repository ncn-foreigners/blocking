# testing evaluation matrices ---------------------------------------------

result <- blocking(x = df_example$txt)

expect_silent(
  blocking(x = df_example$txt, true_blocks = data.frame(x = 1:8, block = rep(1:2, each=4)))
)

expect_equal(
  blocking(x = df_example$txt,
           true_blocks = data.frame(x = 1:8, block = rep(1:2, each=4)))$metrics,
  c(recall = 1, precision = 1, fpr = 0, fnr = 0, accuracy = 1,
    specificity = 1)
)


