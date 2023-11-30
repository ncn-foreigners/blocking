source("test_data.R")

expect_silent(
  pair_ann(x = df_example, on = "txt", ann = "hnsw")
)

expect_equal(
  dim(pair_ann(x = df_example, on = "txt")),
  c(10, 3)
)

expect_equal(
  class(pair_ann(x = df_example, on = "txt")),
  c("pairs", "data.table", "data.frame")
)

expect_silent(
  pair_ann(x = df_example, on = "txt") |>
    compare_pairs(on = "txt", comparators = list(cmp_jarowinkler())) |>
    score_simple("score", on = "txt") |>
    select_threshold("threshold", score = "score", threshold = 0.55) |>
    link(selection = "threshold")
)

expect_silent(
  pair_ann(x = df_base, y = df_example, on = "txt", deduplication = FALSE) |>
    compare_pairs(on = "txt", comparators = list(cmp_jarowinkler())) |>
    score_simple("score", on = "txt") |>
    select_threshold("threshold", score = "score", threshold = 0.55) |>
    link(selection = "threshold")
)

