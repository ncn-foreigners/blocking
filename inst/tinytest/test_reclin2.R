source("test_data.R")

expect_silent(
  pair_ann(x = df_example, on = "txt")
)

expect_equal(
  dim(pair_ann(x = df_example, on = "txt")),
  c(8, 3)
)

expect_equal(
  class(pair_ann(x = df_example, on = "txt")),
  c("pairs", "data.table", "data.frame")
)

df_on_multi <- data.frame(a = c("a", "a", "x", "x", "solo"),
                          b = c("a", "b", "x", "y", ""))
df_on_direct <- data.frame(txt = paste0(df_on_multi$a, df_on_multi$b))

pair_on_multi <- pair_ann(x = df_on_multi,
                          on = c("a", "b"),
                          seed = 2024,
                          control_txt = controls_txt(n_shingles = 1L))
pair_on_direct <- pair_ann(x = df_on_direct,
                           on = "txt",
                           seed = 2024,
                           control_txt = controls_txt(n_shingles = 1L))

expect_equal(
  data.frame(.x = pair_on_multi$.x,
             .y = pair_on_multi$.y,
             block = pair_on_multi$block),
  data.frame(.x = pair_on_direct$.x,
             .y = pair_on_direct$.y,
             block = pair_on_direct$block)
)

expect_equal(
  attr(pair_on_multi, "blocking_on"),
  c("a", "b")
)

expect_false(
  "txt" %in% names(attr(pair_on_multi, "x"))
)

df_on_blocking <- data.frame(txt = df_example$txt,
                             group = rep(c("person", "python"), each = 4))

pair_on_blocking <- pair_ann(x = df_on_blocking,
                             on = "txt",
                             on_blocking = "group")

expect_true(
  all(df_on_blocking$group[pair_on_blocking$.x] ==
        df_on_blocking$group[pair_on_blocking$.y])
)

x_on_blocking <- data.frame(txt = c("aa", "xx"),
                            group = c("a", "b"))
y_on_blocking <- data.frame(txt = c("ab", "xy", "solo"),
                            group = c("a", "b", "c"))

pair_on_blocking_link <- pair_ann(x = x_on_blocking,
                                  y = y_on_blocking,
                                  on = "txt",
                                  on_blocking = "group",
                                  control_txt = controls_txt(n_shingles = 1L))

expect_equal(
  nrow(pair_on_blocking_link),
  2L
)

expect_true(
  all(x_on_blocking$group[pair_on_blocking_link$.x] ==
        y_on_blocking$group[pair_on_blocking_link$.y])
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
