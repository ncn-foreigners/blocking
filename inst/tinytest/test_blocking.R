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

expect_equal(
  blocking(x = df_example, on = "txt")$result,
  blocking(x = df_example$txt)$result
)

expect_equal(
  blocking(x = df_base, y = df_example, on = "txt")$result,
  blocking(x = df_base$txt, y = df_example$txt)$result
)

df_on_multi <- data.frame(a = c("a", "a", "x", "x", "solo"),
                          b = c("a", "b", "x", "y", ""))

expect_equal(
  blocking(x = df_on_multi,
           on = c("a", "b"),
           seed = 2024,
           control_txt = controls_txt(n_shingles = 1L))$result,
  blocking(x = paste0(df_on_multi$a, df_on_multi$b),
           seed = 2024,
           control_txt = controls_txt(n_shingles = 1L))$result
)

ann_input_missing <- getFromNamespace(".blocking_ann_input", "blocking")(
  data.table::data.table(a = c("aa", NA), b = c(NA, "bb")),
  c("a", "b")
)

expect_equal(
  ann_input_missing,
  c("aa", "bb")
)

expect_false(
  any(grepl("NA", ann_input_missing))
)

df_on_blocking <- data.frame(
  txt = c(df_example$txt, "jan"),
  group = c(rep("person", 4), rep("python", 4), "single"),
  group2 = c(rep(c("a", "b"), each = 2), rep(c("a", "b"), each = 2), "a")
)

result_on_blocking <- blocking(x = df_on_blocking,
                               on = "txt",
                               on_blocking = "group")

expect_equal(
  names(result_on_blocking$result),
  c("x", "y", "block", "dist")
)

expect_true(
  all(df_on_blocking$group[result_on_blocking$result$x] ==
        df_on_blocking$group[result_on_blocking$result$y])
)

expect_true(
  !any(9L %in% c(result_on_blocking$result$x, result_on_blocking$result$y))
)

df_on_blocking$txt1 <- substr(df_on_blocking$txt, 1, 4)
df_on_blocking$txt2 <- substring(df_on_blocking$txt, 5)

result_on_blocking_on_multi <- blocking(x = df_on_blocking,
                                        on = c("txt1", "txt2"),
                                        on_blocking = "group")

expect_true(
  all(df_on_blocking$group[result_on_blocking_on_multi$result$x] ==
        df_on_blocking$group[result_on_blocking_on_multi$result$y])
)

result_on_blocking_multi <- blocking(x = df_on_blocking,
                                     on = "txt",
                                     on_blocking = c("group", "group2"))

expect_true(
  all(paste(df_on_blocking$group[result_on_blocking_multi$result$x],
            df_on_blocking$group2[result_on_blocking_multi$result$x]) ==
        paste(df_on_blocking$group[result_on_blocking_multi$result$y],
              df_on_blocking$group2[result_on_blocking_multi$result$y]))
)

x_link <- data.frame(txt = df_base$txt,
                     group = c("python", "person", "other"))
y_link <- data.frame(txt = df_example$txt,
                     group = c(rep("person", 4), rep("python", 3), "missing"))

result_on_blocking_link <- blocking(x = x_link,
                                    y = y_link,
                                    on = "txt",
                                    on_blocking = "group")

expect_equal(
  nrow(result_on_blocking_link$result),
  7L
)

expect_true(
  !any(result_on_blocking_link$result$y == 8L)
)

expect_true(
  all(x_link$group[result_on_blocking_link$result$x] ==
        y_link$group[result_on_blocking_link$result$y])
)

df_on_blocking_backend <- data.frame(txt = c("aa", "ab", "xx", "xy", "solo"),
                                     group = c("a", "a", "b", "b", "c"))

for (ann in c("hnsw", "annoy", "kd", "lsh")) {
  result_on_blocking_backend <- blocking(x = df_on_blocking_backend,
                                         on = "txt",
                                         on_blocking = "group",
                                         control_txt = controls_txt(n_shingles = 1L),
                                         ann = ann)

  expect_true(
    all(df_on_blocking_backend$group[result_on_blocking_backend$result$x] ==
          df_on_blocking_backend$group[result_on_blocking_backend$result$y])
  )
}

result_on_blocking_empty <- blocking(x = df_on_blocking_backend[c(1, 3), ],
                                     on = "txt",
                                     on_blocking = "group",
                                     control_txt = controls_txt(n_shingles = 1L))

expect_equal(
  names(result_on_blocking_empty$result),
  c("x", "y", "block", "dist")
)

expect_equal(
  nrow(result_on_blocking_empty$result),
  0L
)


# test parameters ---------------------------------------------------------

expect_error(
  blocking(x = df_example$txt, ann = "hnsw", distance = "manhatan")
)

expect_error(
  blocking(x = df_example$txt, ann = "annoy", distance = "cosine")
)

expect_error(
  blocking(x = df_on_blocking, on_blocking = "group")
)

expect_error(
  blocking(x = df_example$txt, on = "txt", on_blocking = "group")
)

expect_error(
  blocking(x = df_on_blocking, on = "missing", on_blocking = "group")
)

expect_error(
  blocking(x = df_on_blocking, on = "txt", on_blocking = "missing")
)

df_on_blocking_missing <- df_on_blocking
df_on_blocking_missing$group[1] <- NA

expect_error(
  blocking(x = df_on_blocking_missing, on = "txt", on_blocking = "group")
)

expect_error(
  blocking(x = df_on_blocking,
           on = "txt",
           on_blocking = "group",
           ann_write = tempdir())
)


## printing

expect_silent(
  print(blocking(x = df_example$txt))
)
