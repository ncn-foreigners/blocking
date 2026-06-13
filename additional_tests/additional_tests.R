## Simple manual checks for `on` and `on_blocking`.

if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  library(blocking)
}

options(text2vec.mc.cores = 1L)

n <- 50
on <- c("first_name", "last_name", "city")
on_blocking <- c("region", "cohort")

x <- data.frame(
  id = seq_len(n),
  first_name = paste0("first", seq_len(n)),
  last_name = paste0("last", seq_len(n)),
  city = paste0("city", rep(1:10, length.out = n)),
  region = rep(c("north", "south", "east", "west", "central"),
               length.out = n),
  cohort = rep(c("a", "b"), length.out = n),
  stringsAsFactors = FALSE
)

y <- data.frame(
  id = seq_len(n),
  first_name = paste0("first", c(1:45, 101:105)),
  last_name = paste0("last", c(1:45, 101:105)),
  city = paste0("city", rep(1:10, length.out = n)),
  region = rep(c("north", "south", "east", "west", "central"),
               length.out = n),
  cohort = rep(c("a", "b"), length.out = n),
  stringsAsFactors = FALSE
)

true_blocks <- data.frame(
  x = 1:45,
  y = 1:45,
  block = 1:45
)

blocking_on <- blocking::blocking(
  x = x,
  y = y,
  on = on,
  true_blocks = true_blocks
)

blocking_on_blocking <- blocking::blocking(
  x = x,
  y = y,
  on = on,
  on_blocking = on_blocking,
  true_blocks = true_blocks
)

pair_on <- blocking::pair_ann(
  x = x,
  y = y,
  on = on
)

pair_on_blocking <- blocking::pair_ann(
  x = x,
  y = y,
  on = on,
  on_blocking = on_blocking
)

true_pairs <- data.frame(
  .x = true_blocks$x,
  .y = true_blocks$y,
  true_block = true_blocks$block
)

pair_on_eval <- merge(pair_on, true_pairs, by = c(".x", ".y"), all.x = TRUE)
pair_on_eval$true_match <- !is.na(pair_on_eval$true_block)
pair_on_summary <- table(pair_on_eval$true_match)

pair_on_blocking_eval <- merge(pair_on_blocking, true_pairs,
                               by = c(".x", ".y"), all.x = TRUE)
pair_on_blocking_eval$true_match <- !is.na(pair_on_blocking_eval$true_block)
pair_on_blocking_summary <- table(pair_on_blocking_eval$true_match)

blocking_on
blocking_on$metrics
blocking_on_blocking
blocking_on_blocking$metrics
pair_on
pair_on_eval
pair_on_summary
pair_on_blocking
pair_on_blocking_eval
pair_on_blocking_summary
