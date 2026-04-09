# Example: record linkage between `census` and `cis`
# using the DuckDB backend (`vss` engine).

library(devtools)
load_all()

library(blocking)
library(data.table)

# One-time setup outside `blocking()`.
duckdb_setup_vss(update = TRUE, verbose = TRUE)

data(census)
data(cis)

census <- data.table::copy(as.data.table(census))
cis <- data.table::copy(as.data.table(cis))

set.seed(2026)

common_ids <- intersect(census$person_id, cis$person_id)
census_only_ids <- setdiff(census$person_id, common_ids)
cis_only_ids <- setdiff(cis$person_id, common_ids)

# Keep the example quick to run while preserving both
# true matches and unmatched records on each side.
sample_common <- sample(common_ids, 1000)
sample_census_only <- sample(census_only_ids, 250)
sample_cis_only <- sample(cis_only_ids, 250)

census_small <- census[person_id %chin% c(sample_common, sample_census_only)]
cis_small <- cis[person_id %chin% c(sample_common, sample_cis_only)]

# Shuffle row order so the linkage problem is not trivial.
census_small <- census_small[sample(.N)]
cis_small <- cis_small[sample(.N)]

census_small[, txt := paste0(
  pername1, pername2, sex,
  dob_day, dob_mon, dob_year,
  enumcap, enumpc
)]

cis_small[, txt := paste0(
  pername1, pername2, sex,
  dob_day, dob_mon, dob_year,
  enumcap, enumpc
)]

matches <- merge(
  x = census_small[, .(x = .I, person_id)],
  y = cis_small[, .(y = .I, person_id)],
  by = "person_id"
)
matches[, block := .I]

system.time(

duckdb_result <- blocking(
  x = census_small$txt,
  y = cis_small$txt,
  ann = "duckdb",
  distance = "cosine",
  true_blocks = matches[, .(x, y, block)],
  n_threads = 8L,
  control_ann = controls_ann(
    k_search = 1L,
    duckdb = control_duckdb(
      engine = "vss",
      join_mode = "classic",
      install_extension = FALSE,
      vss_ef_search = 16L,
      vss_ef_construction = 64L,
      vss_M = 8L,
      vss_M0 = 16L
    )
  )
)
)

print(duckdb_result)
print(head(duckdb_result$result, 20))
print(duckdb_result$metrics)

system.time(
nnd_result <- blocking(
  x = census_small$txt,
  y = cis_small$txt,
  ann = "nnd",
  distance = "cosine",
  true_blocks = matches[, .(x, y, block)],
  n_threads = 8L,
  control_ann = controls_ann(
    k_search = 25
  ),
  verbose = T
)
)

print(nnd_result)
print(head(nnd_result$result, 20))
print(nnd_result$metrics)
