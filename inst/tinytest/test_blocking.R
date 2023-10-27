
## small data

# generate data -----------------------------------------------------------

df_example <- data.frame(txt = c(
  "jankowalski",
  "kowalskijan",
  "kowalskimjan",
  "kowaljan",
  "montypython",
  "pythonmonty",
  "cyrkmontypython",
  "monty"
))

# check arguments ---------------------------------------------------------

expect_silent(
  blocking(x = df_example$txt, verbose = F, ann = "hnsw")
)


# check functionalities ---------------------------------------------------


expect_equal(
  as.numeric(blocking(x = df_example$txt, ann = "hnsw")),
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

expect_equal(
  as.numeric(blocking(x = df_example$txt, ann = "lsh")),
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

