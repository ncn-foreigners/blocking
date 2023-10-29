
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
  blocking(x = df_example$txt,
           verbose = F,
           control_ann = controls_ann(hnsw=list(M = 5, ef_s = 10, ef_b = 10)))
)


# check functionalities ---------------------------------------------------

## RcppHNSW ----------------------------------------------------------------

expect_equal(
  as.numeric(blocking(x = df_example$txt,
                      verbose = F,
                      ann = "hnsw",
                      control_ann = controls_ann(hnsw = list(M = 5, ef_s = 10, ef_b = 10)))),
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

## mlpack ----------------------------------------------------------------

expect_equal(
  as.numeric(blocking(x = df_example$txt, ann = "lsh")),
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

## RcppAnnoy ----------------------------------------------------------------

expect_equal(
  as.numeric(blocking(x = df_example$txt, verbose = F, ann = "annoy",
                      control_ann = controls_ann(distance = "euclidean"))),
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

## RANN ----------------------------------------------------------------

expect_equal(
  as.numeric(blocking(x = df_example$txt, verbose = F, ann = "kd")),
  c(1, 1, 1, 1, 2, 2, 2, 2)
)
