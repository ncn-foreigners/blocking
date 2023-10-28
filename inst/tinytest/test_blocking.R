
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
           ann = "hnsw",
           control_ann = controls_ann(hnsw=list(M = 5, ef_s = 10, ef_b = 10),
                                      distance = "cosine"))
)


# check functionalities ---------------------------------------------------


expect_equal(
  as.numeric(blocking(x = df_example$txt,
                      verbose = F,
                      ann = "hnsw",
                      control_ann = controls_ann(hnsw = list(M = 5, ef_s = 10, ef_b = 10),
                                                 distance = "cosine"))),
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

expect_equal(
  as.numeric(blocking(x = df_example$txt, ann = "lsh"),
             control_ann = controls_ann(distance = "cosine")),
  c(1, 1, 1, 1, 2, 2, 2, 2)
)

