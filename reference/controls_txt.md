# Controls for processing character data

Controls for text data used in the `blocking` function (if
`representation = shingles`), passed to
[tokenize_character_shingles](https://docs.ropensci.org/tokenizers/reference/shingle-tokenizers.html).

## Usage

``` r
controls_txt(
  n_shingles = 2L,
  n_chunks = 10L,
  lowercase = TRUE,
  strip_non_alphanum = TRUE
)
```

## Arguments

- n_shingles:

  length of shingles (default `2L`),

- n_chunks:

  passed to (default `10L`),

- lowercase:

  should the characters be made lower-case? (default `TRUE`),

- strip_non_alphanum:

  should punctuation and white space be stripped? (default `TRUE`).

## Value

Returns a list with parameters.

## Author

Maciej Beręsewicz
