# Integration with the reclin2 package

Function for the integration with the reclin2 package. The function is
based on [pair_minsim](https://rdrr.io/pkg/reclin2/man/pair_minsim.html)
and reuses some of its source code.

## Usage

``` r
pair_ann(
  x,
  y = NULL,
  on,
  deduplication = TRUE,
  keep_block = TRUE,
  add_xy = TRUE,
  ...
)
```

## Arguments

- x:

  reference data (a data.frame or a data.table),

- y:

  query data (a data.frame or a data.table, default NULL),

- on:

  a character with column name or a character vector with column names
  for the ANN search,

- deduplication:

  whether deduplication should be performed (default TRUE),

- keep_block:

  whether to keep the block variable in the set,

- add_xy:

  whether to add x and y,

- ...:

  arguments passed to
  [blocking](https://ncn-foreigners.ue.poznan.pl/blocking/reference/blocking.md)
  function.

## Value

Returns a
[data.table](https://rdrr.io/pkg/data.table/man/data.table.html) with
two columns `.x` and `.y`. Columns `.x` and `.y` are row numbers from
data.frames x and y respectively. Returned `data.table` is also of a
class `pairs` which allows for integration with the
[compare_pairs](https://rdrr.io/pkg/reclin2/man/compare_pairs.html)
function.

## Author

Maciej Beręsewicz

## Examples

``` r
# example using two datasets from reclin2

# \donttest{
if (requireNamespace("reclin2", quietly = TRUE)) {

library(reclin2)
data("linkexample1", "linkexample2", package = "reclin2")

linkexample1$txt <- with(linkexample1, tolower(paste0(firstname, lastname, address, sex, postcode)))
linkexample1$txt <- gsub("\\s+", "", linkexample1$txt)
linkexample2$txt <- with(linkexample2, tolower(paste0(firstname, lastname, address, sex, postcode)))
linkexample2$txt <- gsub("\\s+", "", linkexample2$txt)

# pairing records from linkexample2 to linkexample1 based on txt column

pair_ann(x = linkexample1, y = linkexample2, on = "txt", deduplication = FALSE) |>
compare_pairs(on = "txt", comparators = list(cmp_jarowinkler())) |>
score_simple("score", on = "txt") |>
select_threshold("threshold", score = "score", threshold = 0.75) |>
link(selection = "threshold")
}
#>   Total number of pairs: 5 pairs
#> 
#> Key: <.y>
#>       .y    .x  id.x lastname.x firstname.x  address.x  sex.x postcode.x
#>    <int> <int> <int>     <fctr>      <fctr>     <fctr> <fctr>     <fctr>
#> 1:     1     2     2      Smith      George 12 Mainstr      M    1234 AB
#> 2:     2     3     3    Johnson        Anna 61 Mainstr      F    1234 AB
#> 3:     3     4     4    Johnson     Charles 61 Mainstr      M    1234 AB
#> 4:     4     6     6   Schwartz         Ben  1 Eaststr      M    6789 XY
#> 5:     5     6     6   Schwartz         Ben  1 Eaststr      M    6789 XY
#>                             txt.x  id.y lastname.y firstname.y     address.y
#>                            <char> <int>     <fctr>      <fctr>        <fctr>
#> 1:    georgesmith12mainstrm1234ab     2      Smith      Gearge 12 Mainstreet
#> 2:    annajohnson61mainstrf1234ab     3     Jonson          A. 61 Mainstreet
#> 3: charlesjohnson61mainstrm1234ab     4    Johnson     Charles    61 Mainstr
#> 4:     benschwartz1eaststrm6789xy     6   Schwartz         Ben        1 Main
#> 5:     benschwartz1eaststrm6789xy     7   Schwartz        Anna     1 Eaststr
#>     sex.y postcode.y                           txt.y
#>    <fctr>     <fctr>                          <char>
#> 1:   <NA>    1234 AB geargesmith12mainstreetna1234ab
#> 2:      F    1234 AB     a.jonson61mainstreetf1234ab
#> 3:      F    1234 AB  charlesjohnson61mainstrf1234ab
#> 4:      M    6789 XY         benschwartz1mainm6789xy
#> 5:      F    6789 XY     annaschwartz1eaststrf6789xy
# }
```
