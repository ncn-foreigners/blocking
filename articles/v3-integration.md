# Integration with existing packages

## Setup

``` r
library(blocking)
library(data.table)
library(reclin2)
```

## Data

In the example we will use the same dataset as in the *Blocking records
for record linkage* vignette.

``` r
data(census)
data(cis)
setDT(census)
setDT(cis)
census[, x:=1:.N]
cis[, y:=1:.N]
```

## Integration with the `reclin2` package

The package contains function `pair_ann` which aims at integration with
`reclin2` package. This function works as follows.

``` r
pair_ann(x = census[1:1000], 
         y = cis[1:1000], 
         on = c("pername1", "pername2", "sex", "dob_day", "dob_mon", "dob_year", "enumcap", "enumpc"), 
         deduplication = FALSE) |>
  head()
#>   First data set:  1 000 records
#>   Second data set: 1 000 records
#>   Total number of pairs: 6 pairs
#>   Blocking on: 'pername1', 'pername2', 'sex', 'dob_day',
#>     'dob_mon', 'dob_year', 'enumcap', 'enumpc'
#> 
#>       .x    .y block
#>    <int> <int> <num>
#> 1:   204     1     1
#> 2:   204   176     1
#> 3:   204   375     1
#> 4:   204   391     1
#> 5:   204   405     1
#> 6:   204   424     1
```

Which provides you information on the total number of pairs. This can be
further included in the pipeline of the `reclin2` package (note that we
use a different ANN this time).

``` r
pair_ann(x = census[1:1000], 
         y = cis[1:1000], 
         on = c("pername1", "pername2", "sex", "dob_day", "dob_mon", "dob_year", "enumcap", "enumpc"), 
         deduplication = FALSE,
         ann = "hnsw") |>
  compare_pairs(on = c("pername1", "pername2", "sex", "dob_day", "dob_mon", "dob_year", "enumcap", "enumpc"),
                comparators = list(cmp_jarowinkler())) |>
  score_simple("score",
               on = c("pername1", "pername2", "sex", "dob_day", "dob_mon", "dob_year", "enumcap", "enumpc")) |>
  select_threshold("threshold", score = "score", threshold = 6) |>
  link(selection = "threshold") |>
  head()
#>   Total number of pairs: 6 pairs
#> 
#> Key: <.y>
#>       .y    .x   person_id.x pername1.x pername2.x  sex.x dob_day.x dob_mon.x
#>    <int> <int>        <char>     <char>     <char> <char>    <char>    <char>
#> 1:    11   945 DE256NG039003    HARRIET    THOMSON      F        12         1
#> 2:    71   427 DE159QA062001      LEWIS      GREEN      M        23         3
#> 3:    83   720 DE237GG025002     IMOGEN      DARIS      F         6         4
#> 4:    99   136 DE125LU022001     DANIEC     MICCER      M        21         4
#> 5:   154   949 DE256NG040002      CHLOE     WILSON      F         5         7
#> 6:   156   549 DE159QY035002        AVA       KING      F         7         7
#>    dob_year.x hse_num           enumcap.x enumpc.x          str_nam
#>        <char>   <num>              <char>   <char>           <char>
#> 1:       1995      39 39 SPRINGFIELD ROAD  DE256NG Springfield Road
#> 2:       1973      62      62 CHURCH ROAD  DE159QA      Church Road
#> 3:       1968      25   25 WOODLANDS ROAD  DE237GG   Woodlands Road
#> 4:       1947      22        22 PARK LANE  DE125LU        Park Lane
#> 5:       1978      40 40 SPRINGFIELD ROAD  DE256NG Springfield Road
#> 6:       1969      35      35 CHURCH ROAD  DE159QY      Church Road
#>                 cap_add         census_id     x
#>                  <char>            <char> <int>
#> 1: 39, Springfield Road CENSDE256NG039003   945
#> 2:      62, Church Road CENSDE159QA062001   427
#> 3:   25, Woodlands Road CENSDE237GG025002   720
#> 4:        22, Park Lane CENSDE125LU022001   136
#> 5: 40, Springfield Road CENSDE256NG040002   949
#> 6:      35, Church Road CENSDE159QY035002   549
#>                                               txt.x   person_id.y pername1.y
#>                                              <char>        <char>     <char>
#> 1: HARRIETTHOMSONF121199539 SPRINGFIELD ROADDE256NG DE256NG039003    HARRIET
#> 2:          LEWISGREENM233197362 CHURCH ROADDE159QA DE159QA062001      LEWIS
#> 3:       IMOGENDARISF64196825 WOODLANDS ROADDE237GG DE237GG025002     IMOGEW
#> 4:          DANIECMICCERM214194722 PARK LANEDE125LU DE125LU022001     DAMIEL
#> 5:     CHLOEWILSONF57197840 SPRINGFIELD ROADDE256NG DE256NG040002      CHLOE
#> 6:              AVAKINGF77196935 CHURCH ROADDE159QY DE159QY035002        AVA
#>    pername2.y  sex.y dob_day.y dob_mon.y dob_year.y           enumcap.y
#>        <char> <char>    <char>    <char>     <char>              <char>
#> 1:    THOMSON      F        12         1            39 SPRINGFIELD ROAD
#> 2:      GREEN      M        23         3                 62 CHURCH ROAD
#> 3:      DAVIS      F         6         4              25 WOODLANDS ROAD
#> 4:     HILLER      M        21         4                   22 PARK LANE
#> 5:     WILSOM      F         5         7            40 SPRINGFIELD ROAD
#> 6:       KING      F         7         7                 35 CHURCH ROAD
#>    enumpc.y           cis_id     y                                        txt.y
#>      <char>           <char> <int>                                       <char>
#> 1:  DE256NG CISDE256NG039003    11 HARRIETTHOMSONF12139 SPRINGFIELD ROADDE256NG
#> 2:  DE159QA CISDE159QA062001    71          LEWISGREENM23362 CHURCH ROADDE159QA
#> 3:  DE237GG CISDE237GG025002    83       IMOGEWDAVISF6425 WOODLANDS ROADDE237GG
#> 4:  DE125LU CISDE125LU022001    99          DAMIELHILLERM21422 PARK LANEDE125LU
#> 5:  DE256NG CISDE256NG040002   154     CHLOEWILSOMF5740 SPRINGFIELD ROADDE256NG
#> 6:  DE159QY CISDE159QY035002   156              AVAKINGF7735 CHURCH ROADDE159QY
```

## Usage with `fastLink` package

Just use the `block` column in the function `fastLink::blockData()`. As
a result you will obtain a list of records blocked for further
processing.

## Usage with `RecordLinkage` package

Just use the `block` column in the argument `blockfld` in the
`compare.dedup()` or `compare.linkage()` function. Please note that
`block` column for the `RecordLinkage` package should be stored as a
`character` not a `numeric/integer` vector.
