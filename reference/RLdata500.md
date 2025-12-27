# RLdata500 dataset from the RecordLinkage package

This data is taken from RecordLinkage R package developed by Murat
Sariyar and Andreas Borg. The package is licensed under GPL-3 license.

The `RLdata500` table contains artificial personal data. Some records
have been duplicated with randomly generated errors. `RLdata500`
contains fifty duplicates.

## Usage

``` r
RLdata500
```

## Format

A `data.table` with 500 records. Each row represents one record, with
the following columns:

- `fname_c1` – first name, first component,

- `fname_c2` – first name, second component,

- `lname_c1` – last name, first component,

- `lname_c2` – last name, second component,

- `by` – year of birth,

- `bm` – month of birth,

- `bd` – day of birth,

- `rec_id` – record id,

- `ent_id` – entity id.

## References

Sariyar M., Borg A. (2022). RecordLinkage: Record Linkage Functions for
Linking and Deduplicating Data Sets. R package version 0.4-12.4,
<https://CRAN.R-project.org/package=RecordLinkage>

## Examples

``` r
data("RLdata500")
head(RLdata500)
#>   fname_c1 fname_c2 lname_c1 lname_c2   by bm bd rec_id ent_id
#> 1  CARSTEN             MEIER          1949  7 22      1     34
#> 2     GERD             BAUER          1968  7 27      2     51
#> 3   ROBERT          HARTMANN          1930  4 30      3    115
#> 4   STEFAN             WOLFF          1957  9  2      4    189
#> 5     RALF           KRUEGER          1966  1 13      5     72
#> 6  JUERGEN            FRANKE          1929  7  4      6    142
```
