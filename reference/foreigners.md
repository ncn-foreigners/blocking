# Fictional 2024 population of foreigners in Poland

A fictional data set of the foreign population in Poland, generated
based on publicly available information while maintaining the
distributions from administrative registers.

## Usage

``` r
foreigners
```

## Format

A `data.table` with 110000 records. Each row represents one record, with
the following columns:

- `fname` – first name,

- `sname` – second name,

- `surname` – surname,

- `date` – date of birth,

- `region` – region (county),

- `country` – country,

- `true_id` – person ID.

## Examples

``` r
data("foreigners")
head(foreigners)
#>    fname sname    surname       date region country true_id
#> 1   emin           imanov 1998/02/05            031       0
#> 2 nurlan       suleymanli 2000/08/01            031       1
#> 3   amio       maharrsmov 1939/03/08            031       2
#> 4   amik       maharramof 1939/03/08            031       2
#> 5   amil       maharramov 1993/03/08            031       2
#> 6  gadir       jahangirov 1991/08/29            031       3
```
