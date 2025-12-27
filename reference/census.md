# Fictional census data

This data set was created by Paula McLeod, Dick Heasman and Ian Forbes,
ONS, for the ESSnet DI on-the-job training course, Southampton, 25-28
January 2011. It contains fictional data representing some observations
from a decennial Census.

## Usage

``` r
census
```

## Format

A `data.table` with 25343 records. Each row represents one record, with
the following columns:

- `person_id` – a unique number for each person, consisting of postcode,
  house number and person number,

- `pername1` – forename,

- `pername2` – surname,

- `sex` – gender (M/F),

- `dob_day` – day of birth,

- `dob_mon` – month of birth,

- `dob_year` – year of birth,

- `hse_num` – house number, a numeric label for each house within a
  street,

- `enumcap` – an address consisting of house number and street name,

- `enumpc` – postcode,

- `str_nam` – street name of person's household's street,

- `cap_add` – full address, consisting of house number, street name and
  postcode,

- `census_id` – person ID with "CENS" added in front.

## References

McLeod, P., Heasman, D., Forbes, I. (2011). Simulated data for the
ESSnet DI on-the-job training course, Southampton, 25-28 January 2011.
<https://wayback.archive-it.org/12090/20231221144450/https://cros-legacy.ec.europa.eu/content/job-training_en>

## Examples

``` r
data("census")
head(census)
#>      person_id pername1 pername2 sex dob_day dob_mon dob_year hse_num
#> 1 DE03US001001    COUIE    PRICE   M       1       6     1960       1
#> 2 DE03US001002    ABBIE    PVICE   F       9      11     1961       1
#> 3 DE03US001003    LACEY    PRICE   F       7       2     1999       1
#> 4 DE03US001004   SAMUEL    PRICE   M      13       4     1990       1
#> 5 DE03US001005   JOSEPH    PRICE   M      20       4     1986       1
#> 6 DE03US001006     JOSH    PRICE   M      14       2     1996       1
#>          enumcap enumpc      str_nam         cap_add        census_id
#> 1 1 WINDSOR ROAD DE03US Windsor Road 1, Windsor Road CENSDE03US001001
#> 2 1 WINDSOR ROAD DE03US Windsor Road 1, Windsor Road CENSDE03US001002
#> 3 1 WINDSOR ROAD DE03US Windsor Road 1, Windsor Road CENSDE03US001003
#> 4 1 WINDSOR ROAD DE03US Windsor Road 1, Windsor Road CENSDE03US001004
#> 5 1 WINDSOR ROAD DE03US Windsor Road 1, Windsor Road CENSDE03US001005
#> 6 1 WINDSOR ROAD DE03US Windsor Road 1, Windsor Road CENSDE03US001006
```
