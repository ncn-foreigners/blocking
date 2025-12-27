# Fictional customer data

This data set was created by Paula McLeod, Dick Heasman and Ian Forbes,
ONS, for the ESSnet DI on-the-job training course, Southampton, 25-28
January 2011. It contains fictional observations from Customer
Information System, which is combined administrative data from the tax
and benefit systems.

## Usage

``` r
cis
```

## Format

A `data.table` with 24613 records. Each row represents one record, with
the following columns:

- `person_id` – a unique number for each person, consisting of postcode,
  house number and person number,

- `pername1` – forename,

- `pername2` – surname,

- `sex` – gender (M/F),

- `dob_day` – day of birth,

- `dob_mon` – month of birth,

- `dob_year` – year of birth,

- `enumcap` – an address consisting of house number and street name,

- `enumpc` – postcode,

- `cis_id` – person ID with "CIS" added in front.

## References

McLeod, P., Heasman, D., Forbes, I. (2011). Simulated data for the
ESSnet DI on-the-job training course, Southampton, 25-28 January 2011.
<https://wayback.archive-it.org/12090/20231221144450/https://cros-legacy.ec.europa.eu/content/job-training_en>

## Examples

``` r
data("cis")
head(cis)
#>       person_id pername1 pername2 sex dob_day dob_mon dob_year
#> 1 PO827ER091001   HAYDEN     HALL   M               1         
#> 2 LS992DB024001    SEREN ANDERSON   F       1       1         
#> 3  M432ZZ053003    LEWIS    LEWIS   M       1       1         
#> 4  SW75TQ018001 HARRISON   POSTER   M       5       1         
#> 5 EX527TR017006 MUHAMMED   WATSUN   M       7       1         
#> 6 SW540RB001001     RHYS THOMPSON   M       7       1         
#>              enumcap  enumpc           cis_id
#> 1   91 CLARENCE ROAD PO827ER CISPO827ER091001
#> 2     24 CHURCH LANE LS992DB CISLS992DB024001
#> 3     53 CHURCH ROAD  M432ZZ  CISM432ZZ053003
#> 4  19 HIGHFIELD ROAD  SW75TG  CISSW75TQ018001
#> 5 17 VICTORIA STREET         CISEX527TR017006
#> 6 1 SPRINGFIELD ROAD SW540RB CISSW540RB001001
```
