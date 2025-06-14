#' @title RLdata500 dataset from the RecordLinkage package
#'
#' @description
#'
#' This data is taken from \pkg{RecordLinkage} R package developed by Murat Sariyar and Andreas Borg.
#' The package is licensed under GPL-3 license.
#'
#' The `RLdata500` table contains artificial personal data.
#' Some records have been duplicated with randomly generated errors. `RLdata500` contains fifty duplicates.
#'
#' @format A `data.table` with 500 records. Each row represents one record, with the following columns:
#'
#' \itemize{
#' \item{`fname_c1` -- first name, first component,}
#' \item{`fname_c2` -- first name, second component,}
#' \item{`lname_c1` -- last name, first component,}
#' \item{`lname_c2` -- last name, second component,}
#' \item{`by` -- year of birth,}
#' \item{`bm` -- month of birth,}
#' \item{`bd` -- day of birth,}
#' \item{`rec_id` -- record id,}
#' \item{`ent_id` -- entity id.}
#' }
#'
#' @references
#' Sariyar M., Borg A. (2022). RecordLinkage: Record Linkage Functions for Linking and Deduplicating Data Sets.
#' R package version 0.4-12.4, \url{https://CRAN.R-project.org/package=RecordLinkage}
#'
#' @docType data
#' @keywords datasets
#' @name RLdata500
#' @rdname RLdata500
#' @examples
#'
#' data("RLdata500")
#' head(RLdata500)
#'
"RLdata500"


#' @title Fictional census data
#'
#' @description
#' This data set was created by Paula McLeod, Dick Heasman and Ian Forbes, ONS,
#' for the ESSnet DI on-the-job training course, Southampton, 25-28 January 2011.
#' It contains fictional data representing some observations from a decennial Census.
#'
#' @format A `data.table` with 25343 records. Each row represents one record, with the following columns:
#' \itemize{
#' \item{`person_id` -- a unique number for each person, consisting of postcode, house number and person number,}
#' \item{`pername1` -- forename,}
#' \item{`pername2` -- surname,}
#' \item{`sex` -- gender (M/F),}
#' \item{`dob_day` -- day of birth,}
#' \item{`dob_mon` -- month of birth,}
#' \item{`dob_year` -- year of birth,}
#' \item{`hse_num` -- house number, a numeric label for each house within a street,}
#' \item{`enumcap` -- an address consisting of house number and street name,}
#' \item{`enumpc` -- postcode,}
#' \item{`str_nam` -- street name of person's household's street,}
#' \item{`cap_add` -- full address, consisting of house number, street name and postcode,}
#' \item{`census_id` -- person ID with "CENS" added in front.}
#' }
#'
#' @references
#' McLeod, P., Heasman, D., Forbes, I. (2011). Simulated data for the ESSnet DI on-the-job training course,
#' Southampton, 25-28 January 2011.
#' \url{https://wayback.archive-it.org/12090/20231221144450/https://cros-legacy.ec.europa.eu/content/job-training_en}
#'
#' @docType data
#' @keywords datasets
#' @name census
#' @rdname census
#' @examples
#'
#' data("census")
#' head(census)
#'
"census"

#' @title Fictional customer data
#'
#' @description
#' This data set was created by Paula McLeod, Dick Heasman and Ian Forbes, ONS,
#' for the ESSnet DI on-the-job training course, Southampton, 25-28 January 2011.
#' It contains fictional observations from Customer Information System,
#' which is combined administrative data from the tax and benefit systems.
#'
#' @format A `data.table` with 24613 records. Each row represents one record, with the following columns:
#' \itemize{
#' \item{`person_id` -- a unique number for each person, consisting of postcode, house number and person number,}
#' \item{`pername1` -- forename,}
#' \item{`pername2` -- surname,}
#' \item{`sex` -- gender (M/F),}
#' \item{`dob_day` -- day of birth,}
#' \item{`dob_mon` -- month of birth,}
#' \item{`dob_year` -- year of birth,}
#' \item{`enumcap` -- an address consisting of house number and street name,}
#' \item{`enumpc` -- postcode,}
#' \item{`cis_id` -- person ID with "CIS" added in front.}
#' }
#'
#' @references
#' McLeod, P., Heasman, D., Forbes, I. (2011). Simulated data for the ESSnet DI on-the-job training course,
#' Southampton, 25-28 January 2011.
#' \url{https://wayback.archive-it.org/12090/20231221144450/https://cros-legacy.ec.europa.eu/content/job-training_en}
#'
#' @docType data
#' @keywords datasets
#' @name cis
#' @rdname cis
#' @examples
#'
#' data("cis")
#' head(cis)
#'
"cis"

#' Fictional 2024 population of foreigners in Poland
#'
#' @description
#' A fictional data set of the foreign population in Poland,
#' generated based on publicly available information
#' while maintaining the distributions from administrative registers.
#'
#' @format A `data.table` with 110000 records. Each row represents one record, with the following columns:
#' \itemize{
#' \item{`fname` -- first name,}
#' \item{`sname` -- second name,}
#' \item{`surname` -- surname,}
#' \item{`date` -- date of birth,}
#' \item{`region` -- region (county),}
#' \item{`country` -- country,}
#' \item{`true_id` -- person ID.}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name foreigners
#' @rdname foreigners
#' @examples
#'
#' data("foreigners")
#' head(foreigners)
#'
"foreigners"
