#' `RLdata500` data from [RecordLinkage::RLdata]
#'
#'
#' @description
#' The `RLdata500` table contains artificial personal data.
#' Some records have been duplicated with randomly generated errors. `RLdata500` contains fifty duplicates.
#'
#' @format A character matrix with 500 records. Each row represents one record, with the following columns:
#'
#' \itemize{
#' \item{`fname_cq` -- first name, first component,}
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
