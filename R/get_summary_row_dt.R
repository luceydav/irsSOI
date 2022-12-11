

#' Helper function to receive irs data.table and return annual summary rows for that zipcode
#'
#' @description
#' Receive full dataset and return only summary rows usually denoted "00000" or "99999"
#'
#' @param irs IRS data.table
#'
#' @import data.table
#' @importFrom re2 re2_detect
#'
#' @export
get_summary_row_dt <- function(irs) {

  # Copies irs data and keeps zipcode, year and number of returns
  dt_zips <-
    data.table::copy(irs)[, list(zipcode, year, n1)]

  #Set key cols
  data.table::setkey(dt_zips, year)

  # Fix any zips starting with zero hence 4 digits
  dt_zips[, `:=`(
    zipcode =
      data.table::fifelse(
        !re2::re2_detect(zipcode, "^\\d{4}$"),
        zipcode,
        paste0("0", zipcode)))]

  #Remove pre-formatted summary rows
  dt_zips <-
    dt_zips[zipcode %chin% c("00000", "99999", "0")]

  return(dt_zips)
}
