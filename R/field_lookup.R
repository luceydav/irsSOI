
#' Lookup Field Description from IRS SOI Data
#'
#' @description
#' Takes year and NBER Field identifier and returns Field Description
#' (https://www.nber.org/research/data/individual-income-tax-statistics-zip-code-data-soi)
#'
#' @param select_year Int year
#' @param name Field identifier
#'
#' @details
#' - Example is year = "2016" and name = "a00100"
#'
#' @examples
#' \dontrun{library(data.table)
#' field_lookup("2016", "a00100")}
#'
#' @export
field_lookup <- function(select_year = "2016", name = "a00100") {
  if (!is.character(select_year) ) {
    select_year <- as.character(select_year)
  }
  data("data_dict")
  unlist(data_dict[year == select_year & names == name]$row_lists)
}
