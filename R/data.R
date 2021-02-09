#' Clean IRS Data for Shiny App
#'
#' Cleaned up IRS Data
#'
#' @examples
#' irs_app_data
#'
#'
#'#' @format A data frame with 2033904 rows and 9 variables:
#' \describe{
#'   \item{year}{year, int}
#'   \item{zipcode}{zipcode chr}
#'   \item{state}{state abbreviation code, chr}
#'   \item{county}{county name, State, chr}
#'   \item{agi_level}{five income levels, chr}
#'   \item{post_office_city}{city, state, chr}
#'   \item{n1}{number of taxpayers, int}
#'   \item{a00100}{Adjusted Gross Income, num}
#'   \item{total_tax}{Total tax paid, num}
#' }
#' @source \url{https://www.nber.org/research/data/individual-income-tax-statistics-zip-code-data-soi/}
#'
"irs_app_data"
