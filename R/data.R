#' Clean IRS Data for Shiny App
#'
#' Sample of cleaned up IRS Data product after running load_soi() and clean_soi()
#' Only includes 2018 for CT
#'
#' @format A data frame with 2033904 rows and 9 variables:
#' \describe{
#'   \item{year}{year, int}
#'   \item{zipcode}{zipcode, chr}
#'   \item{state}{state, chr}
#'   \item{county}{county, chr}
#'   \item{post_office_city}{city, chr}
#'   \item{agi_level}{five income levels, chr}
#'   \item{n1}{number of taxpayers, int}
#'   \item{a00100}{Adjusted Gross Income, num}
#'   \item{total_tax}{Total tax paid, num}
#' }
#' @source \url{https://www.nber.org/research/data/individual-income-tax-statistics-zip-code-data-soi/}
#'
"irs_app_data"
