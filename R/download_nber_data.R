
#' Download data from NBER and store in designated folder
#'
#' @description
#' Given years to download and folder path, downloads raw .csv data
#' (https://www.nber.org/research/data/individual-income-tax-statistics-zip-code-data-soi)
#' Download time is approximately 1 minute per year
#'
#' @param path "/home/irs_data/"
#' @param start_year 2005
#' @param finish_year 2016
#'
#' @examples
#' \dontrun{library(data.table)
#'  download_nber_data(start = 2016)}
#'

download_nber_data <- function(path, start_year, finish_year) {

  years <- as.character(c(start_year:finish_year))
  sapply(years, function(year) {
    url <-
      glue::glue("https://data.nber.org/tax-stats/zipcode/{year}/zipcode{year}.csv")
    file <- fread(url)
    fwrite(file, glue::glue("{path}zipcode_{year}.csv"))
  })
}
