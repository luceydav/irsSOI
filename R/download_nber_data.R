
#' Download raw data from NBER and store in designated folder
#'
#' @description
#' Given years to download and folder path, downloads raw .csv data
#' Download time is approximately 1 minute per year and 75MB per year
#' (https://www.nber.org/research/data/individual-income-tax-statistics-zip-code-data-soi)
#'
#' @param path "/home/irs_data/"
#' @param start_year 2005
#' @param end_year 2020
#'
#' @examples
#' \dontrun{library(data.table)
#'  download_nber_data(start = 2005)}
#'
#' @export
download_nber_data <- function(path = "", start_year, end_year) {

    # Verify start_year
    if (is.integer(start_year) & !start_year %in% c(2005:2020) ) {
      print("Choose integer start_year between 2005-2020")
    }

    # Verify end_year
    if (is.integer(end_year) & !end_year %in% c(2005:2020) ){
      print("Choose integer end_year between 2005-2020")
    }

    # Verify start_year < end_year
    if ( end_year < start_year) {
      print("Choose end_year after start_year")
    }

  # If no path provided, make "data" folder in working dir and save request there
  if ( path == "" ) {
    dir <- getwd()
    path <- paste0(dir, "/data/")
    output_dir <- "data"
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
      }
  }

  # Download selected years and put in data file
  years <- as.character(c(start_year:end_year))
  sapply(years, function(year) {
    filename <- paste0(substr(as.character(year), 3, 4), "zpallagi.csv")
    url <- glue::glue('https://data.nber.org/tax-stats/zipcode/{year}/{filename}')
    #url <-
      #glue::glue("https://data.nber.org/tax-stats/zipcode/{year}/zipcode{year}.csv")
    print(glue::glue("Downloading {year}"))
    file <- data.table::fread(url)
    data.table::fwrite(
      file,
      glue::glue("{path}zipcode_{year}.csv"),
      compress = "gzip")
  })
}
