#'
#' Get column class types for use in load_soi()
#'
#' @description
#' Given a .csv, determine the column types based on regex matches
#'
#' @param file A .csv file loaded from the IRS SOI folder
#' @param drop Columns to drop
#'
# Function to match colClasses based on regex with names
get_col_classes <- function(file, drop) {

  data <- data.table::fread(file, nrows = 1, drop = drop)
  names <- tolower(names(data))
  col_class <-
    data.table::fifelse(
      re2::re2_detect(names, "year|state|zip|agi"),
      "character",
      "numeric")
  return(col_class)
}
