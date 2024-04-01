
#' Import IRS SOI NBER Data
#'
#' @description
#' Given folder of NBER Downloaded annual .csv's from [download_nber_data()], return a data.table
#' (https://www.nber.org/research/data/individual-income-tax-statistics-zip-code-data-soi)
#'
#' @param state A State ID Code
#' @param path The location of the .csv folder output of download_nber_data()
#'
#' @details
#' - Example of state would be "CT" or "FL"
#' - Directory path used for download_nber_data() (ie: "/home/..")
#'
#' @examples
#' \dontrun{library(data.table)
#' irs <- load_soi("/home/irs_data/")}
#'
#' @importFrom stats setNames
#'
#' @export
load_soi <- function(state = "", path = path) {

  # Directory
  files <- list.files(path = path, full.names = TRUE)

  state_code <- paste0("'", paste(state, tolower(state), sep = "|"), "'")
  command <-
    data.table::fifelse(state != "", glue::glue("grep -E {state_code} "), glue::glue(""))

  # Get years
  year <- re2::re2_match(files, "\\d{4}")[,1]

  # Regex match for rows with 'CT/ct'
  irs <- data.table::rbindlist(

    setNames(lapply(files, function(file) {

      # Get column names
      col_names <-
        names(data.table::fread(
          file,
          nrows = 1,
          header = TRUE,
          sep = ","
        ))

      # Get vector of variable types
      drop <- c("agi_04470", "agi_19700", "agi_18300")
      drop <- drop[drop %chin% col_names]
      col_classes <- get_col_classes(file, drop = drop)
      cols <- col_classes
      names(cols) <- setdiff(col_names, drop)

      # Get data based on state argument
      if (state == "") {
        data <-
          data.table::fread(
            file,
            select = cols,
            nThread = 4)
      } else {
        data <-
          data.table::fread(
            cmd = glue::glue("{command}{file}"),
            select = cols,
            nThread = 4
          )
      }

      # Clean names
      data <- janitor::clean_names(data)

      # Return
      return(data)
    }), year),
    fill = TRUE,
    use.names = TRUE,
    idcol = "year1"
  )



  return(irs)

}
