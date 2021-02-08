
load_soi <- function(state = "", path = path) {

  # Directory
   files <- list.files(subdir,full.names = TRUE)

  state_code <-
    paste0("'", paste(state, tolower(state), sep = "|"), "'")
  command <-
    data.table::fifelse(state != "", glue::glue("grep -E {state_code} "), glue::glue(""))

  # Get years
  year <-
    stringr::str_extract(list.files(subdir, full.names = TRUE), "\\d{4}")

  # Regex match for rows with 'CT/ct'
  irs <- data.table::rbindlist(

    setNames(lapply(files, function(file) {

      # Get column names
      cols <- names(data.table::fread(
        file,
        nrows = 1,
        header = TRUE,
        sep = ","
      ))

      # Get vector of variable types
      col_classes <- get_col_classes(file)

      # Get data based on state argument
      if ( state == "" ) {
        data <-
          data.table::fread(file,
                colClasses = col_classes,
                col.names = cols,
                nThread = 3)
      } else {
        data <-
          data.table::fread(
            cmd = glue::glue("{command}{file}"),
            colClasses = col_classes,
            col.names = cols,
            nThread = 3
          )
      }

      # Clean names
      data <- janitor::clean_names(data)

      # Return
      return(data)
    }), year),
    fill = TRUE,
    use.names = TRUE,
    idcol = "year"
  )

  return(irs)

}
