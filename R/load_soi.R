
load_soi <- function(state = "", path = path) {

  # Directory
   files <- list.files(subdir,full.names = TRUE)

  state_code <-
    paste0("'", paste(state, tolower(state), sep = "|"), "'")
  command <-
    fifelse(state != "", glue("grep -E {state_code} "), glue(""))

  # Get years
  year <-
    str_extract(list.files(subdir, full.names = TRUE), "\\d{4}")

  # Regex match for rows with 'CT/ct'
  irs <- rbindlist(

    setNames(lapply(files, function(file) {

      # Get column names
      cols <- names(fread(
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
          fread(file,
                colClasses = col_classes,
                col.names = cols,
                nThread = 3)
      } else {
        data <-
          fread(
            cmd = glue("{command}{file}"),
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
