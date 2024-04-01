
#' Helper function to clean raw IRS SOI data loaded with load_soi()
#'
#' @description
#' Cleans raw IRS annual data from [load_soi()] by removing summary rows,
#' aggregating income levels and coalescing number of return columns,
#' which vary from early to later years and number of return columns
#'
#' @param data Uncleaned data.table of annual tax data
#'
clean_soi <- function(data) {

  # Make copy
  data <- data.table::copy(data)

  # Filter duplicate year column from post 2008 if exists
  data[, year := NULL]
  setnames(data, "year1", "year")

  # Set key cols
  data.table::setkeyv(data, c("year", "zipcode"))

  # Remove pre-formatted summary rows
  data <- data[!zipcode %chin% c("00000", "99999", "0", "")]

  ## Impute agi_class to bad missing allocations in 2006 data
  if ("agi_class" %chin% names(data)) {
    data[as.integer(year) < 2008 &
          as.integer(agi_class) > 7,
        agi_class := calc_agi(a00100, n1)]
  }

  # Coalesce multiple dependent fields across years
  if ("n6" %chin% names(data) & "numdep" %chin% names(data)) {
    # Coalesce diff dependent variable name from pre 2007
    data[, numdep := data.table::fcoalesce(numdep, n6)]
  } else if ("n6" %chin% names(data)) {
    data.table::setnames(data, "n6", "numdep")
  }

  ## Convert all state abbreviations to upper case and all zipcodes to 5-digit
  data[
    , zipcode := data.table::fifelse(
        !re2::re2_detect(zipcode, "^\\d{4}$"),
        zipcode,
        paste0("0", zipcode)
        )]

  #Fix unequal income brackets
  if ("agi_class" %in% names(data)) {
    data[as.integer(year) <= 2008,
        agi_level := convert_agi(agi_class)]
  }
  if ("agi_stub" %chin% names(data)) {
    data[as.integer(year) > 2008,
        `:=`(
          agi_level = convert_agi_2(agi_stub),
          agi_stub = convert_agi_3(agi_stub)
          )]
  }

  # Add missing for agi_stub in pre-2008 years
  data[as.integer(year) <= 2008,
          agi_stub := NA_character_]
  data.table::setnames(data, "agi_stub" ,"agi_level_2")

  # Drop cols if included in data
  cols <-
    c("n6",
      "statefips",
      "agi_class",
      "agi_stub")
  drops <- names(data)[names(data) %in% cols]
  if (length(drops) > 0) {
    data[, c(drops) := NULL]
  }

  # Aggregate rows by same agi_level, year, zipcode
  # num <-
  #   setdiff(names(data), c("agi_level", "agi_level_2", "year", "zipcode", "state"))
  # data <-
  #   data[, lapply(.SD, sum, na.rm = TRUE)
  #         , .SDcols = num
  #       , by = list(year, zipcode, agi_level)]

  # Convert state to upper to fix bad rows
  data[, state := toupper(state)]
  # states <- unique(data[, list(zipcode, state)], by = "zipcode")
  # data <- states[data, on = "zipcode"]

  # Remove if agi_level is NA (only a few rows in 2006)
  data <- data[!is.na(agi_level)]

  # Filter only zipcodes where all periods/agi_levels present
  zip_table <-
    table(data[, .N, zipcode]$N)
  complete_zipcode <- as.integer(names(zip_table[which.max(zip_table)]))
  complete_zipcodes <- seq(complete_zipcode-2,complete_zipcode +2, 1)
  data <- data[, .SD[.N %in% complete_zipcodes], zipcode]

  ## Divide numeric amount columns by 1000 for 2007 and 2008 to equalize with other years
  cols <- names(data)[re2::re2_which(names(data), "^a\\d{5}")]
  if (any(c("2016", "2008") %chin% unique(data$year))) {
    data[year %in% c("2007", "2008"),
        (cols) := lapply(.SD, function(x)
          x / 1000), .SDcols = cols]
  }

  ##Merge total tax variables from pre and post 2007 into total tax
  if (all(any(as.numeric(data$year) >= 2007)
          & any(as.numeric(data$year) < 2007))) {
    data[, total_tax :=
          data.table::fcase(
            as.numeric(year) < 2007, a09200,
            as.numeric(year) >= 2007, a10300)]
    } else if (all(as.numeric(data$year) < 2007)) {
      data[, total_tax := a09200]
    } else {
      data[, total_tax := a10300]
    }

  # Set keys and indices
  setkeyv(data, c("year", "zipcode", "agi_level"))

  #Return
  return(data)

}
