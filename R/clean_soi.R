
#' Helper function to clean raw IRS SOI data loaded with load_soi()
#'
#' @description
#' Cleans raw IRS annual data from [load_soi()] by removing summary rows,
#' aggregating income levels and coalescing number of return columns,
#' which vary from early to later years and number of return columns
#'
#' @param irs_raw Uncleaned data.table of annual tax data
#'
#' @export
clean_soi <- function(irs_raw) {

  # Make copy
  irs <- data.table::copy(irs_raw)

  # Filter duplicate year column from post 2008 if exists
  irs <- irs[, .SD, .SDcols = unique(names(irs))]

  # Set key cols
  data.table::setkey(irs, zipcode)

  # Remove pre-formatted summary rows
  irs <-
    irs[!zipcode %chin% c("00000", "99999", "0", "")]

  ## Fill in bad agi_class allocations in 2006 data
  if ( "agi_class" %chin% names(irs) ) {
    irs[as.integer(year) < 2008 &
          as.integer(agi_class) > 7,
        agi_class := calc_agi(a00100, n1)]
  }

  # Coalesce multiple dependent fields across years
  if ("n6" %chin% names(irs) & "numdep" %chin% names(irs)) {
    # Coalesce diff dependent variable name from pre 2007
    irs[, numdep := data.table::fcoalesce(numdep, n6)]
  } else if ("n6" %chin% names(irs) ) {
    setnames(irs, "n6", "numdep")
  }

  ## Convert all state abbreviations to upper case and all zipcodes to 5-digit
  irs[, `:=`(
    zipcode =
      fifelse(
        !stringr::str_detect(zipcode, "^\\d{4}$"),
        zipcode,
        paste0("0", zipcode)))]

  #Fix unequal income brackets
  if ( "agi_class" %in% names(irs) ) {
    irs[as.integer(year) <= 2008,
        agi_level := convert_agi(agi_class)]
  }
  if ( "agi_stub" %chin% names(irs) ) {
    irs[as.integer(year) > 2008,
        agi_level := convert_agi_2(agi_stub)]
  }

  # Drop cols if included in data
  cols <-
    c("n6",
      "statefips",
      "agi_04470",
      "agi_19700",
      "agi_18300",
      "agi_class",
      "agi_stub")
  drops <- names(irs)[names(irs) %in% cols]
  if ( length(drops) > 0 ) {
    irs[, c(drops) := NULL]
  }

  # Aggregate rows by same agi_level, year, zipcode
  num <-
    setdiff(names(irs), c("agi_level", "year", "zipcode", "state"))
  states <-
    unique(irs[, list(zipcode, state)], by = "zipcode")
  irs <-
    irs[, lapply(.SD, sum, na.rm = TRUE),
        .SDcols = num,
        by = list(year, zipcode, agi_level)]
  irs <- states[irs, on = "zipcode"]
  irs[, state := toupper(state)]

  # Remove if agi_level is NA (only a few rows)
  irs <- irs[!is.na(agi_level)]

  # Filter only zipcodes where all periods/agi_levels present
  complete_zipcode <- length(unique(irs$year)) * 5
  irs <- irs[, .SD[.N == complete_zipcode], zipcode]

  ## Divide numeric amount columns by 1000 for 2007 and 2008 to equalize with other years
  cols <- names(irs)[stringr::str_which(names(irs), "^a\\d{5}")]
  if ( any(c("2016", "2008") %chin% unique(irs$year)) ) {
    irs[year %in% c("2007", "2008"),
        (cols) := lapply(.SD, function(x)
          x / 1000), .SDcols = cols]
  }

  ##Merge total tax variables from pre and post 2007 into total tax
  if ( all(any(as.numeric(irs$year) >= 2007) & any(as.numeric(irs$year) < 2007)) ) {
    irs[, total_tax :=
          data.table::fcase(
            as.numeric(year) < 2007, a09200,
            as.numeric(year) >= 2007, a10300)]
    } else if ( all(as.numeric(irs$year) < 2007) ) {
      irs[, total_tax := a09200]
    } else {
      irs[, total_tax := a10300]
    }

  # Set keys
  data.table::setkeyv(irs, c("year", "zipcode"))

  #Return
  return(irs)

}
