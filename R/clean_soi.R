
#' Helper function to clean raw IRS SOI data loaded with load_soi()
#'
#' @description
#' Cleans raw IRS annual data from [load_soi()] by removing summary rows,
#' aggregating income levels and coalescing number of return columns,
#' which vary from early to later years and number of return columns
#'
#' @param irs_raw Uncleaned data.table of annual tax data
#'
clean_soi <- function(irs_raw) {

  # Make copy
  irs_raw <- data.table::copy(irs_raw)

  # Filter duplicate year column from post 2008 if exists
  irs_raw <- irs_raw[, .SD, .SDcols = unique(names(irs_raw))]

  # Set key cols
  data.table::setkey(irs_raw, zipcode)

  # Remove pre-formatted summary rows
  irs_raw <- irs_raw[!zipcode %chin% c("00000", "99999", "0", "")]

  ## Fill in bad agi_class allocations in 2006 data
  if ("agi_class" %chin% names(irs_raw)) {
    irs_raw[as.integer(year) < 2008 &
          as.integer(agi_class) > 7,
        agi_class := calc_agi(a00100, n1)]
  }

  # Coalesce multiple dependent fields across years
  if ("n6" %chin% names(irs_raw) & "numdep" %chin% names(irs_raw)) {
    # Coalesce diff dependent variable name from pre 2007
    irs_raw[, numdep := data.table::fcoalesce(numdep, n6)]
  } else if ("n6" %chin% names(irs_raw)) {
    data.table::setnames(irs_raw, "n6", "numdep")
  }

  ## Convert all state abbreviations to upper case and all zipcodes to 5-digit
  irs_raw[
    , zipcode := data.table::fifelse(
        !re2::re2_detect(zipcode, "^\\d{4}$"),
        zipcode,
        paste0("0", zipcode)
        )]

  #Fix unequal income brackets
  if ("agi_class" %in% names(irs_raw)) {
    irs_raw[as.integer(year) <= 2008,
        agi_level := convert_agi(agi_class)]
  }
  if ("agi_stub" %chin% names(irs_raw)) {
    irs_raw[as.integer(year) > 2008,
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
  drops <- names(irs_raw)[names(irs_raw) %in% cols]
  if (length(drops) > 0) {
    irs_raw[, c(drops) := NULL]
  }

  # Aggregate rows by same agi_level, year, zipcode
  num <-
    setdiff(names(irs_raw), c("agi_level", "year", "zipcode", "state"))
  states <-
    unique(irs_raw[, list(zipcode, state)], by = "zipcode")
  irs_raw <-
    irs_raw[, lapply(.SD, sum, na.rm = TRUE),
        .SDcols = num,
        by = list(year, zipcode, agi_level)]
  irs_raw <- states[irs_raw, on = "zipcode"]
  irs_raw[, state := toupper(state)]

  # Remove if agi_level is NA (only a few rows)
  irs_raw <- irs_raw[!is.na(agi_level)]

  # Filter only zipcodes where all periods/agi_levels present
  complete_zipcode <- length(unique(irs_raw$year)) * 5
  irs_raw <- irs_raw[, .SD[.N == complete_zipcode], zipcode]

  ## Divide numeric amount columns by 1000 for 2007 and 2008 to equalize with other years
  cols <- names(irs_raw)[re2::re2_which(names(irs_raw), "^a\\d{5}")]
  if (any(c("2016", "2008") %chin% unique(irs_raw$year))) {
    irs_raw[year %in% c("2007", "2008"),
        (cols) := lapply(.SD, function(x)
          x / 1000), .SDcols = cols]
  }

  ##Merge total tax variables from pre and post 2007 into total tax
  if (all(any(as.numeric(irs_raw$year) >= 2007) & any(as.numeric(irs_raw$year) < 2007))) {
    irs_raw[, total_tax :=
          data.table::fcase(
            as.numeric(year) < 2007, a09200,
            as.numeric(year) >= 2007, a10300)]
    } else if (all(as.numeric(irs_raw$year) < 2007)) {
      irs_raw[, total_tax := a09200]
    } else {
      irs_raw[, total_tax := a10300]
    }

  # Set keys
  data.table::setkeyv(irs_raw, c("year", "zipcode"))

  #Return
  return(irs_raw)

}
