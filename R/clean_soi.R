
clean_soi <- function(irs_raw) {

  irs <- data.table::copy(irs_raw)

  #Filter duplicate year column from post 2008
  irs[, 100:=NULL]

  #Set key cols
  data.table::setkey(irs, zipcode)

  #Remove pre-formatted summary rows
  irs <-
    irs[!zipcode %chin% c("00000", "99999", "0", "")]

  ## Fill in bad agi_class allocations in 2006 data
  irs[as.integer(year) < 2008 &
        as.integer(agi_class) > 7,
      agi_class := calc_agi(a00100, n1)]

  # Coalesce diff dependent variable name from pre 2007
  irs[, numdep := data.table::fcoalesce(numdep, n6)]

  # Drop cols
  irs[, `:=`(
    n6 = NULL,
    statefips = NULL,
    agi_04470 = NULL,
    agi_19700 = NULL,
    agi_18300 = NULL
  )]

  ## Convert all state abbreviations to upper case and all zipcodes to 5-digit
  irs[, `:=`(
    zipcode =
      fifelse(
        !stringr::str_detect(zipcode, "^\\d{4}$"),
        zipcode,
        paste0("0", zipcode)))]

  # Convert year, zipcode to numeric
  #id_cols <- c("year", "zipcode")
  #irs[, (id_cols) := lapply(.SD, as.numeric), .SDcols = id_cols]

  #Fix unequal income brackets
  irs[as.integer(year) <= 2008,
      agi_level := convert_agi(agi_class)]
  irs[as.integer(year) > 2008,
      agi_level := convert_agi_2(agi_stub)]
  irs[, `:=`(agi_class = NULL, agi_stub = NULL)]

  # Aggregate rows by same agi_level, year, zipcode
  num <-
    data.table::fsetdiff(names(irs), c("agi_level", "year", "zipcode", "state"))
  states <- data.table::unique(irs[, .(zipcode, state)], by = "zipcode")
  irs <-
    irs[, lapply(.SD, sum, na.rm = TRUE),
        .SDcols = num,
        by = .(year, zipcode, agi_level)]
  irs <- states[irs, on = "zipcode"]
  irs[, state := toupper(state)]

  ## Divide numeric amount columns by 1000 for 2007 and 2008 to equalize with other years
  cols <- names(irs)[stringr::str_which(names(irs), "^a\\d{5}")]
  irs[year %in% c("2007", "2008"),
      (cols) := lapply(.SD, function(x)
        x / 1000), .SDcols = cols]

  ##Merge total tax variables from pre and post 2007 into total tax
  irs[, total_tax :=
        data.table::fcase(as.numeric(year) < 2007, a09200,
              as.numeric(year) >= 2007, a10300)]

  # Set keys
  data.table::setkeyv(irs, c("year", "zipcode"))

  #Return
  return(irs)

}
