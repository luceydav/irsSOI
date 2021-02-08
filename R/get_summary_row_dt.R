# Function to take summary rows and determine which are in all years
get_summary_row_dt <- function(irs) {
  
  dt_zips <- 
    copy(irs)[, .(zipcode, year, n1)]
  
  #Set key cols
  setkey(dt_zips, year)
  
  dt_zips[, `:=`(
    zipcode = 
      fifelse(
        !str_detect(zipcode, "^\\d{4}$"),
        zipcode,
        paste0("0", zipcode)))]
  
  #Remove pre-formatted summary rows
  dt_zips <-
    dt_zips[zipcode %chin% c("00000", "99999", "0")]
  
  return(dt_zips)
}