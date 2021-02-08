# Function to select cols and add cities for app

prepare_app_data <- function(full_irs_clean) {
  
  d <- copy(full_irs_clean)
  
  cols <-
    c(
      "year",
      "zipcode",
      "state",
      "agi_level",
      "a00100",
      "a00200",
      "n00200",
      "total_tax",
      "n1",
      "n2",
      "numdep",
      "a01000",
      "n01000",
      "a02300",
      "n02300",
      "a59660",
      "a09750",
      "a04470",
      "n02500",
      "a18425",
      "n18425",
      "a18450",
      "n18450",
      "a18500",
      "n18500",
      "a19300",
      "n19300",
      "a00600",
      "n00600",
      "a00300",
      "n00300"
    )
  
  # Load zips
  zips <-
    setDT(zipcodeR::zip_code_db)[population > 0, ][, 
      ][,.(
        zipcode,
        zipcode_type,
        post_office_city,
        county,
        major_city,
        population
      )]
  
  # Selcct cols from full_irs_clean
  d <- d[, ..cols][
    !zipcode %chin% d[, .SD[sum(n1) < 100], 
                      .(zipcode, year)]$zipcode]
  #d[, zipcode :=
  #   fifelse(zipcode <= 9999,
    #          paste0("0", zipcode),
    #          as.character(zipcode))]
  
  # Join to add county, city and population
  d <- zips[d, on = "zipcode"]
  
  # Adjust cities with missing post office
  d[is.na(post_office_city), 
    post_office_city := glue_data(.SD, "Smaller City, {state}")]
  d[, county := fifelse(
    is.na(county),
    glue_data(.SD, "Smaller County, {state}"),
    glue_data(.SD, "{county}, {state}")
  )]  
  
  return(d)
}