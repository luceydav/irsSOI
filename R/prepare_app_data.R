#' Function to select cols and add city/county to irs for use in [irsApp()]
#'
#' @description
#' Helper to
#'
#' @param irs Cleaned IRS data.table from [clean_soi()]
#'
#' @examples
#' \dontrun{library(data.table)
#' irs_app_data <- prepare_app_data(irs)}
#'
#' @export
prepare_app_data <- function(irs) {

  data <- data.table::copy(irs)

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
    data.table::setDT(zipcodeR::zip_code_db)[population > 0, ][,
    ][,list(
      zipcode,
      zipcode_type,
      post_office_city,
      county,
      major_city,
      population
    )]

  # Selcct cols from full IRS dataset
  data <- data[, .SD, .SDcols = cols][
    !zipcode %chin% data[, .SD[sum(n1) < 100],
                      list(zipcode, year)]$zipcode]

  # Join to add county, city and population
  data <- zips[data, on = "zipcode"]

  # Adjust cities with missing post office
  data[is.na(post_office_city),
    post_office_city := glue::glue_data(.SD, "Smaller City, {state}")]
  data[, county := data.table::fifelse(
    is.na(county),
    glue::glue_data(.SD, "Smaller County, {state}"),
    glue::glue_data(.SD, "{county}, {state}")
  )]

  return(data)
}