#' Function to select cols and add city/county
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
#' @import data.table
#' @importFrom glue glue_data
#'
#' @export
prepare_app_data <- function(irs) {

  data <- data.table::copy(irs)

  cols <-
    c(
      "year",
      "zipcode",
      "zipcode_type",
      "state",
      "county",
      "major_city",
      "agi_level",
      "population",
      "post_office_city",
      "a00100", # agi
      "a00200", # salary
      "n00200",
      "total_tax", # derived
      "n1",
      "n2", # exemptions
      "numdep",
      "a01000", # gains
      "n01000",
      "a02300", # unemploy
      "n02300",
      "a09750", # healthcare indiv
      "n09750",
      "a04470", # total item deds
      "a18425", # sl tax
      "n18425",
      "a18450", # sales tax
      "n18450",
      "a18500", # RE tax
      "n18500",
      "a19300", # mortgage
      "n19300",
      "a19700", # contributions
      "n19700",
      "a09600", # amt
      "n09600",
      "a11000", # eitc (early)
      "n11000",
      "a59660", # eitc (late)
      "n59660",
      "a00600", # dividends
      "n00600",
      "a00300", # interest
      "n00300",
      "a00900", # business inc
      "n00900",
      "a26270", # pship/scorp net inc
      "n26270",
      "a85300", # net inv income tax (late)
      "n85300",
      "a01700", # pension/annuity
      "n01700",
      "a02500", # soc sec
      "n02500"
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

  # Join to add county, city and population
  data <- zips[data, on = "zipcode"]

  # Select specified columns, then filter any zip codes where
  # number of returns < 100 in any year in specified year range
  data <- data[, .SD, .SDcols = cols][
    !zipcode %chin% data[, .SD[sum(n1) < 100],
                         list(zipcode, year)]$zipcode]

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
