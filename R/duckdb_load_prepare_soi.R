
#' @title Load and Prepare NBER SOI Data
#'
#' @description
#' Given folder of NBER Downloaded annual .csv's from [download_nber_data()], return a data.table
#' (https://www.nber.org/research/data/individual-income-tax-statistics-zip-code-data-soi)
#'
#' @param states A State ID Code
#' @param path The location of the .csv folder output of download_nber_data()
#'
#' @details
#' - Example of state would be "CT" or "FL"
#' - Directory path used for download_nber_data() (ie: "/home/..")
#'
#' @examples
#' \dontrun{
#' library(duckplyr)
#' irs <- duckdb_load_prepare_soi("~/Documents/Data/irs_zip_soi/")}
#'
#' @export
duckdb_load_prepare_soi <- function(states = "", path = "") {

  my_cols <- function() {
    c(
      "year",
      "zipcode",
      "state",
      "agi_level",
      "agi_level_2",
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
      "a09750",
      "n09750",
      "a04470",
      "a18425",
      "n18425",
      "a18450",
      "n18450",
      "a18500",
      "n18500",
      "a19300",
      "n19300",
      "a19700",
      "n19700",
      "a09600",
      "n09600",
      "a11000",
      "n11000",
      "a59660",
      "n59660",
      "a00600",
      "n00600",
      "a00300",
      "n00300",
      "a00900",
      "n00900",
      "a26270",
      "n26270",
      "a85300" ,
      "n85300",
      "a01700",
      "n01700",
      "a02500",
      "n02500",
      "taxpayers",
      "a18800",
      "n18800"
    )
  }

  if (states == "") {
    states <-
      c("MA", "RI", "NH", "ME","VT", "CT", "NY", "NJ", "PA", "DE", "DC",
        "VA", "MD", "WV", "NC", "SC", "GA", "FL","AL","TN", "MS","KY", "OH" ,
        "IN", "MI", "IA", "WI", "MN", "SD", "ND", "MT", "IL", "MO", "KS", "NE",
        "LA", "AR", "OK", "TX","CO", "WY", "ID", "UT", "AZ", "NM", "NV", "CA",
        "HI", "OR", "WA", "AK")
  }

  # Load and clean data
  data <-
    duckplyr::duckplyr_df_from_file(
      path = paste0(path, "/*.csv.gz"),
      table_function = "read_csv_auto",
      options = list(
        hive_partitioning = TRUE,
        filename = TRUE,
        union_by_name = TRUE,
        normalize_names = TRUE
      )
    ) |>
    duckplyr::filter(!zipcode %in%  c("00000", "99999", "0", "")) |>
    filter(state %in% states) |>
    duckplyr::mutate(year = as.integer(substr(filename, 53, 56))) |>
    duckplyr::mutate(duckplyr::across(
      dplyr::matches("a\\d+"),
      ~ duckplyr::case_when(year %in% c(2007:2008) ~ .x / 1000,
                                    .default = .x)
    )) |>
    duckplyr::mutate(
      state = toupper(state),
      total_tax = duckplyr::case_when(
        year <= 2006 ~ a09200,
        year > 2006 ~ a06500
      ),
      taxpayers = duckplyr::case_when(
        year <= 2006 ~ n09200,
        year > 2006 ~ n06500
      ),
      a59660 = duckplyr::case_when(
        year <= 2005 ~ a11000,
        year > 2005 ~ a59660
      ),
      n59660 = duckplyr::case_when(
        year <= 2005 ~ n11000,
        year > 2005 ~ n59660
      ),
      numdep = duckplyr::case_when(
        year <= 2006 ~ n6,
        year > 2006 ~ numdep
      ),
      zipcode = duckplyr::case_when(
        nchar(zipcode) == 3 ~ paste0("00", zipcode),
        nchar(zipcode) == 4 ~ paste0("0", zipcode),
        .default = zipcode
      ),
      agi_level = duckplyr::case_when(
        year <= 2008 & agi_class == 1  ~ "<$25k",
        year <= 2008 & agi_class == 2 ~  "<$25k",
        year <= 2008 & agi_class == 3 ~  "$25-50k",
        year <= 2008 & agi_class == 4 ~  "$50-75k",
        year <= 2008 & agi_class == 5 ~  "$75-100k",
        year <= 2008 & agi_class == 6 ~  "$100k+",
        year <= 2008 & agi_class == 7 ~ "$100k+",
        year > 2008 & agi_stub == 1 ~  "<$25k",
        year > 2008 & agi_stub == 2 ~  "$25-50k",
        year > 2008 & agi_stub == 3 ~  "$50-75k",
        year > 2008 & agi_stub == 4 ~  "$75-100k",
        year > 2008 & agi_stub == 5 ~  "$100k+",
        year > 2008 & agi_stub == 6 ~  "$100k+"
      ),
      agi_level_2 = duckplyr::case_when(
        agi_stub == 1 ~  "<$25k",
        agi_stub == 2 ~  "$25-50k",
        agi_stub == 3 ~  "$50-75k",
        agi_stub == 4 ~  "$75-100k",
        agi_stub == 5 ~  "$100k-200k",
        agi_stub == 6 ~  "$200k+",
        .default = NA
      )) |>
    #select(where(~mean(is.na(.)) < 0.7))
    select(my_cols())

  # Set keys and index
  data.table::setDT(data, key=c("year", "zipcode"))
  if (states != "") {
    data.table::setindex(data, cols = "state")
  }

  #Return
  return(data)

}
