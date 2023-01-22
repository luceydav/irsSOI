
#' Function to take incoming reactive data.frame, convert to data.table and build Summary DT
#'
#' @description
#' Makes a Summary datatable of AGI, Federal Tax, Total returns and Unique Zips by year
#' using output of [prepare_app_data()]
#'
#' @param data IRS data.table
#' @param type chr specifying how to display data
#'
#' @import data.table
#' @import DT
#' @import htmltools
#' @importFrom stats setNames
#' @importFrom glue glue
#'
#' @export
make_deductions_DT <- function(data, type = "agi") {

  # data <-
  #  fst::read_fst("/Users/davidlucey/Desktop/David/Projects/irs_soi_app/data/irs_app_big.fst")

  # Convert data to data.table if not one
  if (!data.table::is.data.table(data) ){
    data <- data.table::setDT(data)
  } else { data <- copy(data) }

  if (length(data$zipcode) < 500) {
    digits <- 3
  } else {
    digits <- 1
  }

if( type == "agi") {

  data <-
    data[,
         list(
           total_deduct = sum(as.numeric(a04470), na.rm = TRUE) / 1000000,
           state_tax = (
             sum(as.numeric(a18425), na.rm = TRUE) / 1000000 + sum(as.numeric(a18450), na.rm = TRUE) / 1000000),
           re_tax = sum(as.numeric(a18500), na.rm = TRUE) / 1000000,
           mortgage = sum(as.numeric(a19300), na.rm = TRUE) / 1000000,
           charity = sum(as.numeric(a19700), na.rm = TRUE) / 1000000
         ),
         by = year]

} else {

  data <-
    data[,
         { total_deduct = sum(as.numeric(a04470), na.rm = TRUE) / 1000000
           state_tax = (
              sum(as.numeric(a18425), na.rm = TRUE) / 1000000 + sum(as.numeric(a18450), na.rm = TRUE) / 1000000)
           re_tax = sum(as.numeric(a18500), na.rm = TRUE) / 1000000
           mortgage = sum(as.numeric(a19300), na.rm = TRUE) / 1000000
           charity = sum(as.numeric(a19700), na.rm = TRUE) / 1000000
           n1 = sum(as.numeric(n1), na.rm=TRUE) / 1000000
         list(total_deduct_cap = total_deduct / n1,
              state_tax_cap = state_tax/ n1,
              re_tax_cap = re_tax/ n1,
              mortgage_cap = mortgage/ n1,
              charity_cap = charity / n1)
         },
         by = year]

  }

  # Fix labels
  scale <- ifelse(type == "per_cap", "$k", "$B")
  scope <- ifelse(type == "per_cap", "Per Cap", "Aggregated Total")
  type <- ifelse(type == "per_cap", "Per Capita ", "")

  # https://taxfoundation.org/federal-tax-revenue-source-1934-2018/
  DT::datatable(
    data,
    colnames = c(
      "Year",
      glue::glue("Total Deduct. {type}{scale}"),
      glue::glue("State Taxes {type}{scale}"),
      glue::glue("Local (RE) Tax {type}{scale}"),
      glue::glue("Mortgage Int. {type}{scale}"),
      glue::glue("Charitable {type}{scale}")
    ),
    options =
      list(
        pageLength = 16,
        scrollY = TRUE,
        dom = 't'
      ),
    caption = htmltools::tags$caption(
      style = 'caption-side: top; text-align: center;',
      '', htmltools::em(glue::glue('Annual {scope} Deductions, State Tax, Local Tax, Mortgage Int. & Charitable Donations by Selection'))
    ),
    rownames = FALSE
    ) %>%
    DT::formatRound(
      columns = c(2:6),
      mark = ",",
      digits = digits
    )
}
