
#' Function to take incoming reactive data.frame, convert to data.table and build Summary DT
#'
#' @description
#' Makes a Summary datatable of AGI, Federal Tax, Total returns and Unique Zips by year
#' using output of [clean_soi()]
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
make_summary_DT <- function(data, type = "agi") {

  # data <-
  #  fst::read_fst("/Users/davidlucey/Desktop/David/Projects/irs_soi_app/data/irs_app_data.fst")

  # Convert data to data.table if not one
  if (!data.table::is.data.table(data) ){
    data <- data.table::setDT(data)
  } else { data <- copy(data) }

  # Set scale as filtered down to zipcode
  if (length(data$zipcode) < 500 & type == "agi") {
    digits <- 3
  } else {
    digits <- 1
  }

  if( type == "agi") {

      data <- data[,
        list(
          tot_agi = sum(as.numeric(a00100), na.rm = TRUE) / 1000000,
          tot_tax = sum(as.numeric(total_tax), na.rm = TRUE) /
            1000000,
          tot_returns = sum(as.numeric(n1), na.rm = TRUE) / 1000000,
          unique_zips = length(unique(zipcode))
        ),
        by = year]

  } else {

      data <- data[,
        { tot_agi = sum(as.numeric(a00100), na.rm = TRUE) / 1000000
        tot_tax = sum(as.numeric(total_tax), na.rm = TRUE) /
          1000000
        tot_returns = sum(as.numeric(n1), na.rm = TRUE) / 1000000
        unique_zips = length(unique(zipcode))
        n1 = sum(as.numeric(n1), na.rm=TRUE) / 1000000
        list(agi_cap = tot_agi / n1,
             tax_cap = tot_tax / n1,
             tot_returns,
             unique_zips)
        },
        by = year]

  }

  # Fix labels
  scale <- ifelse(type == "per_cap", "$k", "$B")
  scope <- ifelse(type == "per_cap", "Per Cap", "Aggregated Total")
  type <- ifelse(type == "per_cap", "Per Capita ", "")

  # Table
  # https://taxfoundation.org/federal-tax-revenue-source-1934-2018/
  DT::datatable(
    data,
    colnames = c(
      "Year",
      glue::glue("AGI {type}{scale}"),
      glue::glue("Fed'l Tax {type}{scale}"),
      "Returns (m)",
      "Zips"
    ),
    options =
      list(
        pageLength = 15,
        scrollY = TRUE,
        dom = 't'
      ),
    caption = htmltools::tags$caption(
      style = 'caption-side: top; text-align: center;',
      '', htmltools::em(glue::glue('Annual {scope} AGI, Federal Tax, Total Returns and Unique Zips by Selection'))
    ),
    rownames = FALSE
  ) %>%
    DT::formatRound(
      columns = c(2:4),
      mark = ",",
      digits = digits
    ) %>%
    DT::formatRound(
      columns = 5,
      mark = ",",
      digits = 0
    )
}
