
#' Function to take incoming reactive data.frame, convert to data.table and build Summary DT
#'
#' @description
#' Makes a Summary datatable of AGI, Federal Tax, Total returns and Unique Zips by year
#' using output of [clean_soi()]
#'
#' @param data IRS data.table
#'
#' @importFrom stats setNames
#'
#'

make_summary_DT <- function(data) {

  if (length(data$zipcode) < 50) {
    digits <- 3
  } else {
    digits <- 1
  }

  # https://taxfoundation.org/federal-tax-revenue-source-1934-2018/
  DT::datatable(
    data.table::setDT(data)[,
      list(
        tot_agi = sum(as.numeric(a00100), na.rm = TRUE) / 1000000,
        tot_tax = sum(as.numeric(total_tax), na.rm = TRUE) /
          1000000,
        tot_returns = sum(as.numeric(n1), na.rm = TRUE) / 1000000,
        unique_zips = length(unique(zipcode))
      ),
      by = year],
    colnames = c(
      "Year",
      "Total AGI ($B)",
      "Federal Tax ($B)",
      "Total Returns (m)",
      "Unique Zips"
    ),
    options =
      list(
        pageLength = 15,
        scrollY = TRUE,
        dom = 't'
      ),
    caption = htmltools::tags$caption(
      style = 'caption-side: top; text-align: center;',
      '', htmltools::em('Annual Aggregated AGI, Federal Tax, Total Returns and Unique Zipcodes by Selection')
    ),
    rownames = FALSE
  ) %>%
    DT::formatRound(
      columns = c(2:3),
      mark = ",",
      digits = digits
    )  %>%
    DT::formatRound(
      columns = 4,
      mark = ",",
      digits = digits
    )%>%
    DT::formatRound(
      columns = 5,
      mark = ",",
      digits = 0
    )
}
