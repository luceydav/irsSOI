
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
#' @export
#'
make_deductions_DT <- function(data) {
  #data <- fst::read_fst("/Users/davidlucey/Desktop/David/Projects/irs_soi_app/data/irs_app_big.fst")

  if (length(data$zipcode) < 50) {
    digits <- 3
  } else {
    digits <- 1
  }

  # https://taxfoundation.org/federal-tax-revenue-source-1934-2018/
  DT::datatable(
    data.table::setDT(data)[,
      list(
        total_deduct = sum(as.numeric(a04470), na.rm = TRUE) / 1000000,
        state_tax = (
          sum(as.numeric(a18425), na.rm = TRUE) / 1000000 + sum(as.numeric(a18450), na.rm = TRUE) / 1000000),
          re_tax = sum(as.numeric(a18500), na.rm = TRUE) / 1000000,
          mortgage = sum(as.numeric(a19300), na.rm = TRUE) / 1000000
        ), by = year],
    colnames = c(
      "Year",
      "Total Deduct. ($B)",
      "State Taxes ($B)",
      "Local (RE) Tax ($B)",
      "Mortgage Int. ($B)"
    ),
    options =
      list(
        pageLength = 15,
        scrollY = TRUE,
        dom = 't'
      ),
    caption = htmltools::tags$caption(
      style = 'caption-side: top; text-align: center;',
      '', htmltools::em('Annual Aggregated Total Deductions, State Taxes, Local Taxes and Mortgage Int. by Selection')
    ),
    rownames = FALSE
    ) %>%
    DT::formatRound(
      columns = c(2:5),
      mark = ",",
      digits = digits
    )
}
