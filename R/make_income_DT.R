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
make_income_DT <- function(data) {
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
        salary = sum(as.numeric(a00200), na.rm = TRUE) / 1000000,
        div_int = (sum(as.numeric(a00300), na.rm = TRUE) + sum(as.numeric(a00600), na.rm = TRUE)) / 1000000,
        gains = sum(as.numeric(a01000), na.rm = TRUE) / 1000000,
        unemploy = sum(as.numeric(a02300), na.rm = TRUE) / 1000
      ), by = year],
    colnames = c(
      "Year",
      "Salary ($B)",
      "Div & Interest ($B)",
      "Cap. Gains ($B)",
      "Unempl.($m)"
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
