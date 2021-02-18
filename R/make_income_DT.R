#' Function to take incoming reactive data.frame, convert to data.table and build Income DT
#'
#' @description
#' Makes a Income datatable of Salary, Divs/Int, Cap Gains and Unemploy. by year
#'
#' @param data IRS data.table
#'
#' @importFrom stats setNames
#'
#' @export
#'
make_income_DT <- function(data) {
  #data <- fst::read_fst("/Users/davidlucey/Desktop/David/Projects/irs_soi_app/data/irs_app_big.fst")

  if (length(data$zipcode) < 25) {
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
        business_partner = (sum(as.numeric(a00900), na.rm = TRUE) + sum(as.numeric(a26270), na.rm = TRUE)) / 1000000,
        pension_ss = (sum(as.numeric(a01700), na.rm = TRUE) + sum(as.numeric(a02500), na.rm = TRUE)) / 1000000
      ), by = year],
    colnames = c(
      "Year",
      "Salary ($B)",
      "Div & Interest ($B)",
      "Cap. Gains ($B)",
      "Bus/Ptnr Inc ($B)",
      "Pens/Soc Sec. ($B)"
    ),
    options =
      list(
        pageLength = 15,
        scrollY = TRUE,
        dom = 't'
      ),
    caption = htmltools::tags$caption(
      style = 'caption-side: top; text-align: center;',
      '', htmltools::em('Annual Aggregated Salary, Dividend & Interest, Cap. Gains and Unemployment by Selection')
    ),
    rownames = FALSE
  ) %>%
    DT::formatRound(
      columns = c(2:6),
      mark = ",",
      digits = digits
    )  %>%
    DT::formatRound(
      columns = 5,
      mark = ",",
      digits = 0
    )
}
