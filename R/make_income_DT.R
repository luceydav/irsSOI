#' Function to take incoming reactive data.frame, convert to data.table and build Income DT
#'
#' @description
#' Makes a Income datatable of Salary, Divs/Int, Cap Gains and Unemploy. by year
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
make_income_DT <- function(data, type = "agi") {

  # data <-
  #fst::read_fst("/Users/davidlucey/Desktop/David/Projects/irs_soi_app/data/irs_app_big.fst")

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
          salary = sum(as.numeric(a00200), na.rm = TRUE) / 1000000,
          div_int = (sum(as.numeric(a00300), na.rm = TRUE) + sum(as.numeric(a00600), na.rm = TRUE)) / 1000000,
          gains = sum(as.numeric(a01000), na.rm = TRUE) / 1000000,
          business_partner = (sum(as.numeric(a00900), na.rm = TRUE) + sum(as.numeric(a26270), na.rm = TRUE)) / 1000000,
          pension_ss = (sum(as.numeric(a01700), na.rm = TRUE) + sum(as.numeric(a02500), na.rm = TRUE)) / 1000000
        ),
        by = year]

  } else {

    data <-
      data[,
        { salary = sum(as.numeric(a00200), na.rm = TRUE) / 1000000
          div_int = (sum(as.numeric(a00300), na.rm = TRUE) + sum(as.numeric(a00600), na.rm = TRUE)) / 1000000
          gains = sum(as.numeric(a01000), na.rm = TRUE) / 1000000
          business_partner = (sum(as.numeric(a00900), na.rm = TRUE) + sum(as.numeric(a26270), na.rm = TRUE)) / 1000000
          pension_ss = (sum(as.numeric(a01700), na.rm = TRUE) + sum(as.numeric(a02500), na.rm = TRUE)) / 1000000
          n1 = sum(as.numeric(n1), na.rm=TRUE) / 1000000
        list(salary_cap = salary / n1,
             div_int_cap = div_int/ n1,
             gains_cap = gains/ n1,
             business_partner = business_partner/ n1,
             pension_ss_cap = pension_ss / n1)
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
      glue::glue("Salary {type}{scale}"),
      glue::glue("Div & Interest {type}{scale}"),
      glue::glue("Cap. Gains {type}{scale}"),
      glue::glue("Bus/Ptnr Inc {type}{scale}"),
      glue::glue("Pens/Soc Sec. {type}{scale}")
    ),
    options =
      list(
        pageLength = 16,
        scrollY = TRUE,
        dom = 't'
      ),
    caption = htmltools::tags$caption(
      style = 'caption-side: top; text-align: center;',
      '', htmltools::em(glue::glue('Annual {scope} Salary, Dividend & Interest, Cap. Gains, Business/Pship Inc, Pension/Soc. Sec Inc. by Selection'))
  ),
    rownames = FALSE
  ) %>%
    DT::formatRound(
      columns = c(2:6),
      mark = ",",
      digits = digits
    )
}
