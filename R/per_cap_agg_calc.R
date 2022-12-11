
#' Aggregates Per Capita Amount
#'
#' @description
#' Takes full data and aggregates by requested group variable with inputs
#'
#' @param data IRS data.table
#' @param soi_number Income or deduction chr value
#' @param soi_count Returns reporting income or deduction chr value
#'
#' @import data.table
#'
#' @export
per_cap_agg_calc <- function(data, soi_number = "a00100", soi_count = "n1") {

  # Copy data and select cols needed for plot
  cols <- c("agi_level", "year", soi_number, soi_count)
  data <- data.table::copy(data)[, cols, with = FALSE]

  return(
    data[,
          { agg_soi_number = sum(get(soi_number), na.rm = TRUE)
          agg_soi_count = sum(get(soi_count), na.rm = TRUE)
          per_capita = agg_soi_number / agg_soi_count
          list(`Per Capita` = per_capita,
                Returns = agg_soi_count) },
          by = list(Year = year,
                    `Income Group` = agi_level)]
  )
}
