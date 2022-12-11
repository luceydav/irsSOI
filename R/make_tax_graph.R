#' Plotly graph of per tax rate
#'
#' @description
#' Takes cleaned IRS data.table and makes Plotly graph of tax rate by selection
#' using output of [clean_soi()]
#'
#' @param data IRS data.table
#'
#' @examples
#' \dontrun{
#' make_tax_graph(irs)}
#'
#' @import data.table
#' @import plotly
#'
#' @export
make_tax_graph <- function(data) {

  plotly::layout(
    data.table::setDT(data)[,
      {
        agi_sum = sum(a00100 , na.rm = TRUE)
        tot_tax = sum(total_tax, na.rm = TRUE)
        tax_rate = tot_tax / agi_sum
        total = sum(n1 , na.rm = TRUE)
        list(`Returns` = total,
             `Tax Rate` = tax_rate)
      },
      by = list(Year = year, `Income Group` = agi_level)][,
        plotly::plot_ly(
          .SD,
          x = ~ Year,
          y = ~ `Tax Rate`,
          color =  ~ `Income Group`,
          type = "scatter",
          mode = "lines",
          fill = ~'',
          showlegend = F,
          hoverinfo = 'text',
          text = ~ paste(
            '</br> Year: ',
            Year,
            '</br> Tax Rate: ',
            paste0(format(`Tax Rate` * 100,
                          digits = 2), "%"),
            '</br> Income Group: ',
            `Income Group`,
            '</br> # of Returns: ',
            format(
              `Returns`,
              big.mark = ",",
              #digits = 0,
              scientific = FALSE
            )
          )
        ) %>%
          plotly::add_markers(size = ~ `Returns`,
                          mode = "markers")],
    title = "Annual Effective Tax Rate by Selected Group",
    xaxis = list(
      title = "Year"
    ),
    yaxis = list(
      title = "Effective Tax Rate (%)"
    )
  )
}
