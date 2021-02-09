
#' Plotly graph of per capita items
#'
#' @description
#' Takes cleaned IRS data.table and makes Plotly graph of aggregated income divided by returns by selection
#' using output of [clean_soi()]
#'
#' @param data IRS data.table
#'
#' @examples
#' \dontrun{
#' make_agi_graph(irs)}
#'
#' @export
make_agi_graph <- function(data) {

  plotly::layout(
    data.table::setDT(data)[,
      {
        agi_sum = sum(a00100, na.rm = TRUE)
        total = sum(n1, na.rm = TRUE)
        agi_return = agi_sum / total
        list(`Income Amount` = agi_return ,
             `Returns` = total)
      },
      by = list(Year = year,
             `Income Group` = agi_level)][,
      plotly::plot_ly(
        .SD,
        x = ~ Year,
        y = ~ `Income Amount`,
        color =  ~ `Income Group`,
        type = "scatter",
        mode = "lines",
        showlegend = F,
        fill = ~ '',
        hoverinfo = 'text',
        text = ~ paste(
          '</br> Year: ',
          Year,
          '</br> After Tax Income: ',
          paste0("$", format(`Income Amount`,
                             digits = 2), "k"),
          '</br> Income Group: ',
          `Income Group`,
          '</br> # of Returns: ',
          format(
            `Returns`,
            big.mark = ",",
            digits = 2,
            scientific = FALSE
          )
        )
      ) %>%
        plotly::add_markers(size = ~ `Returns`,
                            mode = "markers")],
    title = "Annual Aggregate AGI Per Capita by Selected Group"
    ,
    xaxis = list(
      title = "Year"
    ),
    yaxis = list(
      title = "AGI per Capita ($k)"
    ))
}
