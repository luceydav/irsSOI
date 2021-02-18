
#' Plotly graph of per capita items
#'
#' @description
#' Takes cleaned IRS data.table and makes Plotly graph of aggregated income divided by returns by selection
#' using output of [clean_soi()]
#'
#' @param data IRS data.table
#' @param type chr vector to specify if agi or per_cap
#'
#' @examples
#' \dontrun{
#' make_agi_graph(irs)}
#'
#' @export
make_agi_graph <- function(data, type = "agi") {

  # data <-
  #  fst::read_fst("/Users/davidlucey/Desktop/David/Projects/irs_soi_app/data/irs_app_data.fst")
  # Convert data to data.table if not one
  if (!data.table::is.data.table(data) ){
    data <- data.table::setDT(data)
  }

  # Choose chart type function
  if (type == "agi") {
    data <- group_agg_calc(data, soi_number = "a00100", soi_count = "n1")
    var <- names(data)[3]
    }
  if (type == "per_cap") {
    data <- per_cap_agg_calc(data, soi_number = "a00100", soi_count = "n1")
    var <- names(data)[3]
    }

  # Plotly graph
  plotly::layout(
    data[,
      plotly::plot_ly(
        .SD,
        x = ~ Year,
        y = ~ get(var),
        color =  ~ `Income Group`,
        type = "scatter",
        mode = "lines",
        showlegend = F,
        fill = ~ '',
        hoverinfo = 'text',
        text = ~ paste(
          '</br> Year: ',
          Year,
          '</br> Amount: ',
          paste0("$", format(get(var),
                             big.mark = ",",
                             digits = 2,
                             scientific = FALSE)),
          '</br> Income Group: ',
          `Income Group`,
          '</br> # of Returns: ',
          format(
            Returns,
            big.mark = ",",
            digits = 2,
            scientific = FALSE
          )
        )
      ) %>%
        plotly::add_markers(size = ~ Returns,
                            mode = "markers")],
    title = glue::glue("Annual Aggregate AGI {var} by Selected Group"),
    xaxis = list(
      title = "Year"
    ),
    yaxis = list(
      title = glue::glue("AGI {var}")
    ))
}
