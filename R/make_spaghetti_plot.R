
#' Plotly graph of comparing selected components
#'
#' @description
#' Takes cleaned IRS data.table and makes Plotly graph of aggregated income divided by returns by selection
#' using output of [clean_soi()]
#'
#' @param data IRS data.table
#'
#' @examples
#' \dontrun{
#' make_spaghetti_plot(irs_app_data, "zipcode")}
#'
#' @export
# spaghetti of every town of average sales price used in Shiny app
# town_name and property specified from Shiny ui
make_spaghetti_plot <- function(data) {

  # data <-
  #  fst::read_fst("/Users/davidlucey/Desktop/David/Projects/irs_soi_app/data/irs_app_data.fst")
  # Convert data to data.table if not one
  if (!data.table::is.data.table(data) ){
    data <- data.table::setDT(data)
  }

  # Change to smaller geography if only one level in chosen one
  entity <- "state"
  if ( length(unique(data$state)) == 1 ) {
    entity <- "county"
  }
  if ( length(unique(data$county)) == 1 ) {
    entity <- "post_office_city"
  }
  if (length(unique(data$post_office_city)) == 1 ) {
    entity <- "zipcode"
  }

  # Warning if too many levels in grouping variable
  if (length(unique(data[, get(entity)])) > 100 ) {
    print("Suggest narrowing down choices")
  }

  # Select cols needed for plot
  cols <- c("a00100", "year", "n1", entity)
  data <- data[, cols, with = FALSE]

  # Prepare data for chart if not zipcode
  col <- setdiff(cols, c("year", "a00100", "n1"))
  data <-
    data[,
         {
           total = sum(a00100, na.rm = TRUE)
           returns = sum(n1, na.rm = TRUE)
           agi_cap = total / returns
           list(agi_cap,
                returns)
         },
         by = c("year", col)]
  data.table::setnames(data, names(data), c("Year", "State", "AGI", "Returns"))
  entity <- stringr::str_to_title(entity)

  #Plotly function
  plotly::ggplotly(
    data[,
     ggplot2::ggplot(.SD,
                     ggplot2::aes_string(
                       x = "Year",
                       y = "AGI",
                       group = entity))+
       ggplot2::geom_line() +
       ggplot2::geom_point(ggplot2::aes(size = Returns)) +
       # geom_line(,
       #           aes(`Year`,
       #               `Average Price`,
       #               col = "red"),
                 # size = 0.5) +
       ggplot2::scale_y_continuous(trans = "pseudo_log"
                          #, labels = ks
                          ) +
       ggplot2::theme(
         axis.text.x = ggplot2::element_text(
           angle = 90,
           vjust = 0.5,
           hjust = 1
         ),
         legend.position = "none"
       )+
       ggplot2::labs(title = "",
            subtitle = "",
            caption = "Public data via IRS SOI",
            x = "Year",
            y = "Average Income per Cap - Log Scale ($K)") +
       ggplot2::theme_bw()]
  )

}
