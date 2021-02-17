
#' Plotly graph of comparing selected components
#'
#' @description
#' Takes cleaned IRS data.table and makes Plotly graph of aggregated income divided by returns by selection
#' using output of [clean_soi()]
#'
#' @param data IRS data.table
#' @param entity state, county, post_office_city, zipcode
#'
#' @examples
#' \dontrun{
#' make_spaghetti_plot(irs_app_data, "zipcode")}
#'
#' @export
# spaghetti of every town of average sales price used in Shiny app
# town_name and property specified from Shiny ui
make_spaghetti_plot <- function(data, entity = "state"){

  if (entity == "state" & length(unique(data$state) == 1) ) {
    entity <- "county"
  } else if (length(unique(data$county == 1 ))) {
    entity <- "post_office_city"
  }

  if (nrow(unique(data[, entity, with = FALSE])) > 100 ) {
    print("Suggest narrowing down choices")
  }

  cols <- c("a00100", "year", "n1", entity)
  data <- data[, cols, with = FALSE]

  # Prepare data for chart
  col <- setdiff(cols, c("year", "a00100", "n1"))
  data <-
    data[,
       { total = sum(a00100, na.rm = TRUE)
         returns = sum(n1, na.rm = TRUE)
         agi_cap = total /returns
         list(agi_cap,
              returns)},
        by = c("year", col)]

  # Change name of selected col
  # data.table::setnames(
  #   data,
  #   c("year", entity),
  #   c("Year", stringr::str_to_title(entity)))

  #data[, (col) := lapply(.SD, as.factor), .SDcols = col]
  # ks <- function(x) { scales::number_format(accuracy = 1,
  #                                    scale = 1/1000,
  #                                    suffix = "k",
  #                                    big.mark = ",")(x) }

  #data <- data[, data.table::melt(.SD, idvars= c("Year", entity))]

  plotly::ggplotly(
    data[,
     ggplot2::ggplot(.SD,
                     ggplot2::aes_string(
                       x = "year",
                       y = "agi_cap",
                       group = entity))+
       ggplot2::geom_line() +
       ggplot2::geom_point(ggplot2::aes(size = returns)) +
       # geom_line(,
       #           aes(`Year`,
       #               `Average Price`,
       #               col = "red"),
                 # size = 0.5) +
       ggplot2::scale_y_continuous(trans = "log10"
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
            subtitle = "Selected municipality shown in red",
            caption = "Public data via IRS SOI",
            x = "Year",
            y = "Average Income per Cap - Log Scale ($K)") +
       ggplot2::theme_bw()]
  )

}
