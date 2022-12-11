
#' Import NBER IRS Annual Data Dictionary
#'
#' @description
#' Given a year, scrape NBER dictionary and return a data.table
#' (https://www.nber.org/research/data/individual-income-tax-statistics-zip-code-data-soi)
#'
#' @param year int year between 2005-2016
#'
#' @examples
#' \dontrun{
#' dict <- get_dict(2016)}
#'
#' @importFrom stats setNames
#' @importFrom glue glue
#' @importFrom rvest html_node html_text
#' @importFrom re2 re2_detect re2_which re2_replace
#' @importFrom xml2 read_html
#'
#' @export
get_dict <- function(year) {

  # Set URL based on year
  url <-
    glue::glue('https://data.nber.org/tax-stats/zipcode/{year}/desc/zipcode{year}/desc.txt')

  # Scrape dictionary
  txt <- xml2::read_html(url) %>%
    rvest::html_node("body > p") %>%
    rvest::html_text()
  fil <- tempfile(fileext = ".data")
  cat(txt, file = fil,
      sep = "\n")
  d <- readLines(fil, n = -1L)
  unlink(fil)

  # Filter needed rows
  d1 <- d[(re2::re2_which(d, "^state:") - 1):(re2::re2_which(d, "obs:") -2)]
  d1 <- d1[re2::re2_detect(d1, "")]
  col_2 <- d1[re2::re2_detect(d1, "^\\s")]
  col_2 <- re2::re2_replace(col_2, "^\\s*", "")
  col_2 <- col_2[re2::re2_detect(col_2, "^[1-9]")]

  # Build lists of description bullets
  row_list <- list()
  row_lists <- list()
  for (i in seq_along(col_2) ) {
    if ( re2::re2_detect(col_2[i], "^1") ){
      row_list <- append(row_list, list(col_2[i]))
      if (i < length(col_2) - 1) {
        j <- i + 1
        while( !re2::re2_detect(col_2[j], "^1") ) {
          row_list <- append(row_list, list(col_2[j]))
          if (j < length(col_2) - 1) { j <- j + 1 }
          else { break }
        }
      }
      row_lists <- append(row_lists, list(row_list))
      i <- j
      row_list <- list()
      if (i >= length(col_2)) break
    }
  }

  # Extract names
  names <- d1[re2::re2_detect(d1, "^[a-z]")]

  # Compile in data.table
  dt <- data.table::data.table(names, row_lists)

  return(dt)
}
