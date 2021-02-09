
#' Helper function used by load_soi() to normalize post 2008 levels
#'
##' @description
#' Helper to aggregate income levels from multiple year's of data
#'
#' @param agi_level A State ID Code
#'
#'

convert_agi_2 <- function(agi_level) {

  # convert to y based on x
  y <- data.table::fcase(
    agi_level == "1", "<$25k",
    agi_level == "2", "$25-50k",
    agi_level == "3", "$50-75k",
    agi_level == "4", "$75-100k",
    agi_level == "5", "$100k+",
    agi_level == "6", "$100k+"
  )
  return(y)
}
