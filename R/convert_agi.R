
#' Helper function used by load_soi() to normalize pre 2008 levels
#'
#' @description
#' Helper to aggregate income levels from multiple year's of data
#'
#' @param agi_level A State ID Code
#'
convert_agi <- function(agi_level) {

  # Convert x to y based on fcase
  y <- data.table::fcase(
    agi_level == "1", "<$25k",
    agi_level == "2", "<$25k",
    agi_level == "3", "$25-50k",
    agi_level == "4", "$50-75k",
    agi_level == "5", "$75-100k",
    agi_level == "6", "$100k+",
    agi_level == "7", "$100k+"
  )
  return(y)
}
