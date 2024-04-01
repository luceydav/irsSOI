
#' Helper function used by load_soi() to name agi_stub
#'
##' @description
#' Helper to aggregate income levels from multiple year's of data
#'
#' @param agi_stub A State ID Code
#'
convert_agi_3 <- function(agi_stub) {

  # convert to y based on x
  y <- data.table::fcase(
    agi_stub == "1", "<$25k",
    agi_stub == "2", "$25-50k",
    agi_stub == "3", "$50-75k",
    agi_stub == "4", "$75-100k",
    agi_stub == "5", "$100k+",
    agi_stub == "6", "$200k+"
  )
  return(y)
}
