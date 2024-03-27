
#' Helper function to clean wrongly allocated 2006 agi_category levels
#'
#' @description
#' 2006 Data had incorrectly specified agi_levels above 7 so best efforts to fix
#'
#' @param x a00100 (AGI)
#' @param y n1 (number of returns)
#'
calc_agi <- function(x, y) {

  # Classify based on x, y
  z <- data.table::fcase(
    x / y < 25, "1",
    x / y >= 25 & x / y < 50, "2",
    x / y >= 50 & x / y < 75, "3",
    x / y >= 75 & x / y < 100, "4",
    x / y >= 100, "5"
  )
  return(z)
}
