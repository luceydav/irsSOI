

#Function to normalize post 2008 levels
convert_agi_2 <- function(x) {

  # convert to y based on x
  y <- data.table::fcase(
    x == "1", "<$25k",
    x == "2", "$25-50k",
    x == "3", "$50-75k",
    x == "4", "$75-100k",
    x == "5", "$100k+",
    x == "6", "$100k+"
  )
  return(y)
}
