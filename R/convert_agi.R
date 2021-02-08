
#Function to normalize pre 2008 levels

convert_agi <- function(x) {
  
  # Convert x to y based on fcase
  y <- fcase(
    x == "1", "<$25k",
    x == "2", "<$25k",
    x == "3", "$25-50k",
    x == "4", "$50-75k",
    x == "5", "$75-100k",
    x == "6", "$100k+",
    x == "7", "$100k+"
  )
  return(y)
}