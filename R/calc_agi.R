
#Function to clean wrongly allocated 2006 agi_category levels
calc_agi <- function(x, y) {
  
  # Convert numeric
  #x <- as.numeric(x)
  #y <- as.numeric(y)
  
  # Classify based on x, y
  z <- fcase(
    x / y < 25,
    "1",
    x / y >= 25 & x / y < 50,
    "2",
    x / y >= 50 & x / y < 75,
    "3",
    x / y >= 75 & x / y < 100,
    "4",
    x / y >= 100,
    "5"
  )
  return(z)
}