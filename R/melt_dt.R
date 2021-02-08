# Function takes irs from global env, subsets and melts 
# based on id_vars, then saves each iteration to disc
# Too large to hold in memory with all data in irs
# Data size is expanded by 10x going from wide to long

melt_dt <- function(irs) {
  
  # Aggregate rows by same agi_level, year
  id_vars <- c("year", "zipcode", "agi_level")
  measure_vars <- 
    setdiff(names(irs), id_vars)
  
  # Create id for zipcode and year
  #irs_zips <- 
  #data.table(zipcode = unique(irs$zipcode, by = "zipcode"))
  #irs_zips[, id := .I]
  
  lapply(measure_vars, function(num_var) {
    
    # Subset id.vars for melt
    subset <- 
      c(id_vars, num_var)
    
    # Load data and add id column
    d <- fread("irs_dt.csv", select = subset)
    #d <- irs_zips[d, on = "zipcode"]
    
    # Melt
    try(
      d <- d[, melt(
        .SD,
        id.vars = id_vars,
        measure.vars = num_var,
        variable.factor = TRUE,
        na.rm = TRUE
      ),
      .SDcols = subset])
    print(num_var)
    
    # Save
    fwrite(d, file = "irs_melt.csv", append = TRUE)
    rm(d, subset)
  })  
}