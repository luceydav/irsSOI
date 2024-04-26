## code to prepare `DATASET` dataset goes here

data_dict <- lapply(2005:2020, get_dict)
names(data_dict) <- as.character(2005:2020)
data_dict <- data_dict[sapply(data_dict, is.null) == FALSE]
data_dict <- rbindlist(data_dict, use.names = TRUE, fill = TRUE, idcol = "year")
data_dict[, `:=`(
  name = re2::re2_replace_all(names, ":", ""),
  desc = sapply(row_lists, `[[`, 1),
  names = NULL,
  row_lists = NULL
)]
data_dict[, desc := stringr::str_replace_all(desc, "^1.  ", "")]

usethis::use_data(data_dict, overwrite = TRUE)
