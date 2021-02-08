# LAB 44: MAKE AN R PACKAGE ----

# 1.0 LIBRARIES ----
library(data.table)
library(glue)
library(stringr)
library(janitor)
library(fst)

# 2.0 FUNCTIONS -----
lapply(list.files("./R", full.names = TRUE), source)

# 3.0 DATA ----
subdir <- "/Volumes/davidlucey/aDL/data/irs_zip_soi"
irs_raw <- load_soi(path = subdir)

# 4.0 WORKFLOW ----

irs_clean <- clean_soi(irs_raw)
irs_prepared <- prepare_app_data(full_clean)
write_fst(irs_app_data, "irs_app_full.fst")


