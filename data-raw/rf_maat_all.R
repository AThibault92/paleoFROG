## code to prepare `rf_maat_all` dataset goes here
rf_maat_all <- readRDS("./data-raw/rf_maat_all_gdgt.rds")
usethis::use_data(rf_maat_all, overwrite = TRUE)
