## code to prepare `rf_maat_map500_all` dataset goes here
rf_maat_map500_all <- readRDS("./data-raw/rf_maat_map500_all_gdgt.rds")
usethis::use_data(rf_maat_map500_all, overwrite = TRUE)
