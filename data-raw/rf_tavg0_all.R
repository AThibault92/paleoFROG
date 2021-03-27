## code to prepare `rf_tavg0_all` dataset goes here
rf_tavg0_all <- readRDS("./data-raw/rf_tavg0_all_gdgt.rds")
usethis::use_data(rf_tavg0_all, overwrite = TRUE)
