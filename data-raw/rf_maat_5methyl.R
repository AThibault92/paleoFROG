## code to prepare `rf_maat_5methyl` dataset goes here
rf_maat_5methyl <- readRDS("./data-raw/rf_maat_only_5methyl_gdgt.rds")
usethis::use_data(rf_maat_5methyl, overwrite = TRUE)
