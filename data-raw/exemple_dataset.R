## code to prepare `exemple_dataset` dataset goes here
exemple_dataset <- readRDS("./data-raw/all_data.rds")
usethis::use_data(exemple_dataset, overwrite = TRUE)
