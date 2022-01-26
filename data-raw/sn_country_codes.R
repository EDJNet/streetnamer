## code to prepare `sn_country_codes` dataset goes here

sn_country_codes <- readr::read_csv(file = "https://datahub.io/core/country-list/r/data.csv")

usethis::use_data(sn_country_codes, overwrite = TRUE)
