## code to prepare `sn_country_codes` dataset goes here

sn_country_codes <- readr::read_csv(file = "https://datahub.io/core/country-list/r/data.csv")

sn_country_codes$Name[sn_country_codes$Code=="MD"] <- "Moldova"

sn_country_codes <- sn_country_codes %>% 
  dplyr::filter(is.na(Code)==FALSE)

usethis::use_data(sn_country_codes, overwrite = TRUE)
