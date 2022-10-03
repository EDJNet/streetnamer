## code to prepare `sn_lau_by_country` dataset goes here

sn_country_codes <- readr::read_csv(file = "https://datahub.io/core/country-list/r/data.csv")

sn_lau_by_country <- ll_get_lau_eu() %>%
  sf::st_drop_geometry() %>%
  dplyr::select(GISCO_ID, CNTR_CODE, LAU_NAME) %>%
  dplyr::left_join(
    y = sn_country_codes %>%
      dplyr::rename(CNTR_CODE = Code, CNTR_NAME = Name),
    by = "CNTR_CODE"
  ) %>%
  dplyr::select(GISCO_ID, LAU_NAME, CNTR_CODE, CNTR_NAME) %>%
  dplyr::arrange(GISCO_ID)


sn_lau_by_country$CNTR_NAME[sn_lau_by_country$CNTR_CODE == "EL"] <- "Greece"
sn_lau_by_country$CNTR_NAME[sn_lau_by_country$CNTR_CODE == "UK"] <- "United Kingdom"
sn_lau_by_country$CNTR_NAME[sn_lau_by_country$CNTR_CODE == "MK"] <- "North Macedonia"
sn_lau_by_country$CNTR_NAME[sn_lau_by_country$CNTR_CODE == "CZ"] <- "Czechia"
## check
sn_lau_by_country %>%
  dplyr::filter(is.na(CNTR_NAME)) %>%
  dplyr::distinct(CNTR_CODE)

sn_lau_by_country <- dplyr::bind_rows(sn_lau_by_country,
                                      sn_lau_by_nuts %>% 
                                        dplyr::transmute(GISCO_ID = gisco_id,
                                                         LAU_NAME = lau_name,
                                                         CNTR_CODE = country,
                                                         CNTR_NAME = country_name)) %>% 
  dplyr::distinct()




usethis::use_data(sn_lau_by_country, overwrite = TRUE)
