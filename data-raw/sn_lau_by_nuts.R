## code to prepare `sn_lau_by_nuts` dataset goes here

library("readr")
library("dplyr")
sn_lau_by_nuts_df <- readr::read_csv(
  file = "https://github.com/EDJNet/lau_centres/releases/download/lau_2020_nuts_2021_pop_grid_2018/lau_2020_nuts_2021_pop_2018_p_2_adjusted_intersection.csv",
  col_types = cols(
    gisco_id = col_character(),
    longitude = col_double(),
    latitude = col_double(),
    country = col_character(),
    nuts_2 = col_character(),
    nuts_3 = col_character(),
    lau_id = col_character(),
    lau_name = col_character(),
    population = col_double(),
    area_km2 = col_double(),
    year = col_double(),
    fid = col_character(),
    concordance = col_character(),
    pop_weighted = col_logical()
  ))


library("latlon2map")
options(timeout = 60000) # big timeout, as big downloads needed 

ll_set_folder(path = fs::path(fs::path_home_r(),
                              "R",
                              "ll_data"))

sn_lau_by_nuts <- sn_lau_by_nuts_df %>% 
  dplyr::mutate(country_name = countrycode::countrycode(sourcevar = country, origin = "eurostat", destination = "country.name.en")) %>% 
  dplyr::select(gisco_id, country, country_name, nuts_2, nuts_3, lau_name) %>% 
  left_join(y = ll_get_nuts_eu(level = 2, year = 2021) %>% 
              sf::st_drop_geometry() %>% 
              transmute(nuts_2 = NUTS_ID, nuts_2_name = NUTS_NAME, nuts_2_name_latin = NAME_LATN),
            by = "nuts_2") %>% 
  left_join(y = ll_get_nuts_eu(level = 3, year = 2021) %>% 
              sf::st_drop_geometry() %>% 
              transmute(nuts_3 = NUTS_ID, nuts_3_name = NUTS_NAME, nuts_3_name_latin = NAME_LATN),
            by = "nuts_3") %>% 
  dplyr::group_by(gisco_id) %>% 
  dplyr::mutate(lau_label = paste0(lau_name, " (", nuts_3_name, ")")) %>% 
  ungroup()

sn_lau_by_nuts %>% 
  anti_join(y = sn_lau_by_nuts %>% 
  tidyr::drop_na(), by = "gisco_id")

sn_lau_by_nuts <- sn_lau_by_nuts %>% tidyr::drop_na()

usethis::use_data(sn_lau_by_nuts, overwrite = TRUE)
