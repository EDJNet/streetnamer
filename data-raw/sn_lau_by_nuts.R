## code to prepare `sn_lau_by_nuts` dataset goes here
library("latlon2map")
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
  )
)


library("latlon2map")
options(timeout = 60000) # big timeout, as big downloads needed

ll_set_folder(path = fs::path(
  fs::path_home_r(),
  "R",
  "ll_data"
))

sn_lau_by_nuts_pre_df <- sn_lau_by_nuts_df %>%
  dplyr::mutate(country_name = countrycode::countrycode(sourcevar = country, origin = "eurostat", destination = "country.name.en")) %>%
  dplyr::select(gisco_id, country, country_name, nuts_2, nuts_3, lau_name, population, longitude, latitude) %>%
  left_join(
    y = ll_get_nuts_eu(level = 2, year = 2021) %>%
      sf::st_drop_geometry() %>%
      transmute(nuts_2 = NUTS_ID, nuts_2_name = NUTS_NAME, nuts_2_name_latin = NAME_LATN),
    by = "nuts_2"
  ) %>%
  left_join(
    y = ll_get_nuts_eu(level = 3, year = 2021) %>%
      sf::st_drop_geometry() %>%
      transmute(nuts_3 = NUTS_ID, nuts_3_name = NUTS_NAME, nuts_3_name_latin = NAME_LATN),
    by = "nuts_3"
  ) %>%
  dplyr::group_by(gisco_id) %>%
  dplyr::mutate(lau_label = dplyr::if_else(condition = lau_name == nuts_3_name,
    true = lau_name,
    false = paste0(lau_name, " (", nuts_3_name, ")")
  )) %>%
  ungroup()

sn_lau_by_nuts_pre_df %>%
  anti_join(y = sn_lau_by_nuts_pre_df %>%
    tidyr::drop_na(), by = "gisco_id")

# checks
sn_lau_by_nuts_pre_df %>%
  group_by(country) %>%
  arrange(country, desc(population)) %>%
  filter(country == "CH")

#### Add custom streets ####

##### Add Brussels #####

brussels_row_df <- ll_get_nuts_eu(nuts_id = "BE100") %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(
    gisco_id = NUTS_ID,
    country = "BE",
    country_name = "Belgium",
    nuts_2 = "BE10",
    nuts_3 = "BE100",
    lau_name = NAME_LATN,
    priority = TRUE,
    lau_label = NAME_LATN
  )

##### Add Dublin #####

dublin_row_df <- ll_get_nuts_eu(nuts_id = "IE061") %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(
    gisco_id = NUTS_ID,
    country = "IE",
    country_name = "Ireland",
    nuts_2 = "IE06",
    nuts_3 = "IE061",
    lau_name = NAME_LATN,
    priority = TRUE,
    lau_label = NAME_LATN
  )


##### Add Portugal #####

pt_concelho_df <- ll_lau_pt_id %>%
  dplyr::transmute(
    gisco_id = id,
    country = "PT",
    country_name = "Portugal",
    lau_name = Concelho,
    population,
    priority = TRUE,
    lau_label = Concelho
  )

##### Add Ukraine #####

ua1_df <- ll_get_gadm(geo = "UKR", level = 1) %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(ENGTYPE_1 == "Independent City") %>%
  dplyr::transmute(
    country = "UA",
    country_name = "Ukraine",
    gisco_id = stringr::str_c("UA_", GID_1),
    lau_name = "Kyiv",
    lau_label = "Kyiv"
  )

ua2_df <- ll_get_gadm(geo = "UKR", level = 2) %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(ENGTYPE_2 == "City" | ENGTYPE_2 == "City of Regional Significance") %>%
  dplyr::transmute(
    country = "UA",
    country_name = "Ukraine",
    gisco_id = stringr::str_c("UA_", GID_2),
    lau_name = VARNAME_2,
    lau_label = VARNAME_2
  ) %>%
  dplyr::filter(
    is.na(lau_name) == FALSE,
    lau_name != "NA"
  )







##### Add Moldova #####

md_rows <- ll_get_adm_ocha(geo = "MD", level = 1) %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(
    gisco_id = ADM1_PCODE,
    country = "MD",
    country_name = "Moldova",
    lau_name = ADM1_EN,
    lau_label = ADM1_EN,
    priority = FALSE
  )

md_rows$priority[md_rows$lau_label == "Chisinau"] <- TRUE

sn_lau_by_nuts <- dplyr::bind_rows(
  sn_lau_by_nuts_pre_df,
  brussels_row_df,
  dublin_row_df,
  pt_concelho_df,
  ua1_df,
  md_rows
) %>%
  group_by(country) %>%
  arrange(country, desc(priority), desc(population)) %>%
  ungroup() %>%
  dplyr::select(-priority)

usethis::use_data(sn_lau_by_nuts, overwrite = TRUE)
