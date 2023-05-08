## code to prepare `sn_bilingual_gisco_id` dataset goes here
library("dplyr", warn.conflicts = FALSE)
#lau_nuts_df <- readr::read_csv(file = "https://github.com/EDJNet/lau_centres/raw/main/lau_centres/lau_2020_nuts_2021_pop_2018_p_2_adjusted_intersection.csv")

south_tyrol_lau_df <- sn_lau_by_nuts %>% 
  dplyr::filter(nuts_3 == "ITH10") %>% 
  dplyr::arrange(dplyr::desc(population))

# actually checked only top 10 most popolous cities in South Tyrol
south_tyrol_lau_by_language_df <- south_tyrol_lau_df %>%
  dplyr::select(gisco_id, country, lau_name) %>% 
  dplyr::mutate(languages = dplyr::if_else(
    condition = gisco_id %in% c(
      "IT_021008", # Bozen
      "IT_021040" # Laives
      ),
    true =  "it_de",
    false = "de_it"))


## Belgium / actually checked only top, rest based on nuts2 region
provinces_Wallonia_df <- tidywikidatar::tw_get_property(id = "Q231", p = "P150")

nuts2_wallonia_v <- provinces_Wallonia_df %>% 
  dplyr::pull(value) %>% 
  tidywikidatar::tw_get_p1(p = "P605") 

wallonia_df <- sn_lau_by_nuts %>% 
  dplyr::filter(nuts_2 %in% c(nuts2_wallonia_v, 
                              c("BE100",
                                "BE_21004",
                                "BE_21015",
                                "BE_21001",
                                "BE_21012",
                                "BE_21016"))) 


belgium_by_language_df <- sn_lau_by_nuts %>% 
  dplyr::select(gisco_id, country, lau_name) %>% 
  dplyr::filter(country == "BE") %>% 
  dplyr::mutate(languages = dplyr::if_else(
    condition = gisco_id %in% wallonia_df$gisco_id,
    true =  "fr_nl",
    false = "nl_fr"))


sn_bilingual_gisco_id <- dplyr::bind_rows(
  south_tyrol_lau_by_language_df,
  belgium_by_language_df
)

usethis::use_data(sn_bilingual_gisco_id, overwrite = TRUE)
