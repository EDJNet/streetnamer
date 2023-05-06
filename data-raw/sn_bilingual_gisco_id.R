## code to prepare `sn_bilingual_gisco_id` dataset goes here
library("dplyr", warn.conflicts = FALSE)
lau_nuts_df <- readr::read_csv(file = "https://github.com/EDJNet/lau_centres/raw/main/lau_centres/lau_2020_nuts_2021_pop_2018_p_2_adjusted_intersection.csv")

south_tyrol_lau_df <- lau_nuts_df %>% 
  dplyr::filter(nuts_3 == "ITH10") %>% 
  dplyr::arrange(dplyr::desc(population))

# actually checked only top 10 most popolous cities in South Tyrol
sn_bilingual_gisco_id <- south_tyrol_lau_df %>%
  dplyr::select(gisco_id, country, lau_name) %>% 
  dplyr::mutate(languages = dplyr::if_else(
    condition = gisco_id %in% c(
      "IT_021008", # Bozen
      "IT_021040" # Laives
      ),
    true =  "it_de",
    false = "de_it"))


usethis::use_data(sn_bilingual_gisco_id, overwrite = TRUE)
