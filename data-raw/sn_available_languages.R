## code to prepare `sn_available_languages` dataset goes here

languages_df <- tw_get_all_with_p(p = "P424") # get all with a Wikimedia language code

sn_available_languages <- languages_df %>%
  tw_get_property("P31") %>% 
  filter(value == "Q34770") %>% 
  tw_get_property("P424") %>% 
  filter(nchar(value)==2) %>% 
  distinct(value, .keep_all = TRUE) %>% 
  rename(language_code = value) %>% 
  mutate(language_name = tw_get_label(id)) %>% 
  select(-property, -rank) %>% 
  arrange(language_name) 
  
usethis::use_data(sn_available_languages, overwrite = TRUE)

