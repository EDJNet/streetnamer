## code to prepare `sn_occupation_categories_df` dataset goes here

# this is messy and derived from unstructured manual work, but until then...

library("tidywikidatar")

# get all qid for all countries

countries_v <- sn_lau_by_country %>% 
  dplyr::distinct(CNTR_CODE) %>% 
  dplyr::pull(CNTR_CODE)

all_qid_df <- purrr::map(
  .x = countries_v,
  .progress = TRUE,
  .f = \(x) {
    sn_get_street_named_after_id(
      country = x,
      remove_ignored = TRUE
    ) %>% 
      dplyr::mutate(time = as.numeric(time))
  }) %>% 
  purrr::list_rbind()

all_occupations_df <- all_qid_df %>% 
  dplyr::distinct(named_after_id) %>% 
  dplyr::pull(named_after_id) %>% 
  tw_get_property(p = "P106") %>% 
  dplyr::distinct(value) 

readr::write_csv(all_occupations_df, file = "all_occupations_df.csv")

all_occupations_with_label_df <- all_occupations_df %>% 
  tidyr::drop_na() %>% 
  dplyr::rename(qid = value) %>% 
  dplyr::mutate(label_en = tw_get_label(qid))

readr::write_csv(all_occupations_with_label_df, file = "all_occupations_with_label_df.csv")

all_occupations_with_label_df


matched_df <- readr::read_csv(file = "occupations_matched.csv") %>% 
  tidyr::drop_na() %>% 
  dplyr::mutate(occupation_category = stringr::str_replace_all(string = occupation_category,
                           c(`politics and government` = "politics",
                             `culture, science, arts` = "culture")))

sn_occupation_categories_df <- matched_df %>% 
  dplyr::rename(label_en = occupation_label, 
                category = occupation_category) %>% 
  dplyr::left_join(all_occupations_with_label_df, by = "label_en") %>% 
  dplyr::relocate(qid)



additional_matches_df <- sn_occupation_categories_df %>% 
  dplyr::filter(is.na(qid)) %>% 
  dplyr::pull(label_en) %>% 
  tw_search(include_search = TRUE) %>% 
  dplyr::group_by(search) %>% 
  dplyr::slice_head(n = 1) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(is.na(id)==FALSE) %>% 
  dplyr::rename(qid = id, 
                label_en = label) %>% 
  dplyr::left_join(y = sn_occupation_categories_df %>% 
                     dplyr::filter(is.na(qid) ) %>% 
                     dplyr::select(-qid),
                   by = "label_en") %>% 
  dplyr::filter(is.na(category)==FALSE) %>% 
  dplyr::select(qid, label_en)

sn_occupation_categories_df <- matched_df %>% 
  dplyr::rename(label_en = occupation_label, 
                category = occupation_category) %>% 
  dplyr::left_join(y = dplyr::bind_rows(all_occupations_with_label_df, 
                                        additional_matches_df) %>% 
                     dplyr::distinct(),
                   by = "label_en") %>% 
  dplyr::relocate(qid)


usethis::use_data(sn_occupation_categories_df, overwrite = TRUE)
