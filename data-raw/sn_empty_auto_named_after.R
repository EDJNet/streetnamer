## code to prepare `sn_empty_auto_named_after` dataset goes here

sn_empty_auto_named_after <- tibble::tibble(
  street_name = as.character(NA), 
  named_after_id = as.character(NA),
  named_after_from_wikidata = as.logical(NA)) %>% 

  dplyr::slice(0)

usethis::use_data(sn_empty_auto_named_after, overwrite = TRUE)
