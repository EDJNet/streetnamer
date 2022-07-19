## code to prepare `sn_empty_street_name_wikidata_id` dataset goes here

sn_set_data_folder(path = tempdir())
sn_write_street_name_wikidata_id(
  gisco_id = "IT_022205",
  street_name = "Belvedere San Francesco",
  wikidata_id = "Q676555",
  country = "IT",
  person = TRUE,
  category = NA,
  session = as.character(floor(runif(1) * 1e20)),
  # time = Sys.time(),
  checked = TRUE
)
sn_empty_street_name_wikidata_id <- sn_get_street_name_wikidata_id(
  gisco_id = "IT_022205",
  street_name = "Belvedere San Francesco",
  country = "IT"
) %>%
  dplyr::slice(0)

usethis::use_data(sn_empty_street_name_wikidata_id, overwrite = TRUE)
