## code to prepare `sn_countries_with_streets_as_qid` dataset goes here

sn_countries_with_streets_as_qid <- tibble::tribble(
  ~country_code,
  "DE",
  "CZ",
  "NL",
  "BE",
  "FR"
)

usethis::use_data(sn_countries_with_streets_as_qid, overwrite = TRUE)
