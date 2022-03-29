## code to prepare `sn_language_defaults_by_country` dataset goes here


# https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all

sn_language_defaults_by_country <- tibble::tribble(
  ~country, ~country_code, ~language, ~language_code, ~qid,
  "Czechia", "CZ", "Czech", "cs", "Q9056", 
  "France", "FR", "French", "fr", "Q150",
  "Germany", "DE", "German", "de", "Q188",
  "Austria", "AT", "German", "de", "Q188",
  "Greece", "EL", "Greek", "el", "Q36510",
  "Italy", "IT", "Italian", "it", "Q652",
  "Portugal", "PT", "Portuguese", "pt", "Q5146",
  "Spain", "ES", "Spanish", "es", "Q1321",
  "Ireland", "IE", "English", "en", "Q1860",
  "Sweden", "SV", "Swedish", "sv", "Q9027",
  "United Kingdom", "GB", "English", "en", "Q1860"
)


usethis::use_data(sn_language_defaults_by_country, overwrite = TRUE)
