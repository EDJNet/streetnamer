## code to prepare `sn_language_defaults_by_country` dataset goes here


# https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all

sn_language_defaults_by_country <- tibble::tribble(
  ~country, ~country_code, ~language, ~language_code, ~qid,
  "Albania", "AL", "Albanian", "sq", "Q8748",
  "Kosovo", "AL", "Albanian", "sq", "Q8748",
  "Austria", "AT", "German", "de", "Q188",
  "Belgium", "BE", "Dutch", "nl", "Q7411",
  "Belgium", "BE", "French", "fr", "Q150",
  "Bulgaria", "BG", "Bulgarian", "bg", "Q7918",
  "Czechia", "CZ", "Czech", "cs", "Q9056",
  "Croatia", "HR", "Croatian", "hr", "Q6654",
  "Cyprus", "CY", "Greek", "el", "Q36510",
  "Denmark", "DK", "Danish", "da", "Q9035",
  "Estonia", "EE", "Estonian", "et", "Q9072",
  "Finland", "FI", "Finnish", "fi", "Q1412",
  "France", "FR", "French", "fr", "Q150",
  "Germany", "DE", "German", "de", "Q188",
  "Greece", "EL", "Greek", "el", "Q36510",
  "Hungary", "HU", "Hungarian", "hu", "Q9067",
  "Ireland", "IE", "English", "en", "Q1860",
  "Italy", "IT", "Italian", "it", "Q652",
  "Latvia", "LV", "Latvian", "lv", "Q9078",
  "Lithuania", "LT", "Lithuanian", "lt", "Q9083",
  "Malta", "MT", "Maltese", "mt", "Q9166",
  "Moldova", "MD", "Moldovan", "ro", "Q36392",
  "Netherlands", "NL", "Dutch", "nl", "Q7411",
  "Norway", "NO", "Norwegian", "no", "Q9043",
  "Poland", "PL", "Polish", "pl", "Q809",
  "Portugal", "PT", "Portuguese", "pt", "Q5146",
  "Romania", "RO", "Romanian", "ro", "Q7913",
  "Serbia", "RS", "Serbian", "sr", "Q9299",
  "Slovenia", "SI", "Slovenian", "sl", "Q9063",
  "Slovakia", "SK", "Slovak", "sk", "Q9058",
  "Spain", "ES", "Spanish", "es", "Q1321",
  "Sweden", "SV", "Swedish", "sv", "Q9027",
  "Ukraine", "UA", "Ukrainian", "uk", "Q8798",
  "United Kingdom", "UK", "English", "en", "Q1860"
)


usethis::use_data(sn_language_defaults_by_country, overwrite = TRUE)
