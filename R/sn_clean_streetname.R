#' Clean street name in order to keep only the
#'
#' @param street_name A character vector.
#' @param country Name of country. 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' sn_clean_street_name(street_name = c("Adele-Schreiber-Krieger-Straße", 
#'                                      "Achillesstraße",
#'                                      "Adlerbrücke",
#'                                      "Akazienweg"),
#'                       country = "Germany")
sn_clean_street_name <- function(street_name,
                                 country) {
  if (country == "Austria") {
    country_name <- "Germany"
  } else {
    country_name <- country
  }
  
  if (country_name %in% unique(sn_street_name_to_remove_df[["country"]])) {
    street_name <- stringr::str_remove_all(string = street_name,
                            pattern = sn_street_name_to_remove_df %>% 
                              dplyr::filter(country == country_name) %>% 
                              dplyr::pull(string) %>% 
                              stringr::str_c(collapse = "|") %>% 
                              stringr::regex(ignore_case = TRUE))
  } else {
    usethis::ui_info("No available method for this country.")
  }
  
  if (country == "Germany") {
    street_name <- stringr::str_replace_all(string = street_name,
                                            pattern = "-",
                                            replacement = " ")
  }
  
  
  street_name %>% 
    stringr::str_squish() %>% 
    stringr::str_replace_all(pattern = stringr::fixed("\\"), replacement = " ")
}