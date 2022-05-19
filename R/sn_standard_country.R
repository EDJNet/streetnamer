#' Turn a country name its a standard format, either country name, or two letter country code
#'
#' @param country A character string. Either full name of a country, or a two-letter country code.
#' @param type Defines output format. If "name", outputs full name. If "code", outputs two letter country code. 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' sn_standard_country("DE", type = "name")
#' 
#' sn_standard_country("France", type = "code")
#' 
#' # if requested input the same as output, it is returned consitently
#' sn_standard_country("FR", type = "code")
sn_standard_country <- function(country,
                                type = "name") {
  
  if (nchar(country) == 2) {
    country_code <- stringr::str_to_upper(country)
    country_name <- streetnamer::sn_country_codes %>%
      dplyr::filter(.data$Code == country_code) %>%
      dplyr::pull(.data$Name)
  } else {
    country_lower_v <- stringr::str_to_lower(country)
    country_slice <- streetnamer::sn_country_codes %>%
      dplyr::mutate(country_lower = stringr::str_to_lower(Name)) %>%
      dplyr::filter(.data$country_lower == country_lower_v)
    
    country_name <- country_slice %>%
      dplyr::pull(.data$Name)
    country_code <- country_slice %>%
      dplyr::pull(.data$Code)
    
    if (country_lower_v=="greece") {
      country_code <- "EL"
    } else if (country_lower_v=="czechia") {
      country_code <- "CZ"
      country_name <- "Czech Republic"
    }
  }
  
  if (type == "name") {
    country_name
  } else if (type == "code") {
    country_code
  }
}