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
#' sn_clean_street_name(
#'   street_name = c(
#'     "Adele-Schreiber-Krieger-Straße",
#'     "Achillesstraße",
#'     "Adlerbrücke",
#'     "Akazienweg"
#'   ),
#'   country = "Germany"
#' )
sn_clean_street_name <- function(street_name,
                                 country) {
  if (country == "Austria") {
    country_name <- "Germany"
  } else {
    country_name <- country
  }

  if (country_name %in% unique(sn_street_name_to_remove_df[["country"]])) {
    street_name <- stringr::str_remove_all(
      string = street_name,
      pattern = sn_street_name_to_remove_df %>%
        dplyr::filter(country == country_name) %>%
        dplyr::pull(string) %>%
        stringr::str_c(collapse = "|") %>%
        stringr::regex(ignore_case = TRUE)
    )
  } else {
    usethis::ui_info("No available method for this country.")
  }

  if (country == "Germany") {
    street_name <- stringr::str_replace_all(
      string = street_name,
      pattern = "-",
      replacement = " "
    )
  } else if (country == "Poland") {
    street_name <- purrr::map_chr(
      .x = street_name,
      .f = function(x) {
        sn_clean_street_name_polish(x)
      })
  } else if (country == "Romania") {
    street_name <- purrr::map_chr(
      .x = street_name,
      .f = function(x) {
        sn_clean_street_name_romanian(x)
      })
  }
  
  street_name %>%
    stringr::str_squish() %>%
    stringr::str_replace_all(pattern = stringr::fixed("\\"), replacement = " ")
}



#' Clean Polish street name
#'
#' @param street_name Name of street, after having removed street part (e.g. "Aleja")
#'
#' @return A character vector of lenght one. A name that can more easily be searched. 
#' @export
#'
#' @examples
#' 
#' sn_clean_street_name_polish("Pawła Edmunda Strzeleckiego")
sn_clean_street_name_polish <- function(street_name) {
  
  if (stringr::str_detect(string = street_name, pattern = "ego$")) {
      split_string <- stringr::str_split(string = street_name, pattern = "[[:space:]]", simplify = TRUE) %>% as.character()
      
      split_string[length(split_string)] <- split_string[length(split_string)] %>% stringr::str_remove(pattern = "ego")
      
      for (i in 1:max(1,(length(split_string)-1))) {
        if (stringr::str_detect(string = split_string[i], pattern = "rzego$")) {
          split_string[i] <- stringr::str_replace(string = split_string[i], pattern = "rzego$", replacement = "rzy")
        } else if (stringr::str_detect(string = split_string[i], pattern = "ła$")) {
          split_string[i] <- stringr::str_replace(string = split_string[i], pattern = "ła$", replacement = "eł")
        } else if (stringr::str_detect(string = split_string[i], pattern = "ego$")) {
          split_string[i] <- stringr::str_replace(string = split_string[i], pattern = "ego$", replacement = "")
        } else if (stringr::str_detect(string = split_string[i], pattern = "szka$")) {
          split_string[i] <- stringr::str_replace(string = split_string[i], pattern = "szka$", replacement = "szek")
        } else {
          split_string[i] <- stringr::str_remove(string = split_string[i], pattern = "a$")
        }
      }
      stringr::str_c(split_string, collapse = " ")
  } else {
    street_name
    }
 
}


#' Clean Romanian street names
#'
#' @param street_name Name of street, after having removed street part (e.g. "Aleea")
#'
#' @return A character vector of length one. A name that can more easily be searched. 
#' @export
#'
#' @examples
#' 
#' sn_clean_street_name_romanian("Aleea Egalității")
sn_clean_street_name_romanian <- function(street_name) {
  
  if (stringr::str_detect(string = street_name, pattern = "ății$")) {
    split_string <- stringr::str_split(string = street_name,
                                       pattern = "[[:space:]]",
                                       simplify = TRUE) %>%
      as.character()
    
    for (i in 1:max(1,(length(split_string)-1))) {
      if (stringr::str_detect(string = split_string[i], pattern = "ății$")) {
        split_string[i] <- stringr::str_replace(string = split_string[i],
                                                pattern = "ății$",
                                                replacement = "atea")
      }
    }
    stringr::str_c(split_string, collapse = " ")
  } else if (stringr::str_detect(string = street_name, pattern = "ului$")) {
    split_string <- stringr::str_split(string = street_name, pattern = "[[:space:]]",
                                       simplify = TRUE) %>%
      as.character()
    
    split_string[length(split_string)] <- split_string[length(split_string)] %>%
      stringr::str_remove(pattern = "ului$")
    
  } else if (stringr::str_detect(string = street_name, pattern = "ilor$|elor$")) {
    split_string <- stringr::str_split(string = street_name,
                                       pattern = "[[:space:]]",
                                       simplify = TRUE) %>%
      as.character()
    
    split_string[length(split_string)] <- split_string[length(split_string)] %>%
      stringr::str_remove(pattern = "lor$")
    
  } else {
    street_name
  }
  
}