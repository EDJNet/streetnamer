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
  }

  if (country_name == "Austria") {
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

  if (country_name == "Germany") {
    street_name <- stringr::str_replace_all(
      string = street_name,
      pattern = "-",
      replacement = " "
    )
  } else if (country_name == "Poland") {
    street_name <- purrr::map_chr(
      .x = street_name,
      .f = function(x) {
        sn_clean_street_name_polish(x)
      }
    )
  } else if (country_name == "Romania") {
    street_name <- purrr::map_chr(
      .x = street_name,
      .f = function(x) {
        sn_clean_street_name_romanian(x)
      }
    )
  } else if (country_name == "Greece") {
    street_name <- purrr::map_chr(
      .x = street_name,
      .f = function(x) {
        sn_clean_street_name_greek(x)
      }
    )
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

    for (i in 1:max(1, (length(split_string) - 1))) {
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
    split_string <- stringr::str_split(
      string = street_name,
      pattern = "[[:space:]]",
      simplify = TRUE
    ) %>%
      as.character()

    split_string[length(split_string)] <- split_string[length(split_string)] %>%
      stringr::str_replace(
        pattern = "ății$",
        replacement = "atea"
      )


    stringr::str_c(split_string, collapse = " ")
  } else if (stringr::str_detect(string = street_name, pattern = "ului$")) {
    split_string <- stringr::str_split(
      string = street_name, pattern = "[[:space:]]",
      simplify = TRUE
    ) %>%
      as.character()

    split_string[length(split_string)] <- split_string[length(split_string)] %>%
      stringr::str_remove(pattern = "ului$")

    stringr::str_c(split_string, collapse = " ")
  } else if (stringr::str_detect(string = street_name, pattern = "ilor$|elor$")) {
    split_string <- stringr::str_split(
      string = street_name,
      pattern = "[[:space:]]",
      simplify = TRUE
    ) %>%
      as.character()

    split_string[length(split_string)] <- split_string[length(split_string)] %>%
      stringr::str_remove(pattern = "lor$")
    stringr::str_c(split_string, collapse = " ")
  } else if (stringr::str_detect(string = street_name, pattern = "iei$")) {
    split_string <- stringr::str_split(
      string = street_name,
      pattern = "[[:space:]]",
      simplify = TRUE
    ) %>%
      as.character()

    split_string[length(split_string)] <- split_string[length(split_string)] %>%
      stringr::str_replace(
        pattern = "iei$",
        replacement = "ia"
      )
    stringr::str_c(split_string, collapse = " ")
  } else if (stringr::str_detect(string = street_name, pattern = "ței$")) {
    split_string <- stringr::str_split(
      string = street_name,
      pattern = "[[:space:]]",
      simplify = TRUE
    ) %>%
      as.character()

    split_string[length(split_string)] <- split_string[length(split_string)] %>%
      stringr::str_replace(
        pattern = "ței$",
        replacement = "ța"
      )
    stringr::str_c(split_string, collapse = " ")
  } else if (stringr::str_detect(string = street_name, pattern = "dei$")) {
    split_string <- stringr::str_split(
      string = street_name,
      pattern = "[[:space:]]",
      simplify = TRUE
    ) %>%
      as.character()

    split_string[length(split_string)] <- split_string[length(split_string)] %>%
      stringr::str_replace(
        pattern = "dei$",
        replacement = "da"
      )
    stringr::str_c(split_string, collapse = " ")
  } else if (stringr::str_detect(string = street_name, pattern = "vei$")) {
    split_string <- stringr::str_split(
      string = street_name,
      pattern = "[[:space:]]",
      simplify = TRUE
    ) %>%
      as.character()

    split_string[length(split_string)] <- split_string[length(split_string)] %>%
      stringr::str_replace(
        pattern = "vei$",
        replacement = "va"
      )
    stringr::str_c(split_string, collapse = " ")
  } else if (stringr::str_detect(string = street_name, pattern = "cii$")) {
    split_string <- stringr::str_split(
      string = street_name,
      pattern = "[[:space:]]",
      simplify = TRUE
    ) %>%
      as.character()

    if (length(split_string) == 1) {
      split_string[length(split_string)] <- split_string[length(split_string)] %>%
        stringr::str_replace(
          pattern = "cii$",
          replacement = "ca"
        )
    }

    stringr::str_c(split_string, collapse = " ")
  } else {
    street_name
  }
}


#' Clean Greek street name
#'
#' @param street_name Name of street, after having removed street part.
#'
#' @return A character vector of lenght one. A name that can more easily be searched.
#' @export
#'
#' @examples
#'
#' sn_clean_street_name_greek("Αριστοτέλους")
sn_clean_street_name_greek <- function(street_name) {
  
  split_string <- stringr::str_split(string = street_name,
                                     pattern = "[[:space:]]",
                                     simplify = TRUE) %>%
    as.character()
  
  for (i in 1:max(1, (length(split_string) - 1))) {
    
    if (stringr::str_detect(string =  split_string[i], pattern =  "^Αγίου ")) {
      split_string[i] <- stringr::str_replace(string = split_string[i],
                                              pattern = "^Αγίου ",
                                              replacement = "Αγίους ")
    } else if (stringr::str_detect(string =  split_string[i], pattern =  "^Αγίων ")){
      split_string[i] <- stringr::str_replace(string = split_string[i],
                                              pattern = "^Αγίων ",
                                              replacement = "Αγιοι ")
    } else if (stringr::str_detect(string =  split_string[i], pattern =  "^Αγ. ")) {
      split_string[i] <- stringr::str_replace(string = split_string[i],
                                              pattern = "^Αγ. ",
                                              replacement = "Αγίους ")
      
    } else if (stringr::str_detect(string =  split_string[i], pattern = "λέους$")) {
      split_string[i] <- stringr::str_replace(string = split_string[i],
                                              pattern = "λέους$",
                                              replacement = "λής")
    } else if (stringr::str_detect(string =  split_string[i], pattern = "ους$")) {
      split_string[i] <- stringr::str_replace(string = split_string[i],
                                              pattern = "ους$",
                                              replacement = "ης")
    } else if (stringr::str_detect(string = street_name, pattern = "ανών$")) {
      split_string[i] <- stringr::str_replace(string = split_string[i],
                                              pattern = "ανών$",
                                              replacement = "ανά")
    } else if (stringr::str_detect(string = street_name, pattern = "α$")) {
      split_string[i] <- stringr::str_replace(string = split_string[i],
                                              pattern = "α$",
                                              replacement = "ας")
    } else if (stringr::str_detect(string =  split_string[i], pattern = "ου$")) {
      split_string[i] <- stringr::str_replace(string = split_string[i],
                                              pattern = "ου$",
                                              replacement = "ος")
    } else if (stringr::str_detect(string =  split_string[i], pattern = "η$")) {
      split_string[i] <- stringr::str_replace(string = split_string[i],
                                              pattern = "η$",
                                              replacement = "ης")
    } else if (stringr::str_detect(string =  split_string[i], pattern = "ης$")) {
      split_string[i] <- stringr::str_replace(string = split_string[i],
                                              pattern = "ης$",
                                              replacement = "η")
    } else if (stringr::str_detect(string =  split_string[i], pattern = "ων$")) {
      split_string[i] <- stringr::str_replace(string = split_string[i],
                                              pattern = "ων$",
                                              replacement = "οι")
    }
  }

  stringr::str_c(split_string, collapse = " ") %>% 
    stringr::str_squish()
  
}