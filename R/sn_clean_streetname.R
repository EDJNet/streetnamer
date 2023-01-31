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
    
    if (country_lower_v=="czechia") {
      country_slice <- tibble::tibble(Name = "Czechia",
                                      Code = "CZ",
                                      country_lower = "czechia")
    } else {
      country_slice <- streetnamer::sn_country_codes %>%
        dplyr::mutate(country_lower = stringr::str_to_lower(Name)) %>%
        dplyr::filter(.data$country_lower == country_lower_v)
    }
    
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
  } else if (country_name == "Croatia") {
    street_name <- purrr::map_chr(
      .x = street_name,
      .f = function(x) {
        sn_clean_street_name_croatian(x)
      }
    )
  } else if (country_name == "Romania" | country_name == "Moldova") {
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
  } else if (country_name == "Ukraine") {
    street_name <- purrr::map_chr(
      .x = street_name,
      .f = function(x) {
        sn_clean_street_name_ukrainian(x)
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
#' @return A character vector of length one. A name that can more easily be searched.
#' @export
#'
#' @examples
#'
#' sn_clean_street_name_polish("Pawła Edmunda Strzeleckiego")
sn_clean_street_name_polish <- function(street_name) {
  if (is.na(street_name)) {
    return("")
  }

  if (stringr::str_detect(string = street_name, pattern = "ego$")) {
    split_string <- stringr::str_split(string = street_name, pattern = "[[:space:]]", simplify = TRUE) %>% as.character()

    split_string[length(split_string)] <- split_string[length(split_string)] %>% stringr::str_remove(pattern = "ego")

    for (i in 1:max(1, (length(split_string)))) {
      if (stringr::str_detect(string = split_string[i], pattern = "rzego$")) {
        split_string[i] <- stringr::str_replace(string = split_string[i], pattern = "rzego$", replacement = "rzy")
      } else if (stringr::str_detect(string = split_string[i], pattern = "Świętego$")) {
        split_string[i] <- stringr::str_replace(string = split_string[i], pattern = "tego$", replacement = "ty")
      } else if (stringr::str_detect(string = split_string[i], pattern = "Generała$")) {
        split_string[i] <- stringr::str_replace(string = split_string[i], pattern = "ła$", replacement = "ł")
      } else if (stringr::str_detect(string = split_string[i], pattern = "ława$")) {
        split_string[i] <- stringr::str_replace(string = split_string[i], pattern = "ława$", replacement = "ław")
      } else if (stringr::str_detect(string = split_string[i], pattern = "Ignacego")) {
        split_string[i] <- stringr::str_replace(string = split_string[i], pattern = "Ignacego$", replacement = "Ignacy")
      } else if (stringr::str_detect(string = split_string[i], pattern = "ego$")) {
        split_string[i] <- stringr::str_replace(string = split_string[i], pattern = "ego$", replacement = "")
      } else if (stringr::str_detect(string = split_string[i], pattern = "szka$")) {
        split_string[i] <- stringr::str_replace(string = split_string[i], pattern = "szka$", replacement = "szek")
      } else if (stringr::str_detect(string = split_string[i], pattern = "ndra$")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "ndra$",
          replacement = "nder"
        )
      } else {
        split_string[i] <- stringr::str_remove(string = split_string[i], pattern = "a$")
      }
    }
    return(stringr::str_c(split_string, collapse = " ") %>% stringr::str_squish())
  } else {
    split_string <- stringr::str_split(string = street_name, pattern = "[[:space:]]", simplify = TRUE) %>% as.character()
    name_replace_m_v <- c(
      Adama = "Adam",
      Adolfa = "Adolf",
      Aleksandra = "Aleksander",
      Andrzeja = "Andrzej",
      Antoniego = "Antoni",
      Augusta = "August",
      Bolesława = "Bolesław",
      Bronisława = "Bronisław",
      Czesława = "Czesław",
      Edwarda = "Edward",
      Eugeniusza = "Eugeniusz",
      Franciszka = "Franciszek",
      Gabriela = "Gabriel",
      Grzegorza = "Grzegorz",
      Gustawa = "Gustaw",
      Henryka = "Henryk",
      Ignacego = "Ignacy",
      Jana = "Jan",
      Janusza = "Janusz",
      Jarosława = "Jarosław",
      Jerzego = "Jerzy",
      Józefa = "Józef",
      Juliana = "Julian",
      Juliusza = "Juliusz",
      Leona = "Leon",
      Leopolda = "Leopold",
      Leszka = "Leszek",
      Ludwika = "Ludwik",
      Karola = "Karol",
      Kazimierza = "Kazimierz",
      Krzysztofa = "Krzysztof",
      Macieja = "Maciej",
      Maksymiliana = "Maksymilian",
      Mariana = "Marian",
      Michała = "Michał",
      Mieczysława = "Mieczysław",
      Mikołaja = "Mikołaj",
      Pabla = "Pablo",
      Piotra = "Piotr",
      Romana = "Roman",
      Romualda = "Romuald",
      Szczepana = "Szczepan",
      Szymona = "Szymon",
      Seweryna = "Seweryn",
      Stanisława = "Stanisław",
      Stefana = "Stefan",
      Tadeusza = "Tadeusz",
      Teodora = "Teodor",
      Tomasza = "Tomasz",
      Wacława = "Wacław",
      Wawrzyńca = "Wawrzyniec",
      Wincentego = "Wincent",
      Witolda = "Witold",
      Władysława = "Władysław",
      Włodzimierza = "Włodzimierz",
      Wojciecha = "Wojciech",
      Zbigniewa = "Zbigniew",
      Zdzisława = "Zdzisław",
      Zygmunta = "Zygmunt",
      Świętego = "Święty"
    )

    name_replace_f_v <- c(
      Agnieszki = "Agnieszka",
      Aleksandry = "Aleksandra",
      Aliny = "Alina",
      Anny = "Anna",
      Barbary = "Barbara",
      Bitwa = "Bitwy",
      Elżbiety = "Elżbieta",
      Emilii = "Emilia",
      Ewy = "Ewa",
      Haliny = "Halina",
      Hanny = "Hanna",
      Heleny = "Helena",
      Janiny = "Janina",
      Katarzyny = "Katarzyna",
      Królowej = "Królowa",
      Księżnej = "Księżna",
      Ludwiki = "Ludwika",
      Marii = "Maria",
      Natalii = "Natalia",
      Poli = "Pola",
      Świętej = "Święta",
      Wandy = "Wanda",
      Wisławy = "Wisława",
      Zofii = "Zofia"
    )

    if (split_string[1] %in% names(name_replace_m_v)) {
      if (stringr::str_detect(string = split_string[1], pattern = "Świętego$")) {
        split_string[1] <- stringr::str_replace(string = split_string[1], pattern = "tego$", replacement = "ty")
      } else {
        split_string[1] <- stringr::str_replace_all(string = split_string[1], name_replace_m_v)
      }

      if (length(split_string) > 1) {
        for (j in 2:length(split_string)) {
          if (stringr::str_detect(string = split_string[j], pattern = "ndra$")) {
            split_string[j] <- stringr::str_replace(
              string = split_string[j],
              pattern = "ndra$",
              replacement = "nder"
            )
          } else if (stringr::str_detect(string = split_string[j], pattern = "a$")) {
            split_string[j] <- stringr::str_replace(
              string = split_string[j],
              pattern = "a$",
              replacement = ""
            )
          }
        }
      }

      return(stringr::str_c(split_string, collapse = " ") %>%
        stringr::str_squish())
    } else if (split_string[1] %in% names(name_replace_f_v)) {
      split_string[1] <- stringr::str_replace_all(string = split_string[1], name_replace_f_v)


      if (length(split_string) > 1) {
        for (j in 2:length(split_string)) {
          if (stringr::str_detect(string = split_string[j], pattern = "iej$")) {
            split_string[j] <- stringr::str_replace(
              string = split_string[j],
              pattern = "iej$",
              replacement = "a"
            )
          } else if (stringr::str_detect(string = split_string[j], pattern = "ówny$")) {
            split_string[j] <- stringr::str_replace(
              string = split_string[j],
              pattern = "ówny$",
              replacement = "ówna"
            )
          } else if (stringr::str_detect(string = split_string[j], pattern = "y$")) {
            split_string[j] <- stringr::str_replace(
              string = split_string[j],
              pattern = "y$",
              replacement = "a"
            )
          } else if (stringr::str_detect(string = split_string[j], pattern = "igi$")) {
            split_string[j] <- stringr::str_replace(
              string = split_string[j],
              pattern = "igi$",
              replacement = "iga"
            )
          } else if (stringr::str_detect(string = split_string[j], pattern = "ngi$")) {
            split_string[j] <- stringr::str_replace(
              string = split_string[j],
              pattern = "ngi$",
              replacement = "nga"
            )
          }
        }
      }


      return(stringr::str_c(split_string, collapse = " ") %>%
        stringr::str_squish())
    }
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
#' @return A character vector of length one. A name that can more easily be searched.
#' @export
#'
#' @examples
#'
#' sn_clean_street_name_greek("Αριστοτέλους")
sn_clean_street_name_greek <- function(street_name) {
  split_string <- stringr::str_split(
    string = street_name,
    pattern = "[[:space:]]",
    simplify = TRUE
  ) %>%
    as.character()

  if (stringr::str_detect(string = street_name, pattern = "^Αγίου ")) {
    split_string[1] <- stringr::str_replace(
      string = split_string[1],
      pattern = "^Αγίου",
      replacement = "Αγίος"
    )

    split_string[2] <- stringr::str_replace(
      string = split_string[2],
      pattern = "ου$",
      replacement = "ος"
    )
  } else if (stringr::str_detect(string = street_name, pattern = "^Αγίας ")) {
    split_string[1] <- stringr::str_replace(
      string = split_string[1],
      pattern = "^Αγίας",
      replacement = "Αγία"
    )

    split_string[2] <- stringr::str_replace(
      string = split_string[2],
      pattern = "ς$",
      replacement = ""
    )
  } else {
    for (i in 1:max(1, (length(split_string)))) {
      if (stringr::str_detect(string = split_string[i], pattern = "ονος$")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "ονος$",
          replacement = "ων"
        )
      } else if (stringr::str_detect(string = split_string[i], pattern = "^Αγίου ")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "^Αγίου ",
          replacement = "Αγίους "
        )
      } else if (stringr::str_detect(string = split_string[i], pattern = "^Αγίων ")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "^Αγίων ",
          replacement = "Αγιοι "
        )
      } else if (stringr::str_detect(string = split_string[i], pattern = "^Αγ. ")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "^Αγ. ",
          replacement = "Αγίους "
        )
      } else if (stringr::str_detect(string = split_string[i], pattern = "λέους$")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "λέους$",
          replacement = "λής"
        )
      } else if (stringr::str_detect(string = split_string[i], pattern = "ους$")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "ους$",
          replacement = "ης"
        )
      } else if (stringr::str_detect(string = street_name, pattern = "ανών$")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "ανών$",
          replacement = "ανά"
        )
      } else if (stringr::str_detect(string = street_name, pattern = "α$")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "α$",
          replacement = "ας"
        )
      } else if (stringr::str_detect(string = street_name, pattern = "ας$")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "ας$",
          replacement = "α"
        )
      } else if (stringr::str_detect(string = split_string[i], pattern = "ου$")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "ου$",
          replacement = "ος"
        )
      } else if (stringr::str_detect(string = split_string[i], pattern = "ού$")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "ού$",
          replacement = "ός"
        )
      } else if (stringr::str_detect(string = split_string[i], pattern = "η$")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "η$",
          replacement = "ης"
        )
      } else if (stringr::str_detect(string = split_string[i], pattern = "ης$")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "ης$",
          replacement = "η"
        )
      } else if (stringr::str_detect(string = split_string[i], pattern = "ής$")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "ής$",
          replacement = "ή"
        )
      } else if (stringr::str_detect(string = split_string[i], pattern = "ων$")) {
        split_string[i] <- stringr::str_replace(
          string = split_string[i],
          pattern = "ων$",
          replacement = "οι"
        )
      }
    }
  }


  stringr::str_c(split_string, collapse = " ") %>%
    stringr::str_squish()
}




#' Clean Croatian street name
#'
#' @param street_name Name of street, after having removed street part.
#'
#' @return A character vector of length one. A name that can more easily be searched.
#' @export
#'
#' @examples
#'
#' sn_clean_street_name_croatian("Joze Kljakovića")
sn_clean_street_name_croatian <- function(street_name) {
  split_string <- stringr::str_split(
    string = street_name,
    pattern = "[[:space:]]",
    simplify = TRUE
  ) %>%
    as.character()


  name_replace_m_v <- c(sn_first_names_combinations[["Croatia"]]$fixed_first_name)
  names(name_replace_m_v) <- c(sn_first_names_combinations[["Croatia"]]$original_first_name)


  if (split_string[1] %in% names(name_replace_m_v)) {
    split_string[1] <- stringr::str_replace_all(string = split_string[1], name_replace_m_v)

    if (length(split_string) > 1) {
      for (j in 2:length(split_string)) {
        if (stringr::str_detect(string = split_string[j], pattern = "a$")) {
          split_string[j] <- stringr::str_replace(
            string = split_string[j],
            pattern = "a$",
            replacement = ""
          )
        } else if (stringr::str_detect(string = split_string[j], pattern = "og$")) {
          split_string[j] <- stringr::str_replace(
            string = split_string[j],
            pattern = "og$",
            replacement = "i"
          )
        } else if (stringr::str_detect(string = split_string[j], pattern = "e$")) {
          split_string[j] <- stringr::str_replace(
            string = split_string[j],
            pattern = "e$",
            replacement = "a"
          )
        }
      }
    }


    return(stringr::str_c(split_string, collapse = " ") %>%
      stringr::str_squish())
  } else {
    for (j in seq_along(split_string)) {
      if (stringr::str_detect(string = split_string[j], pattern = "ska$")) {
        split_string[j] <- stringr::str_replace(
          string = split_string[j],
          pattern = "ska$",
          replacement = ""
        )
      } else if (stringr::str_detect(string = split_string[j], pattern = "ski$")) {
        split_string[j] <- stringr::str_replace(
          string = split_string[j],
          pattern = "ski$",
          replacement = ""
        )
      } else if (stringr::str_detect(string = split_string[j], pattern = "ečka$")) {
        split_string[j] <- stringr::str_replace(
          string = split_string[j],
          pattern = "ečka$",
          replacement = ""
        )
      } else if (stringr::str_detect(string = split_string[j], pattern = "ačka$")) {
        split_string[j] <- stringr::str_replace(
          string = split_string[j],
          pattern = "ačka$",
          replacement = ""
        )
      } else if (stringr::str_detect(string = split_string[j], pattern = "inska$")) {
        split_string[j] <- stringr::str_replace(
          string = split_string[j],
          pattern = "inska$",
          replacement = ""
        )
      } else if (stringr::str_detect(string = split_string[j], pattern = "eva$")) {
        split_string[j] <- stringr::str_replace(
          string = split_string[j],
          pattern = "eva$",
          replacement = ""
        )
      } else if (stringr::str_detect(string = split_string[j], pattern = "a$")) {
        split_string[j] <- stringr::str_replace(
          string = split_string[j],
          pattern = "a$",
          replacement = ""
        )
      }
    }


    return(stringr::str_c(split_string, collapse = " ") %>%
      stringr::str_squish())
  }
  street_name
}




#' Clean Ukrainian street name
#'
#' @param street_name Name of street, after having removed street part.
#'
#' @return A character vector of length one. A name that can more easily be searched.
#' @export
#'
#' @examples
#'
#' sn_clean_street_name_ukrainian("Володимира Висоцького")
#' sn_clean_street_name_ukrainian("Кисловодська")
#' sn_clean_street_name_ukrainian("Кисловодський")
#' sn_clean_street_name_ukrainian("Миколи Терещенка")
sn_clean_street_name_ukrainian <- function(street_name) {
  split_string <- stringr::str_split(
    string = street_name,
    pattern = "[[:space:]]",
    simplify = TRUE
  ) %>%
    as.character()

  for (j in 1:length(split_string)) {
    if (stringr::str_detect(string = split_string[j], pattern = "нка$")) {
      split_string[j] <- stringr::str_replace(
        string = split_string[j],
        pattern = "нка$",
        replacement = "нко"
      )
    } else if (stringr::str_detect(string = split_string[j], pattern = "дний$")) {
      split_string[j] <- stringr::str_replace(
        string = split_string[j],
        pattern = "дний$",
        replacement = "д"
      )
    } else if (stringr::str_detect(string = split_string[j], pattern = "дна$")) {
      split_string[j] <- stringr::str_replace(
        string = split_string[j],
        pattern = "дна$",
        replacement = ""
      )
    } else if (stringr::str_detect(string = split_string[j], pattern = "ля$")) {
      split_string[j] <- stringr::str_replace(
        string = split_string[j],
        pattern = "ля$",
        replacement = "ль"
      )
    } else if (stringr::str_detect(string = split_string[j], pattern = "ський$")) {
      split_string[j] <- stringr::str_replace(
        string = split_string[j],
        pattern = "ський$",
        replacement = ""
      )
    } else if (stringr::str_detect(string = split_string[j], pattern = "ська$")) {
      split_string[j] <- stringr::str_replace(
        string = split_string[j],
        pattern = "ська$",
        replacement = ""
      )
    } else if (stringr::str_detect(string = split_string[j], pattern = "а$")) {
      split_string[j] <- stringr::str_replace(
        string = split_string[j],
        pattern = "а$",
        replacement = ""
      )
    } else if (stringr::str_detect(string = split_string[j], pattern = "ого$")) {
      split_string[j] <- stringr::str_replace(
        string = split_string[j],
        pattern = "ого$",
        replacement = "ий"
      )
    } else if (stringr::str_detect(string = split_string[j], pattern = "ий$")) {
      split_string[j] <- stringr::str_replace(
        string = split_string[j],
        pattern = "ий$",
        replacement = ""
      )
    } else if (stringr::str_detect(string = split_string[j], pattern = "и$")) {
      split_string[j] <- stringr::str_replace(
        string = split_string[j],
        pattern = "и$",
        replacement = "а"
      )
    } else if (stringr::str_detect(string = split_string[j], pattern = "ії$")) {
      split_string[j] <- stringr::str_replace(
        string = split_string[j],
        pattern = "ії$",
        replacement = "ія"
      )
    } else if (stringr::str_detect(string = split_string[j], pattern = "я$")) {
      split_string[j] <- stringr::str_replace(
        string = split_string[j],
        pattern = "я$",
        replacement = "й"
      )
    }
  }
  return(stringr::str_c(split_string, collapse = " ") %>%
    stringr::str_squish())
}
