#' Get name_clean_df for cities with bilingual city names
#'
#' So far supported:
#'
#' - major Belgian cities
#' - South Tyrol
#'
#' @param gisco_id A gisco identifier
#' @param languages Couple of languages. Supported: "french-flemish"; "italian-german"
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'   if(interactive()) {
#'     library("latlon2map")
#'     
#'     options(timeout = 60000) # big timeout, as big downloads needed 
#'     
#'     
#'     ll_set_folder(path = fs::path(fs::path_home_r(),
#'                                   "R",
#'                                   "ll_data"))
#'     
#'     sn_get_clean_street_name_bilingual_df(gisco_id = "IT_021008")
#'     sn_get_clean_street_name_bilingual_df(gisco_id = "IT_021051")
#'     
#'   }
#' }

sn_get_clean_street_name_bilingual_df <- function(gisco_id,
                                                  street_names_df = NULL,
                                                  languages = NULL) {
  if (is.null(street_names_df) == FALSE) {
    current_street_names_df <- street_names_df %>%
      dplyr::distinct(name)
  } else {
    current_street_names_df <- latlon2map::ll_osm_get_lau_streets(
      gisco_id = gisco_id,
      unnamed_streets = FALSE,
      streets_sf = NULL
    ) %>%
      sf::st_drop_geometry() %>%
      dplyr::distinct(name)
  }

  if (is.null(languages) == FALSE) {
    if (languages == "french-flemish") {
      languages  <- "fr_fl"
    }  else if (languages == "italian-german") {
      languages  <- "it_de"
    } else if (languages == "german-italian") {
      languages  <- "de_it"
    }
  } else {
    if (gisco_id %in% c("BE100",
                        "BE_21004",
                        "BE_21015",
                        "BE_21001",
                        "BE_21012",
                        "BE_21016")) {
      languages  <- "fr_fl"
    } else if (gisco_id %in% sn_bilingual_gisco_id$gisco_id) {
      current_gisco_id <- gisco_id
      languages <- sn_bilingual_gisco_id %>% 
        dplyr::filter(gisco_id == current_gisco_id) %>% 
        dplyr::pull(languages)
    } else {
      languages = "other"
    }
  }

  if (languages == "fr_fl") {
    name_clean_df <- current_street_names_df %>%
      tidyr::separate(col = name,
                      into = c("French",
                               "Flemish"),
                      sep = " - ",
                      remove = FALSE) %>%
      dplyr::transmute(name,
                       name_clean = French) %>%
      dplyr::mutate(name_clean = sn_clean_street_name(
        street_name = .data$name_clean,
        country = "Belgium"
      ))
  } else if (languages == "de_it") {
    name_clean_df <- current_street_names_df %>%
      tidyr::separate(col = name,
                      into = c("German",
                               "Italian"),
                      sep = " - ",
                      remove = FALSE) %>%
      dplyr::transmute(name,
                       name_clean = German) %>%
      dplyr::mutate(name_clean = sn_clean_street_name(
        street_name = .data$name_clean,
        country = "Germany"
      ))
  } else if (languages == "it_de") {
    name_clean_df <- current_street_names_df %>%
      tidyr::separate(col = name,
                      into = c("Italian",
                               "German"),
                      sep = " - ",
                      remove = FALSE) %>%
      dplyr::transmute(name,
                       name_clean = Italian) %>%
      dplyr::mutate(name_clean = sn_clean_street_name(
        street_name = .data$name_clean,
        country = "Italy"
      ))
  } else {
    usethis::ui_warn("Doing standard name cleaning, as no custom rule for the given gisco_id is currently available.")
    name_clean_df <- current_street_names_df %>%
      dplyr::mutate(name_clean = sn_clean_street_name(
        street_name = name,
        country = stringr::str_extract(string = gisco_id,
                                       pattern = "[[:alpha:]][[:alpha:]]") %>%
          stringr::str_to_upper()
      ))
  }

  output_df <- name_clean_df %>%
    dplyr::mutate(
      name = name %>% 
        stringr::str_replace_all(pattern = stringr::fixed("\\"),
                                 replacement = " ") %>%
        stringr::str_squish(),
      name_clean = name_clean %>% 
        stringr::str_replace_all(pattern = stringr::fixed("\\"),
                                 replacement = " ") %>%
        stringr::str_squish()
    )

  output_df
}
