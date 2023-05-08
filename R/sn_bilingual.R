#' Get name_clean_df for cities with bilingual city names
#'
#' So far supported:
#'
#' - Belgian cities
#' - South Tyrol
#'
#' Please file an issue if you find issues.
#'
#' @param gisco_id A gisco identifier
#' @param languages Couple of languages in the format "it_de" for cities where
#'   the name of the street is written first in Italian, then there's an hypen,
#'   and then in German; "de_it" if it's vice versa.
#' @param lau_year Defaults to 2020 for internal consistency. Passed to `latlon2map::ll_osm_get_lau_streets()`.
#'
#' @return A data frame with two columns, `name` and `name_clean`. `name_clean`
#'   should have the "street" part of a street name removed as well as the part 
#'   in the secondary language.
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   library("latlon2map")
#'
#'   options(timeout = 60000) # big timeout, as big downloads needed
#'
#'
#'   ll_set_folder(path = fs::path(
#'     fs::path_home_r(),
#'     "R",
#'     "ll_data"
#'   ))
#' 
#' sn_get_clean_street_name_bilingual_df(gisco_id = "IT_021008") %>% 
#'   dplyr::slice_sample(n = 10)
#' sn_get_clean_street_name_bilingual_df(gisco_id = "IT_021051") %>% 
#'   dplyr::slice_sample(n = 10)
#' 
#' sn_get_clean_street_name_bilingual_df(gisco_id = "BE100") %>% 
#'   dplyr::slice_sample(n = 10)
#' sn_get_clean_street_name_bilingual_df(gisco_id = "BE_44021") %>% 
#'   dplyr::slice_sample(n = 10)
#' sn_get_clean_street_name_bilingual_df(gisco_id = "BE_62063") %>% 
#'   dplyr::slice_sample(n = 10)
#' }
#' }
sn_get_clean_street_name_bilingual_df <- function(gisco_id,
                                                  street_names_df = NULL,
                                                  languages = NULL,
                                                  lau_year = 2020) {
  if (is.null(street_names_df) == FALSE) {
    current_street_names_df <- street_names_df %>%
      dplyr::distinct(name)
  } else {
    current_street_names_df <- latlon2map::ll_osm_get_lau_streets(
      gisco_id = gisco_id,
      year = lau_year,
      unnamed_streets = FALSE,
      streets_sf = NULL
    ) %>%
      sf::st_drop_geometry() %>%
      dplyr::distinct(name)
  }

  if (gisco_id %in% sn_bilingual_gisco_id$gisco_id) {
    current_gisco_id <- gisco_id
    languages <- sn_bilingual_gisco_id %>%
      dplyr::filter(gisco_id == current_gisco_id) %>%
      dplyr::pull(languages)
  } else {
    languages <- "other"
  }

  if (languages == "other") {
    usethis::ui_warn("Doing standard name cleaning, as no custom rule for the given gisco_id is currently available.")
    name_clean_df <- current_street_names_df %>%
      dplyr::mutate(name_clean = sn_clean_street_name(
        street_name = name,
        country = stringr::str_extract(
          string = gisco_id,
          pattern = "[[:alpha:]][[:alpha:]]"
        ) %>%
          stringr::str_to_upper()
      ))
    return(name_clean_df)
  }

  languages_v <- as.character(stringr::str_split(
    string = languages,
    pattern = "_",
    simplify = TRUE,
    n = 2
  ))
  name_clean_df <- current_street_names_df %>%
    tidyr::separate(
      col = name,
      into = languages_v,
      sep = " - ",
      remove = FALSE
    ) %>%
    dplyr::select(1:2) %>%
    dplyr::rename(name_clean = 2) %>%
    dplyr::mutate(name_clean = sn_clean_street_name(
      street_name = .data$name_clean,
      country = sn_language_defaults_by_country %>%
        dplyr::filter(language_code == languages_v[1]) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::pull(country)
    ))

  output_df <- name_clean_df %>%
    dplyr::mutate(
      name = name %>%
        stringr::str_replace_all(
          pattern = stringr::fixed("\\"),
          replacement = " "
        ) %>%
        stringr::str_squish(),
      name_clean = name_clean %>%
        stringr::str_replace_all(
          pattern = stringr::fixed("\\"),
          replacement = " "
        ) %>%
        stringr::str_squish()
    )

  output_df
}
