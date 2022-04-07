#' Try to guess to whom a street is dedicated to
#'
#' @param gisco_id
#' @param search_language Defaults to NULL, guessed based on country.
#' @param streets_sf Defaults to NULL. If given, used to speed up processing.
#'   Must be an sf object such as the ones output by `ll_osm_get_roads()`.
#'   Passed to `ll_osm_get_lau_streets()`.
#'
#' @return
#' @export
#'
#' @examples
sn_search_dedicated_to <- function(gisco_id,
                                   search_language = NULL,
                                   streets_sf = NULL) {
  country_code <- stringr::str_extract(string = gisco_id, pattern = "[A-Z][A-Z]")
  country_name <- sn_country_codes %>%
    dplyr::filter(Code == country_code) %>%
    dplyr::pull(Name)

  if (is.null(search_language)) {
    search_language <- sn_language_defaults_by_country %>%
      dplyr::filter(country == country_name) %>%
      dplyr::pull(language_code)
  }

  current_street_names_df <- latlon2map::ll_osm_get_lau_streets(
    gisco_id = gisco_id,
    unnamed_streets = FALSE,
    streets_sf = streets_sf
  ) %>%
    sf::st_drop_geometry() %>%
    dplyr::distinct(name) %>%
    dplyr::mutate(name_clean = sn_clean_street_name(
      street_name = name,
      country = country_name
    ))

  search_results_df <- tidywikidatar::tw_search(
    search = current_street_names_df[["name_clean"]],
    language = search_language,
    include_search = TRUE
  ) %>%
    dplyr::group_by(search) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(name_clean = search)


  current_street_names_df %>%
    dplyr::left_join(
      y = search_results_df,
      by = "name_clean"
    )
}
