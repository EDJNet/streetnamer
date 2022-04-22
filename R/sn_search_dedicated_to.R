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
                                   streets_sf = NULL,
                                   cache = TRUE,
                                   overwrite_cache = FALSE,
                                   connection = NULL,
                                   disconnect_db = TRUE) {
  country_code <- stringr::str_extract(string = gisco_id, pattern = "[A-Z][A-Z]")
  country_name <- sn_country_codes %>%
    dplyr::filter(Code == country_code) %>%
    dplyr::pull(Name)

  if (is.null(search_language)) {
    search_language <- sn_language_defaults_by_country %>%
      dplyr::filter(country == country_name) %>%
      dplyr::pull(language_code)
  }

  if (length(search_language)==0) {
    search_language <- tidywikidatar::tw_get_language()
  } else if (length(search_language)>1) {
    search_language <- search_language[1]
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

  db_connection <- tidywikidatar::tw_connect_to_cache(connection = connection)
  
  search_results_df <- tidywikidatar::tw_search(
    search = current_street_names_df[["name_clean"]],
    language = search_language,
    include_search = TRUE,
    cache = cache,
    cache_connection = db_connection,
    overwrite_cache = overwrite_cache,
    disconnect_db = disconnect_db
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
