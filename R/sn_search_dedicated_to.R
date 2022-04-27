#' Try to guess to whom a street is dedicated to
#'
#' @param gisco_id
#' @param search_language Defaults to NULL, guessed based on country.
#' @param response_language Defaults to `tidywikidatar::tw_get_language()`. Used
#'   for defining the language in which label and description are returned.
#' @param check_named_after_original Defaults to TRUE. If TRUE, a search is
#'   performed on the original street name. If the first result has values for
#'   the property "named after", this takes precedence over other methods.
#' @param check_named_after Defaults to TRUE. If TRUE, a search is performed on
#'   the "cleaned" name of the street. If the result is a street, road, square,
#'   or similar, and this has values for the property "named after", this is
#'   kept instead of the standard method.
#' @param streets_sf Defaults to NULL. If given, used to speed up processing.
#'   Must be an sf object such as the ones output by `ll_osm_get_roads()`.
#'   Passed to `ll_osm_get_lau_streets()`.
#'
#' @return
#' @export
#'
#' @examples
sn_search_named_after <- function(gisco_id,
                                  search_language = NULL,
                                  response_language = tidywikidatar::tw_get_language(),
                                  check_named_after_original = TRUE,
                                  check_named_after = TRUE,
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

  if (length(search_language) == 0) {
    search_language <- tidywikidatar::tw_get_language()
  } else if (length(search_language) > 1) {
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


  if (check_named_after_original) {
    search_no_clean_df <- tidywikidatar::tw_search(
      search = current_street_names_df[["name"]],
      language = search_language,
      response_language = response_language,
      include_search = TRUE,
      cache = cache,
      cache_connection = db_connection,
      overwrite_cache = overwrite_cache,
      disconnect_db = FALSE
    ) %>%
      dplyr::group_by(search) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::rename(name = search) %>%
      dplyr::mutate(
        named_after = tidywikidatar::tw_get_p1(id,
          p = "P138",
          language = response_language,
          cache = cache,
          disconnect_db = FALSE,
          overwrite_cache = overwrite_cache,
          cache_connection = db_connection
        )
      )

    named_after_original_df <- search_no_clean_df %>%
      dplyr::filter(is.na(named_after) == FALSE) %>%
      dplyr::mutate(
        named_after_label = tidywikidatar::tw_get_label(
          id = .data$named_after,
          language = response_language,
          cache = cache,
          overwrite_cache = overwrite_cache,
          cache_connection = db_connection,
          disconnect_db = FALSE
        ),
        named_after_description = tidywikidatar::tw_get_description(
          id = .data$named_after,
          language = response_language,
          cache = cache,
          overwrite_cache = overwrite_cache,
          cache_connection = db_connection,
          disconnect_db = FALSE
        )
      )
  }


  search_results_df <- tidywikidatar::tw_search(
    search = current_street_names_df[["name_clean"]],
    language = search_language,
    response_language = response_language,
    include_search = TRUE,
    cache = cache,
    cache_connection = db_connection,
    overwrite_cache = overwrite_cache,
    disconnect_db = FALSE
  ) %>%
    dplyr::group_by(search) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(name_clean = search)

  if (check_named_after == TRUE) {
    named_after_df %>%
      tw_get_property(
        id = search_results_df[["id"]],
        p = "P31",
        language = response_language,
        cache = cache,
        disconnect_db = FALSE,
        overwrite_cache = overwrite_cache,
        cache_connection = db_connection
      ) %>%
      dplyr::filter(.data$value %in% c(
        "Q79007", # street
        "Q174782", # square
        "Q12280", # bridge
        "Q3352369", # footpath
        "Q34442", # road
        "Q54114" # boulevard
      )) %>%
      dplyr::left_join(
        y = search_results_df,
        by = "id"
      ) %>%
      dplyr::distinct(.data$id, .keep_all = TRUE) %>%
      dplyr::mutate(
        named_after = tidywikidatar::tw_get_p1(id,
          p = "P138",
          language = response_language,
          cache = cache,
          disconnect_db = FALSE,
          overwrite_cache = overwrite_cache,
          cache_connection = db_connection
        )
      ) %>%
      dplyr::filter(is.na(named_after) == FALSE) %>%
      dplyr::mutate(
        named_after_label = tidywikidatar::tw_get_label(
          id = .data$named_after,
          language = response_language,
          cache = cache,
          overwrite_cache = overwrite_cache,
          cache_connection = db_connection,
          disconnect_db = FALSE
        ),
        named_after_description = tidywikidatar::tw_get_description(
          id = .data$named_after,
          language = response_language,
          cache = cache,
          overwrite_cache = overwrite_cache,
          cache_connection = db_connection,
          disconnect_db = FALSE
        )
      )
  }



  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db_connection,
    disconnect_db = disconnect_db,
    language = language
  )

  output_street_name_df <- current_street_names_df

  exclude_v <- as.character(NA)[FALSE]

  if (check_named_after_original == TRUE) {
    output_df <- current_street_names_df %>%
      dplyr::left_join(
        y = named_after_original_df %>%
          dplyr::transmute(.data$name,
            id = .data$named_after,
            label = .data$named_after_label,
            description = .data$named_after_description
          ),
        by = "name"
      ) %>%
      dplyr::filter(is.na(.data$id) == FALSE)

    exclude_v <- output_df[["name"]]
  }

  if (check_named_after == TRUE) {
    processed_df <- current_street_names_df %>%
      dplyr::filter(!.data$name %in% exclude_v) %>%
      dplyr::left_join(
        y = named_after_df %>%
          dplyr::transmute(.data$name_clean,
            id = .data$named_after,
            label = .data$named_after_label,
            description = .data$named_after_description
          ),
        by = "name_clean"
      ) %>%
      dplyr::filter(is.na(.data$id) == FALSE)

    exclude_v <- unique(c(exclude_v, processed_df[["name"]]))

    if (check_named_after_original) {
      output_df <- dplyr::bind_rows(
        output_df,
        processed_df
      ) %>%
        dplyr::distinct(.data$name)
    } else {
      output_df <- processed_df
    }
  }

  if (check_named_after_original | check_named_after) {
    output_df <- dplyr::bind_rows(
      output_df,
      current_street_names_df %>%
        dplyr::filter(!.data$name %in% exclude_v) %>%
        dplyr::left_join(
          y = search_results_df,
          by = "name_clean"
        ) %>%
        dplyr::distinct(.data$name)
    )
  } else {
    output_df <- current_street_names_df %>%
      dplyr::filter(!.data$name %in% exclude_v) %>%
      dplyr::left_join(
        y = search_results_df,
        by = "name_clean"
      )
  }

  current_street_names_df %>%
    dplyr::left_join(
      y = output_df %>%
        dplyr::select(-.data$name_clean),
      by = "name"
    )
}
