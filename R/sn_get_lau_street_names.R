#' Retrieve cached item
#'
#' @param id A characther vector, must start with Q, e.g. "Q180099" for the anthropologist Margaret Mead. Can also be a data frame of one row, typically generated with `tw_search()` or a combination of `tw_search()` and `tw_filter_first()`.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection open.
#'
#' @return If data present in cache, returns a data frame with cached data.
#' @export
#'
#' @examples
#'
#'
#' tw_set_cache_folder(path = tempdir())
#' tw_enable_cache()
#' tw_create_cache_folder(ask = FALSE)
#'
#' df_from_api <- tw_get(id = "Q180099", language = "en")
#'
#' df_from_cache <- tw_get_cached_item(
#'   id = "Q180099",
#'   language = "en"
#' )
sn_get_lau_street_names <- function(gisco_id,
                                    country,
                                    connection = NULL,
                                    disconnect_db = TRUE) {
  db <- sn_connect_to_db(
    connection = connection,
    country = country,
    type = "osm_lau_street_names"
  )

  table_name <- sn_get_db_table_name(
    type = "osm_lau_street_names",
    country = country
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    if (disconnect_db == TRUE) {
      DBI::dbDisconnect(db)
    }
    return(NULL)
  }

  db_result <- tryCatch(
    dplyr::tbl(src = db, table_name) %>%
      dplyr::filter(.data$gisco_id %in% !!stringr::str_to_upper(gisco_id)),
    error = function(e) {
      logical(1L)
    }
  )
  if (isFALSE(db_result)) {
    if (disconnect_db == TRUE) {
      DBI::dbDisconnect(db)
    }
    return(NULL)
  }

  street_names_df <- db_result %>%
    tibble::as_tibble()

  if (disconnect_db == TRUE) {
    DBI::dbDisconnect(db)
  }

  street_names_df
}


#' Get gisco id from name of municipality and country
#'
#' Warning: there are many instances where there are more than one municipality with the same name in a country.
#'
#' @param name Name of municipality
#' @param country Two-letter country code.
#'
#' @return
#' @export
#'
#' @examples
#'
#' sn_get_gisco_id(name = "Trento", country = "IT")
sn_get_gisco_id <- function(name, country) {
  ll_get_lau_eu() %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(CNTR_CODE == country, LAU_NAME == name) %>%
    dplyr::pull(GISCO_ID)
}
