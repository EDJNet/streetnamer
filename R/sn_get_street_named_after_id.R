#' Retrieve combination of municipality/street name/wikidata id and category
#' from database.
#'
#'
#' @param country A two letter country code. See
#'   `unique(sn_lau_by_nuts$country)` for available values.
#' @param gisco_id A characther vector of length one, must correpond to a gisco
#'   id. See `sn_lau_by_nuts` for available values.
#' @param street_name A characther vector of length one, a street name. If not
#'   given, returns added data on all streets for given municipality.
#' @param language Defaults to language set with `tw_set_language()`; if not
#'   set, "en". Use "all_available" to keep all languages. For available
#'   language values, see
#'   https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param connection Defaults to NULL. If NULL, and caching is enabled,
#'   `streetnamer` will use a local sqlite database. A custom connection to
#'   other databases can be given (see vignette `caching` for details).
#' @param include_checked_elsewhere_in_country Defaults to FALSE. If TRUE,
#'   retrieves all streets from the whole country, keeps the most recent, and
#'   includes it in the final output if no local match is found or the national
#'   match is more recent.
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection open.
#' @return If data present in database, returns a data frame with relevant data
#'   stored in the database.
#' @export
#'
#' @examples
#'
#'
#' sn_set_data_folder(path = tempdir())
#' sn_write_street_named_after_id(
#'   gisco_id = "IT_022205",
#'   street_name = "Belvedere San Francesco",
#'   named_after_id = "Q676555",
#'   country = "IT",
#'   person = TRUE,
#'   category = NA,
#'   session = as.character(floor(runif(1) * 1e20)),
#'   time = Sys.time(),
#'   checked = TRUE
#' )
#'
#' sn_get_street_named_after_id(
#'   gisco_id = "IT_022205",
#'   street_name = "Belvedere San Francesco",
#'   country = "IT"
#' )
sn_get_street_named_after_id <- function(country = NULL,
                                         gisco_id = NULL,
                                         street_name = NULL,
                                         streets_sf = NULL,
                                         lau_year = 2020,
                                         keep_only_latest = TRUE,
                                         only_checked = FALSE,
                                         remove_ignored = TRUE,
                                         only_ignored = FALSE,
                                         include_checked_elsewhere_in_country = FALSE,
                                         language = tidywikidatar::tw_get_language(),
                                         connection = NULL,
                                         disconnect_db = TRUE) {
  if (is.null(gisco_id) == FALSE) {
    gisco_id <- stringr::str_to_upper(gisco_id)
  }
  
  if (is.null(country)) {
    country <- stringr::str_extract(
      string = stringr::str_to_upper(string = gisco_id),
      pattern = "[A-Z][A-Z]"
    )
  } else {
    country <- stringr::str_to_upper(country)
  }
  
  db <- tidywikidatar::tw_connect_to_cache(
    connection = connection,
    language = language,
    cache = TRUE
  )
  
  table_name <- sn_get_db_table_name(
    type = "street_named_after_id",
    country = country
  )
  
  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    tidywikidatar::tw_disconnect_from_cache(
      cache = TRUE,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
    )
    
    return(sn_empty_street_named_after_id)
  }
  
  if (include_checked_elsewhere_in_country == TRUE) {
    if (is.null(streets_sf)) {
      current_country_code <- country
      if (current_country_code == "UK") {
        # check if northern ireland
        if (stringr::str_starts(string = current_gisco_id, pattern = "UK_N")) {
          current_country_name <- "ireland-and-northern-ireland"
        } else {
          current_country_name <- "great-britain"
        }
      } else if (current_country_code == "IE") {
        current_country_name <- "ireland-and-northern-ireland"
      } else if (current_country_code == "MD") {
        current_country_name <- "moldova"
      } else {
        current_country_name <- NULL
      }
      
      
      
      streets_df <- latlon2map::ll_osm_get_lau_streets(
        gisco_id = gisco_id,
        country = current_country_name,
        unnamed_streets = FALSE,
        year = lau_year
      ) %>%
        sf::st_drop_geometry()
    } else {
      streets_df <- streets_sf %>%
        sf::st_drop_geometry()
    }
    
    # get data for all country
    db_result <- tryCatch(
      dplyr::tbl(src = db, table_name),
      error = function(e) {
        logical(1L)
      }
    )
    
    if (isFALSE(db_result)) {
      tidywikidatar::tw_disconnect_from_cache(
        cache = TRUE,
        cache_connection = db,
        disconnect_db = disconnect_db,
        language = language
      )
      return(sn_empty_street_named_after_id)
    }
    
    country_df <- streets_df %>%
      dplyr::rename(street_name = name) %>%
      dplyr::left_join(
        y = db_result,
        by = "street_name",
        copy = TRUE
      ) %>%
      dplyr::filter(is.na(checked) == FALSE) %>%
      dplyr::filter(checked == 1) %>%
      tidyr::replace_na(replace = list(named_after_n = 1)) %>%
      dplyr::arrange(dplyr::desc(time)) %>%
      dplyr::group_by(street_name) %>%
      dplyr::mutate(
        named_after_n_id = dplyr::row_number(),
        named_after_n_to_keep = dplyr::first(named_after_n)
      ) %>%
      dplyr::filter(named_after_n_id <= named_after_n_to_keep) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(gisco_id, street_name, named_after_id, named_after_n_id, .keep_all = TRUE) %>%
      dplyr::select(-named_after_n_id, -named_after_n_to_keep)
  }
  
  if (is.null(street_name) == FALSE & is.null(gisco_id) == FALSE) {
    # if both street and gisco given, return for current street only
    db_result <- tryCatch(
      dplyr::tbl(src = db, table_name) %>%
        dplyr::filter(
          .data$gisco_id %in% !!stringr::str_c(gisco_id),
          .data$street_name %in% !!stringr::str_c(street_name)
        ),
      error = function(e) {
        logical(1L)
      }
    )
  } else if (is.null(street_name) == FALSE & is.null(gisco_id) == TRUE) {
    # if street name given, but not municipality, return all streets with given name
    street_name_to_filter <- street_name
    db_result <- tryCatch(
      dplyr::tbl(src = db, table_name) %>%
        dplyr::filter(
          street_name %in% street_name_to_filter
        ),
      error = function(e) {
        logical(1L)
      }
    )
  } else if (is.null(street_name) == TRUE & is.null(gisco_id) == FALSE) {
    # if street name not given, but municipality given, return all streets for given municipality
    
    db_result <- tryCatch(
      dplyr::tbl(src = db, table_name) %>%
        dplyr::filter(
          .data$gisco_id %in% !!stringr::str_c(gisco_id),
        ),
      error = function(e) {
        logical(1L)
      }
    )
  } else if (is.null(street_name) == TRUE & is.null(gisco_id) == TRUE) {
    # if only country given, return for all country
    db_result <- tryCatch(
      dplyr::tbl(src = db, table_name),
      error = function(e) {
        logical(1L)
      }
    )
  }
  
  if (isFALSE(db_result)) {
    tidywikidatar::tw_disconnect_from_cache(
      cache = TRUE,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
    )
    return(sn_empty_street_named_after_id)
  }
  
  if (include_checked_elsewhere_in_country == TRUE) {
    db_result <- dplyr::bind_rows(
      db_result %>% dplyr::collect(),
      country_df
    )
  }
  
  if (only_ignored == TRUE) {
    db_result <- db_result %>%
      dplyr::filter(ignore == 1)
  }
  
  if (only_checked == TRUE) {
    db_result <- db_result %>%
      dplyr::filter(checked == 1)
  }
  
  
  if (remove_ignored == TRUE) {
    db_result <- db_result %>%
      dplyr::filter(is.na(ignore) | ignore == 0)
  }
  
  
  street_names_df <- db_result %>%
    dplyr::collect() %>%
    tibble::as_tibble()
  
  if (keep_only_latest == TRUE) {
    street_names_df <- street_names_df %>%
      tidyr::replace_na(replace = list(named_after_n = 1)) %>%
      dplyr::arrange(dplyr::desc(time)) %>%
      dplyr::mutate(
        named_after_n =
          dplyr::if_else(condition = is.na(named_after_n),
                         true = as.numeric(1),
                         false = as.numeric(named_after_n)
          )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(street_name) %>%
      dplyr::mutate(
        named_after_n_id = dplyr::row_number(),
        named_after_n_to_keep = dplyr::first(named_after_n)
      ) %>%
      dplyr::filter(named_after_n_id <= named_after_n_to_keep) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(gisco_id, street_name, named_after_id, named_after_n_id, .keep_all = TRUE) %>%
      dplyr::select(-named_after_n_id, -named_after_n_to_keep)
  }
  
  tidywikidatar::tw_disconnect_from_cache(
    cache = TRUE,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )
  
  street_names_df
}
