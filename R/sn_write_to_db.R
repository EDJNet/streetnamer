#' Writes item to cache
#'
#' Writes item to cache. Typically used internally, but exported to enable custom caching solutions.
#'
#' @param df A data frame with two columns: gisco_id and name.
#' @param type Type of data to be stored, e.g "osm".
#' @param country Two letter country code.
#' @param overwrite Logical, defaults to FALSE. If TRUE, it first deletes all rows associated with the item(s) included in `item_df`. Useful if the original Wikidata object has been updated.
#' @param connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#'
#' tw_set_cache_folder(path = fs::path(tempdir(), paste(sample(letters, 24), collapse = "")))
#' tw_create_cache_folder(ask = FALSE)
#' tw_disable_cache()
#'
#' df_from_api <- tw_get(id = "Q180099", language = "en")
#'
#' df_from_cache <- tw_get_cached_item(
#'   id = "Q180099",
#'   language = "en"
#' )
#'
#' is.null(df_from_cache) # expect TRUE, as nothing has yet been stored in cache
#'
#' tw_write_item_to_cache(
#'   item_df = df_from_api,
#'   language = "en"
#' )
#'
#' df_from_cache <- tw_get_cached_item(
#'   id = "Q180099",
#'   language = "en"
#' )
#'
#' is.null(df_from_cache) # expect a data frame, same as df_from_api
sn_write_lau_street_names <- function(df,
                                      type,
                                      country,
                                      overwrite = FALSE,
                                      connection = NULL,
                                      disconnect_db = TRUE) {
  db <- sn_connect_to_db(
    connection = connection,
    country = stringr::str_to_lower(country),
    type = "osm_lau_street_names"
  )

  table_name <- sn_get_db_table_name(
    type = "osm_lau_street_names",
    country = country
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # do nothing: if table does not exist, previous data cannot be there
  } else {
    previously_available <- dplyr::tbl(src = db, table_name) %>%
      dplyr::distinct(.data$gisco_id) %>%
      dplyr::filter(.data$gisco_id %in% !!stringr::str_to_upper(unique(df$gisco_id))) %>%
      dplyr::pull(.data$gisco_id) %>%
      length() %>%
      as.logical()

    if (previously_available == FALSE) {
      DBI::dbWriteTable(db,
        name = table_name,
        value = df,
        append = TRUE
      )
    } else {
      if (overwrite == TRUE) {
        statement <- glue::glue_sql("DELETE FROM {`table_name`} WHERE id = {gisco_id*}",
          gisco_id = unique(df$gisco_id),
          table_name = table_name,
          .con = db
        )
        result <- DBI::dbExecute(
          conn = db,
          statement = statement
        )
        DBI::dbWriteTable(db,
          name = table_name,
          value = df,
          append = TRUE
        )
      } else {
        # do nothing if data already present and overwrite is set to FALSE
      }
    }
  }

  if (disconnect_db == TRUE) {
    DBI::dbDisconnect(db)
  }
}
