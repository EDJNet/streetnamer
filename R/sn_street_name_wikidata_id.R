#' Writes wikidata id for given street name
#'
#'
#' @param df A data frame with two columns: gisco_id and name.
#' @param country Two letter country code.
#' @param overwrite Logical, defaults to FALSE. If TRUE, it first deletes all rows associated with the item(s) included in `item_df`. Useful if the original Wikidata object has been updated.
#' @param connection Defaults to NULL. If NULL, and caching is enabled, `streetnamer` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#'
#' sn_set_data_folder(path = tempdir())
#' sn_write_street_name_wikidata_id(gisco_id = "IT_022205",
#'                                  street_name = "Belvedere San Francesco",
#'                                  wikidata_id = "Q676555",
#'                                  category = NA,
#'                                  checked = TRUE)
#' 
#' sn_get_street_name_wikidata_id(gisco_id = "IT_022205",
#'                                street_name = "Belvedere San Francesco")

sn_write_street_name_wikidata_id <- function(gisco_id,
                                             street_name,
                                             wikidata_id,
                                             gender,
                                             category,
                                             checked,
                                             overwrite = FALSE,
                                             connection = NULL,
                                             disconnect_db = TRUE) {
  gisco_id <- stringr::str_to_upper(gisco_id)
  country <- stringr::str_extract(string = gisco_id, pattern = "[A-Z][A-Z]")
  
  db <- sn_connect_to_db(
    connection = connection,
    country = stringr::str_to_lower(country),
    type = "street_name_wikidata_id"
  )
  
  df <- tibble::tibble(gisco_id = as.character(gisco_id),
                       street_name = as.character(street_name),
                       wikidata_id = as.character(wikidata_id),
                       gender = as.character(gender),
                       category = as.character(category),
                       checked = as.integer(checked))
  
  table_name <- sn_get_db_table_name(type = "street_name_wikidata_id",
                                     country = country)
  
  if (DBI::dbExistsTable(conn = db, name = table_name) == FALSE) {
    if (overwrite == TRUE) {
      DBI::dbWriteTable(db,
                        name = table_name,
                        value = df,
                        append = TRUE)
    } else {
      # do nothing: if table does not exist, previous data cannot be there 
    }
  } else {
    previously_available <- dplyr::tbl(src = db, table_name) %>%
      dplyr::filter(.data$gisco_id %in% stringr::str_c(gisco_id), 
                    .data$street_name %in% stringr::str_c(street_name)) %>% 
      dplyr::pull(.data$street_name) %>% 
      length() %>% 
      as.logical()
    
    if (previously_available==FALSE) {
      DBI::dbWriteTable(db,
                        name = table_name,
                        value = df,
                        append = TRUE
      )
    } else {
      if (overwrite == TRUE) {
        statement <- glue::glue_sql("DELETE FROM {`table_name`} WHERE gisco_id = {gisco_id*} AND street_name = {street_name*}",
                                    gisco_id = unique(df$gisco_id),
                                    table_name = table_name,
                                    street_name = street_name,
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


#' Retrieve combination of municipality/street name/wikidata id and category from database.
#'
#' @param id A characther vector, must start with Q, e.g. "Q180099" for the anthropologist Margaret Mead. Can also be a data frame of one row, typically generated with `tw_search()` or a combination of `tw_search()` and `tw_filter_first()`.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param connection Defaults to NULL. If NULL, and caching is enabled, `streetnamer` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection open.
#'
#' @return If data present in cache, returns a data frame with cached data.
#' @export
#'
#' @examples
#'
#'
#' sn_set_data_folder(path = tempdir())
#' sn_write_street_name_wikidata_id(gisco_id = "IT_022205",
#'                                  street_name = "Belvedere San Francesco",
#'                                  wikidata_id = "Q676555",
#'                                  category = NA,
#'                                  checked = TRUE)
#' 
#' sn_get_street_name_wikidata_id(gisco_id = "IT_022205",
#'                                street_name = "Belvedere San Francesco")
sn_get_street_name_wikidata_id <- function(gisco_id,
                                           street_name,
                                           connection = NULL,
                                           disconnect_db = TRUE) {
  gisco_id <- stringr::str_to_upper(gisco_id)
  country <- stringr::str_extract(string = gisco_id, pattern = "[A-Z][A-Z]")
  
  db <- sn_connect_to_db(
    connection = connection,
    country = country,
    type = "street_name_wikidata_id"
  )
  
  table_name <- sn_get_db_table_name(
    type = "street_name_wikidata_id",
    country = country
  )
  
  if (DBI::dbExistsTable(conn = db, name = table_name) == FALSE) {
    if (disconnect_db == TRUE) {
      DBI::dbDisconnect(db)
    }
    return(NULL)
  }
  
  db_result <- tryCatch(
    dplyr::tbl(src = db, table_name) %>%
      dplyr::filter(.data$gisco_id %in% stringr::str_c(gisco_id), 
                    .data$street_name %in% stringr::str_c(street_name)),
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

