#' Writes wikidata id for given street name
#'
#'
#' @param df A data frame with two columns: gisco_id and name.
#' @param country Two letter country code.
#' @param overwrite Logical, defaults to FALSE. If TRUE, it first deletes all rows associated with the item(s) included in `item_df`. Useful if the original Wikidata object has been updated.
#' @param connection Defaults to NULL. If NULL, and caching is enabled, `streetnamer` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param gisco_id Identifier of a municipality, typically a gisco identifier. Can be any code, as long as it used consistently, and it starts with a two-letter country code.
#' @param dedicated_to_n An integer, defaults to NULL, but most commonly expected to be 1. Input more than one if the street is named after `n` entities.
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#'
#' sn_set_data_folder(path = tempdir())
#'
#' sn_write_street_name_wikidata_id(
#'   gisco_id = "IT_022205",
#'   country = "IT",
#'   street_name = "Belvedere San Francesco",
#'   person = TRUE,
#'   wikidata_id = "Q676555",
#'   gender = "male",
#'   category = "religion",
#'   tag = "",
#'   checked = TRUE,
#'   session = "testing",
#'   append = TRUE,
#'   overwrite = FALSE,
#'   disconnect_db = TRUE
#' )
#' sn_get_street_name_wikidata_id(
#'   gisco_id = "IT_022205",
#'   street_name = "Belvedere San Francesco",
#'   country = "IT"
#' )
sn_write_street_name_wikidata_id <- function(gisco_id = NULL,
                                             street_name = NULL,
                                             country = NULL,
                                             wikidata_id = NULL,
                                             person = NULL,
                                             gender = NULL,
                                             category = NULL,
                                             tag = NULL,
                                             checked = NULL,
                                             ignore = NULL,
                                             dedicated_to_n = NULL,
                                             session = NULL,
                                             overwrite = FALSE,
                                             append = TRUE,
                                             connection = NULL,
                                             language = tidywikidatar::tw_get_language(),
                                             disconnect_db = TRUE,
                                             return_df_only = FALSE,
                                             df_to_write = NULL) {
  print(df_to_write)
  if (is.null(df_to_write) == FALSE & is.data.frame(df_to_write) == TRUE) {
    df <- df_to_write

    if (is.null(country)) {
      country <- df$country
    }

    if (is.null(street_name)) {
      street_name <- df$street_name
    }

    if (is.null(gisco_id)) {
      gisco_id <- df$gisco_id
    }
  } else {
    gisco_id <- stringr::str_to_upper(ifelse(is.null(gisco_id),
      as.character(NA),
      as.character(gisco_id)
    ))
    country <- stringr::str_to_upper(ifelse(is.null(country),
      as.character(NA),
      as.character(country)
    ))

    df <- tibble::tibble(
      gisco_id = ifelse(is.null(gisco_id),
        as.character(NA),
        as.character(gisco_id)
      ),
      street_name = ifelse(is.null(street_name),
        as.character(NA),
        as.character(street_name)
      ),
      country = ifelse(is.null(country),
        as.character(NA),
        as.character(country)
      ),
      wikidata_id = ifelse(is.null(wikidata_id),
        as.character(NA),
        as.character(wikidata_id)
      ),
      person = ifelse(is.null(person),
        as.integer(NA),
        as.integer(person)
      ),
      gender = ifelse(is.null(gender),
        as.character(NA),
        as.character(gender)
      ),
      category = ifelse(is.null(category),
        as.character(NA),
        as.character(category)
      ),
      checked = ifelse(is.null(checked),
        as.integer(NA),
        as.integer(checked)
      ),
      ignore = ifelse(is.null(ignore),
        as.integer(NA),
        as.integer(ignore)
      ),
      dedicated_to_n = ifelse(is.null(dedicated_to_n),
        as.integer(NA),
        as.integer(dedicated_to_n)
      ),
      tag = ifelse(is.null(tag),
        as.character(NA),
        as.character(tag)
      ),
      session = ifelse(is.null(session),
        as.character(NA),
        as.character(session)
      ),
      time = as.numeric(Sys.time())
    )
  }


  if (return_df_only == TRUE) {
    return(df)
  }

  db <- tidywikidatar::tw_connect_to_cache(
    connection = connection,
    language = language,
    cache = TRUE
  )

  table_name <- sn_get_db_table_name(
    type = "street_name_wikidata_id",
    country = country
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # if table does not exist...
    if (overwrite == TRUE | append == TRUE) {
      DBI::dbWriteTable(db,
        name = table_name,
        value = df,
        append = TRUE
      )
    }
  } else {
    # if table exists...
    if (append == TRUE) {
      DBI::dbWriteTable(db,
        name = table_name,
        value = df,
        append = TRUE
      )
    } else {
      previously_available <- dplyr::tbl(
        src = db,
        table_name
      ) %>%
        dplyr::filter(
          .data$gisco_id %in% !!stringr::str_c(gisco_id),
          .data$street_name %in% !!stringr::str_c(street_name)
        ) %>%
        dplyr::pull(.data$street_name) %>%
        length() %>%
        as.logical()

      if (previously_available == FALSE) {
        # if not previously available, then write to database
        if (overwrite == TRUE | append == TRUE) {
          DBI::dbWriteTable(db,
            name = table_name,
            value = df,
            append = TRUE
          )
        }
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
          # do nothing if data already present and both overwrite and append are set to FALSE
        }
      }
    }
  }


  tidywikidatar::tw_disconnect_from_cache(
    cache = TRUE,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )
  df
}


#' Retrieve combination of municipality/street name/wikidata id and category from database.
#'
#' Country must always be given, other parameters optional.
#'
#' @param country A two letter country code. See `unique(sn_lau_by_nuts$country)` for available values.
#' @param gisco_id A characther vector of length one, must correpond to a gisco id. See `sn_lau_by_nuts` for available values.
#' @param street_name A characther vector of length one, a street name.
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
#' sn_write_street_name_wikidata_id(
#'   gisco_id = "IT_022205",
#'   street_name = "Belvedere San Francesco",
#'   wikidata_id = "Q676555",
#'   country = "IT",
#'   person = TRUE,
#'   category = NA,
#'   session = as.character(floor(runif(1) * 1e20)),
#'   time = Sys.time(),
#'   checked = TRUE
#' )
#'
#' sn_get_street_name_wikidata_id(
#'   gisco_id = "IT_022205",
#'   street_name = "Belvedere San Francesco",
#'   country = "IT"
#' )
sn_get_street_name_wikidata_id <- function(country,
                                           gisco_id = NULL,
                                           street_name = NULL,
                                           only_checked = FALSE,
                                           only_ignore = FALSE,
                                           language = tidywikidatar::tw_get_language(),
                                           connection = NULL,
                                           disconnect_db = TRUE) {
  if (is.null(gisco_id) == FALSE) {
    gisco_id <- stringr::str_to_upper(gisco_id)
  }

  country <- stringr::str_to_upper(country)

  db <- tidywikidatar::tw_connect_to_cache(
    connection = connection,
    language = language,
    cache = TRUE,
  )


  table_name <- sn_get_db_table_name(
    type = "street_name_wikidata_id",
    country = country
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    tidywikidatar::tw_disconnect_from_cache(
      cache = TRUE,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
    )

    return(sn_empty_street_name_wikidata_id)
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
    db_result <- tryCatch(
      dplyr::tbl(src = db, table_name) %>%
        dplyr::filter(
          .data$street_name %in% !!stringr::str_c(street_name)
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
    return(sn_empty_street_name_wikidata_id)
  }

  if (only_ignore == TRUE) {
    db_result <- db_result %>%
      dplyr::filter(ignore == 1)
  }

  if (only_checked == TRUE) {
    db_result <- db_result %>%
      dplyr::filter(checked == 1)
  }

  street_names_df <- db_result %>%
    dplyr::collect() %>%
    tibble::as_tibble()

  tidywikidatar::tw_disconnect_from_cache(
    cache = TRUE,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )

  street_names_df
}
