#' Try to guess to whom a street is dedicated to
#'
#' @param gisco_id A gisco identifier of a European LAU.
#' @param search_language Defaults to NULL, guessed based on country.
#' @param response_language Defaults to `tidywikidatar::tw_get_language()`. Used
#'   for defining the language in which label and description are returned.
#' @param check_named_after_original Defaults to TRUE. If TRUE, a search is
#'   performed on the original street name. If the first result has values for
#'   the property "named after", this takes precedence over other methods.
#' @param check_named_after_original_n Defaults to 1.
#' @param check_named_after Defaults to TRUE. If TRUE, a search is performed on
#'   the "cleaned" name of the street. If the result is a street, road, square,
#'   or similar, and this has values for the property "named after", this is
#'   kept instead of the standard method.
#' @param streets_sf Defaults to NULL. If given, used to speed up processing.
#'   Must be an sf object such as the ones output by `ll_osm_get_roads()`.
#'   Passed to `ll_osm_get_lau_streets()`.
#' @param street_names_df Defaults to NULL. If TRUE, must be a data frame with
#'   two columns, named "name" and "name_clean" respectively. If given, these
#'   are passed directly to the search routine. Useful when name cleaning
#'   provided by the package is not satisfying, e.g. in places such as some
#'   Belgian cities where street names are given in more than one language.
#' @param checked_df Defaults to NULL. If given, a data frame with a `name` and
#'   `id`. Takes precedence over searches.
#' @param drop_if_street Defaults to TRUE. If the result found is primarily an
#'   instance of "street", "square", or such, as the result is probably the
#'   street itself, not what or who it is dedicated to.
#'
#' @return
#' @export
#'
#' @examples
sn_search_named_after <- function(gisco_id,
                                  search_language = NULL,
                                  response_language = tidywikidatar::tw_get_language(),
                                  check_named_after_original = TRUE,
                                  check_named_after_original_n = 1,
                                  check_named_after = TRUE,
                                  drop_if_street = TRUE,
                                  drop_if_disambiguation_page = TRUE,
                                  streets_sf = NULL,
                                  street_names_df = NULL,
                                  checked_df = NULL,
                                  cache = TRUE,
                                  overwrite_cache = FALSE,
                                  connection = NULL,
                                  disconnect_db = TRUE) {
  country_code <- stringr::str_extract(string = gisco_id, pattern = "[A-Z][A-Z]")

  country_name <- sn_standard_country(
    country = country_code,
    type = "name"
  )


  if (is.null(search_language)) {
    search_language <- streetnamer::sn_language_defaults_by_country %>%
      dplyr::filter(country == country_name) %>%
      dplyr::pull(language_code)
  }

  if (length(search_language) == 0) {
    search_language <- tidywikidatar::tw_get_language()
  } else if (length(search_language) > 1) {
    search_language <- search_language[1]
  }


  if (is.null(street_names_df)) {
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
      )) %>%
      dplyr::mutate(
        name = name %>% stringr::str_replace_all(pattern = stringr::fixed("\\"), replacement = " ") %>% stringr::str_squish(),
        name_clean = name_clean %>% stringr::str_replace_all(pattern = stringr::fixed("\\"), replacement = " ") %>% stringr::str_squish()
      )
  } else {
    current_street_names_df <- street_names_df %>%
      dplyr::mutate(
        name = name %>% stringr::str_replace_all(pattern = stringr::fixed("\\"), replacement = " ") %>% stringr::str_squish(),
        name_clean = name_clean %>% stringr::str_replace_all(pattern = stringr::fixed("\\"), replacement = " ") %>% stringr::str_squish()
      )
  }

  language_combo <- stringr::str_c(search_language, "_", response_language)

  db_connection <- tidywikidatar::tw_connect_to_cache(
    connection = connection,
    language = language_combo,
    cache = cache
  )
  
  
  table_name <- sn_get_db_table_name(type = "sn_auto_named_after", country = country_code)
  
  if (pool::dbExistsTable(conn = db_connection, name = table_name) == FALSE) {
    # do nothing, if table does not exist, data cannot be there
    previously_cached_df <- sn_empty_auto_named_after
  } else {
    db_result <- tryCatch(
      dplyr::tbl(src = db_connection, table_name) %>%
        dplyr::filter(
          .data$street_name %in% !!stringr::str_c(current_street_names_df$name)
        ),
      error = function(e) {
        logical(1L)
      }
    )
    
    if (isFALSE(db_result)) {
      previously_cached_df <- sn_empty_auto_named_after
    } else {
      previously_cached_df <- tibble::as_tibble(db_result)
    }
    
  }

  current_street_names_df <- current_street_names_df %>% 
    dplyr::anti_join(y = previously_cached_df %>% dplyr::rename(name = street_name), by = "name")

  if (nrow(current_street_names_df)==0) {
    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = db_connection,
      disconnect_db = disconnect_db,
      language = language
    )
    
    return(previously_cached_df)
  }
  
  exclude_v <- as.character(NA)[FALSE]

  if (is.null(checked_df) == FALSE) {
    from_check_pre_df <- current_street_names_df %>%
      dplyr::distinct(.data$name) %>%
      dplyr::left_join(
        y = checked_df %>%
          dplyr::distinct(
            .data$name,
            .data$id
          ),
        by = "name"
      ) %>%
      dplyr::filter(tidywikidatar::tw_check_qid(
        id = .data$id,
        logical_vector = TRUE
      ))

    if (nrow(from_check_pre_df) > 0) {
      from_check_df <- from_check_pre_df %>%
        dplyr::mutate(
          label = tidywikidatar::tw_get_label(
            id = .data$id,
            language = response_language,
            cache = cache,
            overwrite_cache = overwrite_cache,
            cache_connection = db_connection,
            disconnect_db = FALSE
          ),
          description = tw_get_description(
            id = .data$id,
            language = response_language,
            cache = cache,
            overwrite_cache = overwrite_cache,
            cache_connection = db_connection,
            disconnect_db = FALSE
          ),
          named_after_from_wikidata = FALSE
        )
    } else {
      from_check_df <- from_check_pre_df %>%
        dplyr::mutate(
          label = as.character(NA),
          description = as.character(NA),
          named_after_from_wikidata = as.logical(NA)
        )
    }

    current_street_names_original_df <- current_street_names_df

    current_street_names_df <- current_street_names_original_df %>%
      dplyr::anti_join(
        y = from_check_df,
        by = "name"
      )
  }

  if (check_named_after_original) {
    search_no_clean_df_pre <- tidywikidatar::tw_search(
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
      dplyr::slice(1:check_named_after_original_n) %>%
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

    search_no_clean_df <- search_no_clean_df_pre %>%
      dplyr::filter(is.na(.data$named_after) == FALSE) %>%
      dplyr::distinct(name, label, .keep_all = TRUE)

    named_after_original_df <- search_no_clean_df %>%
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


  search_results_df <- tidywikidatar::tw_search(
    search = current_street_names_df %>%
      dplyr::filter(!.data$name %in% exclude_v) %>%
      dplyr::pull("name_clean"),
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
    named_after_df <- tw_get_property(
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
        "Q54114", # boulevard
        "Q24354" # theater
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

    if (check_named_after_original == TRUE) {
      output_df <- dplyr::bind_rows(
        output_df,
        processed_df
      ) %>%
        dplyr::distinct(.data$name, .keep_all = TRUE)
    } else {
      output_df <- processed_df
    }
  }

  if (check_named_after_original | check_named_after) {
    output_df <- dplyr::bind_rows(
      output_df %>%
        dplyr::mutate(named_after_from_wikidata = TRUE),
      current_street_names_df %>%
        dplyr::filter(!.data$name %in% exclude_v) %>%
        dplyr::left_join(
          y = search_results_df %>%
            dplyr::mutate(named_after_from_wikidata = FALSE),
          by = "name_clean"
        ) %>%
        dplyr::distinct(.data$name,
          .keep_all = TRUE
        )
    )
  } else {
    output_df <- current_street_names_df %>%
      dplyr::filter(!.data$name %in% exclude_v) %>%
      dplyr::left_join(
        y = search_results_df,
        by = "name_clean"
      )
  }

  if (drop_if_street == TRUE) {
    pre_process_join_df <- output_df %>%
      dplyr::select(-.data$name_clean) %>%
      dplyr::mutate(instance_of = tidywikidatar::tw_get_p1(
        id = id, p = "P31",
        language = response_language,
        cache = cache,
        disconnect_db = FALSE,
        overwrite_cache = overwrite_cache,
        cache_connection = db_connection
      )) %>%
      dplyr::filter(!.data$instance_of %in% c(
        "Q79007", # street
        "Q174782", # square
        "Q12280", # bridge
        "Q3352369", # footpath
        "Q34442", # road
        "Q54114" # boulevard
      )) 
    
    if (drop_if_disambiguation_page == TRUE) {
      pre_process_join_df <- pre_process_join_df %>% 
        dplyr::filter(!.data$instance_of %in% c(
          "Q4167410")) # disambiguation page
    }
    
    final_output_df <- current_street_names_df %>%
      dplyr::left_join(
        y = pre_process_join_df %>%
          dplyr::select(-.data$instance_of),
        by = "name"
      )
  } else {
    final_output_df <- current_street_names_df %>%
      dplyr::left_join(
        y = output_df %>%
          dplyr::select(-.data$name_clean),
        by = "name"
      )
  }

  df <- final_output_df %>% 
    dplyr::transmute(street_name = .data$name, 
                     wikidata_id = .data$id, 
                     .data$label,
                     .data$description, 
                     .data$named_after_from_wikidata)
  
  if (pool::dbExistsTable(conn = db_connection, name = table_name) == FALSE) {
    # if table does not exist...
      DBI::dbWriteTable(db_connection,
                        name = table_name,
                        value = df,
                        append = TRUE
      )
  } else {
    # if table exists...
    if (append == TRUE) {
      DBI::dbWriteTable(db_connection,
                        name = table_name,
                        value = df,
                        append = TRUE
      )
    } else {
      previously_available <- dplyr::tbl(
        src = db_connection,
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
        if (overwrite_cache == TRUE | append == TRUE) {
          DBI::dbWriteTable(db_connection,
                            name = table_name,
                            value = df,
                            append = TRUE
          )
        }
      } else {
        if (overwrite_cache == TRUE) {
          statement <- glue::glue_sql("DELETE FROM {`table_name`} WHERE gisco_id = {gisco_id*} AND street_name = {street_name*}",
                                      gisco_id = unique(df$gisco_id),
                                      table_name = table_name,
                                      street_name = street_name,
                                      .con = db_connection
          )
          result <- DBI::dbExecute(
            conn = db_connection,
            statement = statement
          )
          DBI::dbWriteTable(db_connection,
                            name = table_name,
                            value = df,
                            append = TRUE
          )
        } else {
          # do nothing if data already present and both overwrite_cache and append are set to FALSE
        }
      }
    }
  }
  
  
  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db_connection,
    disconnect_db = disconnect_db,
    language = language
  )
  
  final_output_df <- dplyr::bind_rows(previously_cached_df, 
                                           final_output_df %>% 
                                             dplyr::transmute(street_name = .data$name, 
                                                              wikidata_id = .data$id, 
                                                              .data$label,
                                                              .data$description, 
                                                              .data$named_after_from_wikidata))

  if (is.null(checked_df) == FALSE) {
    if (nrow(checked_df) > 0) {
      final_output_df <- current_street_names_original_df %>%
        dplyr::select(-.data$name_clean) %>% 
        dplyr::left_join(
          y = dplyr::bind_rows(
            from_check_df,
            final_output_df %>%
              dplyr::select(-.data$name_clean)
          ),
          by = "name"
        )
    }
  }
  final_output_df %>% 
    dplyr::transmute(street_name = .data$name, 
                     wikidata_id = .data$id, 
                     .data$label,
                     .data$description, 
                     .data$named_after_from_wikidata) %>% 
    dplyr::arrange(street_name) 
}
