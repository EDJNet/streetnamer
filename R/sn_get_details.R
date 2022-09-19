#' Get a pre-defined set of details about
#'
#' @param country A country, either a two letter country code, or country full name. See `sn_country_codes` for valid values.
#' @param collapse_lists Defaults to FALSE. If TRUE, collapses multiple values in a single string, with each value separated by ";". Useful for exports in tabular format.
#' @param language
#' @param connection
#' @param cache Logical, defaults to TRUE.
#' @param streets_sf Defaults to NULL. If given, used to speed up processing.
#'   Must be an sf object such as the ones output by `ll_osm_get_roads()`.
#'   Passed to `ll_osm_get_lau_streets()`.
#' @param manual_check_columns Logical, defaults to FALSE. If set to TRUE, it
#'   adds additional columns that can be used to introduce manually adjustments
#'   in a spreadsheet.
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @inheritParams sn_get_details_by_lau
#'
#' @return
#' @export
#'
#' @examples
sn_get_details_by_country <- function(country,
                                      additional_properties = c(
                                        "P39",
                                        "P509",
                                        "P140",
                                        "P611",
                                        "P411",
                                        "P241",
                                        "P410",
                                        "P97",
                                        "P607",
                                        "P27",
                                        "P172"
                                      ),
                                      export_format = NULL,
                                      collapse_lists = FALSE,
                                      language = tidywikidatar::tw_get_language(),
                                      search_language = NULL,
                                      response_language = tidywikidatar::tw_get_language(),
                                      check_named_after_original = TRUE,
                                      check_named_after_original_n = 1,
                                      check_named_after = TRUE,
                                      checked_df = NULL,
                                      manual_check_columns = FALSE,
                                      connection = NULL,
                                      connection_search = NULL,
                                      base_folder = "sn_data",
                                      streets_sf = NULL,
                                      cache = TRUE,
                                      overwrite_cache = FALSE,
                                      disconnect_db = TRUE) {
  country_name <- sn_standard_country(
    country = country,
    type = "name"
  )

  country_code <- sn_standard_country(
    country = country,
    type = "code"
  )


  current_country_lau_v <- latlon2map::ll_get_lau_eu() %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(CNTR_CODE == country_code) %>%
    dplyr::arrange(desc(POP_2020)) %>%
    dplyr::pull(GISCO_ID)


  connection_db <- tidywikidatar::tw_connect_to_cache(
    connection = connection,
    language = response_language,
    cache = cache
  )

  if (is.null(search_language)) {
    search_language <- streetnamer::sn_language_defaults_by_country %>%
      dplyr::filter(country == country_name) %>%
      dplyr::pull(language_code)
  }


  language_combo <- stringr::str_c(search_language, "_", response_language)




  connection_search_db <- tidywikidatar::tw_connect_to_cache(
    connection = connection_search,
    language = language_combo,
    cache = cache
  )


  purrr::walk(
    .x = current_country_lau_v,
    .f = function(x) {
      sn_get_details_by_lau(
        gisco_id = x,
        additional_properties = additional_properties,
        export_format = export_format,
        collapse_lists = collapse_lists,
        language = language,
        search_language = search_language,
        response_language = response_language,
        check_named_after_original = check_named_after_original,
        check_named_after = check_named_after,
        checked_df = checked_df,
        manual_check_columns = manual_check_columns,
        connection = connection_db,
        connection_search = connection_search_db,
        base_folder = base_folder,
        streets_sf = streets_sf,
        cache = cache,
        overwrite_cache = overwrite_cache,
        disconnect_db = FALSE
      )
    }
  )
  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = connection_db,
    disconnect_db = disconnect_db,
    language = language
  )

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = connection_search_db,
    disconnect_db = disconnect_db,
    language = language
  )
}



#' Gets details for a single city
#'
#' @param export_format Defaults to NULL. If given, it exports outputs in the given format. Available values include "csv" and "geojson".
#' @param additional_properties Defaults to a vector of Wikidata properties, with common properties of interest for humans. Some basic properties related to birth, death, and occupation are included by default.
#' @inheritParams sn_search_named_after
#'
#' @return
#' @export
#'
#' @examples
#'
#' gisco_id <- "DE_11000000"
sn_get_details_by_lau <- function(gisco_id,
                                  additional_properties = c(
                                    "P39",
                                    "P509",
                                    "P140",
                                    "P611",
                                    "P411",
                                    "P241",
                                    "P410",
                                    "P97",
                                    "P607",
                                    "P27",
                                    "P172"
                                  ),
                                  export_format = NULL,
                                  collapse_lists = FALSE,
                                  language = tidywikidatar::tw_get_language(),
                                  search_language = NULL,
                                  response_language = tidywikidatar::tw_get_language(),
                                  check_named_after_original = TRUE,
                                  check_named_after = TRUE,
                                  checked_df = NULL,
                                  manual_check_columns = FALSE,
                                  connection = NULL,
                                  connection_search = NULL,
                                  base_folder = "sn_data",
                                  streets_sf = NULL,
                                  street_names_df = NULL,
                                  cache = TRUE,
                                  overwrite_cache = FALSE,
                                  disconnect_db = TRUE) {
  country_name <- sn_standard_country(
    country = stringr::str_extract(string = gisco_id, pattern = "[A-Z][A-Z]") %>%
      stringr::str_to_upper(),
    type = "name"
  )


  connection_db <- tidywikidatar::tw_connect_to_cache(
    connection = connection,
    language = response_language,
    cache = cache
  )

  if (is.null(search_language)) {
    search_language <- streetnamer::sn_language_defaults_by_country %>%
      dplyr::filter(country == country_name) %>%
      dplyr::pull(language_code)
  }

  language_combo <- stringr::str_c(search_language, "_", response_language)

  connection_search_db <- tidywikidatar::tw_connect_to_cache(
    connection = connection_search,
    language = language_combo,
    cache = cache
  )


  if (is.null(export_format) == FALSE) {
    country_path <- fs::dir_create(path = fs::path(
      base_folder,
      "city_details",
      country_name
    ))

    fs::dir_create(country_path)
    fs::dir_create(fs::path(country_path, export_format, "all"))
    fs::dir_create(fs::path(country_path, export_format, "humans"))
    fs::dir_create(fs::path(country_path, export_format, "not_humans"))
  }

  all_df <- purrr::map_dfr(
    .x = gisco_id,
    .f = function(x) {
      city_name <- streetnamer::sn_lau_by_country %>%
        dplyr::filter(GISCO_ID == x) %>%
        dplyr::pull(LAU_NAME)

      if (is.null(export_format) == FALSE) {
        current_file <- fs::path(
          country_path,
          export_format,
          "all",
          stringr::str_c(x, "-", city_name, ".", export_format) %>%
            fs::path_sanitize()
        )

        current_file_humans <- fs::path(
          country_path,
          export_format,
          "humans",
          stringr::str_c(
            x, "-",
            city_name,
            "-humans.",
            export_format
          ) %>%
            fs::path_sanitize()
        )

        current_file_not_humans <- fs::path(
          country_path,
          export_format,
          "not_humans",
          stringr::str_c(
            x, "-",
            city_name,
            "-not_humans.",
            export_format
          ) %>%
            fs::path_sanitize()
        )
      }
      if (is.null(export_format) == FALSE) {
        if (fs::file_exists(current_file) == TRUE) {
          if (export_format == "csv") {
            output <- readr::read_csv(
              file = current_file,
              col_types = list(.default = "c")
            )
          } else {
            output <- sf::st_read(dsn = current_file)
          }
          return(output)
        }
      }

      search_df <- sn_search_named_after(
        gisco_id = x,
        search_language = search_language,
        response_language = response_language,
        check_named_after_original = check_named_after_original,
        check_named_after = check_named_after,
        streets_sf = streets_sf,
        street_names_df = street_names_df,
        checked_df = checked_df,
        connection = connection_search_db,
        cache = cache,
        overwrite_cache = overwrite_cache,
        disconnect_db = FALSE
      )

      city_df <- tidywikidatar::tw_get_p_wide(
        id = search_df,
        p = c(
          "P31",
          "P21",
          "P106",
          "P569",
          "P19",
          "P570",
          "P20",
          additional_properties
        ),
        label = TRUE,
        property_label_as_column_name = TRUE,
        both_id_and_label = TRUE,
        only_first = FALSE,
        unlist = FALSE,
        cache = cache,
        language = language,
        overwrite_cache = overwrite_cache,
        cache_connection = connection_db,
        disconnect_db = FALSE
      ) %>%
        dplyr::select(
          -.data$id,
          -.data$label
        )

      processed_df <- dplyr::bind_cols(search_df, city_df) %>%
        dplyr::mutate(
          picture = tidywikidatar::tw_get_image_same_length(
            id = id, format = "embed",
            width = 300,
            cache = cache,
            language = language,
            overwrite_cache = overwrite_cache,
            cache_connection = connection_db,
            disconnect_db = FALSE
          ),
          wikipedia = tidywikidatar::tw_get_wikipedia(
            id = id,
            cache = cache,
            language = language,
            overwrite_cache = overwrite_cache,
            cache_connection = connection_db,
            disconnect_db = FALSE
          )
        ) %>%
        dplyr::mutate(
          place_of_birth_single = purrr::map_chr(
            .x = place_of_birth,
            .f = function(x) {
              x[[1]]
            }
          ),
          place_of_death_single = purrr::map_chr(
            .x = place_of_death,
            .f = function(x) {
              x[[1]]
            }
          )
        ) %>%
        dplyr::mutate(
          place_of_birth_coordinates = tw_get_p(place_of_birth_single,
            p = "P625",
            only_first = TRUE,
            preferred = TRUE,
            cache = cache,
            language = language,
            overwrite_cache = overwrite_cache,
            cache_connection = connection_db,
            disconnect_db = FALSE
          ),
          place_of_death_coordinates = tw_get_p(place_of_death_single,
            p = "P625",
            only_first = TRUE,
            preferred = TRUE,
            cache = cache,
            language = language,
            overwrite_cache = overwrite_cache,
            cache_connection = connection_db,
            disconnect_db = FALSE
          )
        ) %>%
        tidyr::separate(
          col = place_of_birth_coordinates,
          into = c(
            "place_of_birth_latitude",
            "place_of_birth_longitude"
          ),
          sep = ",",
          remove = TRUE,
          convert = TRUE
        ) %>%
        tidyr::separate(
          col = place_of_death_coordinates,
          into = c(
            "place_of_death_latitude",
            "place_of_death_longitude"
          ),
          sep = ",",
          remove = TRUE,
          convert = TRUE
        )


      output_df <- processed_df %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(
          dplyr::across(
            where(is.list),
            function(x) {
              stringr::str_c(unique(unlist(x)),
                collapse = "; "
              )
            }
          )
        ) %>%
        dplyr::ungroup()

      if (is.null(checked_df) == FALSE) {
        if ("gender" %in% colnames(checked_df)) {
          na_gender_df <- output_df %>%
            dplyr::filter(is.na(.data$sex_or_gender)) %>%
            dplyr::select(.data$name)

          checked_gender_v <- na_gender_df %>%
            dplyr::left_join(
              y = checked_df %>%
                dplyr::distinct(
                  .data$name,
                  .data$gender
                ),
              by = "name"
            ) %>%
            dplyr::pull(.data$gender)

          logical_filter_v <- is.na(output_df$sex_or_gender_label)

          output_df$sex_or_gender_label[logical_filter_v] <- as.character(checked_gender_v)

          sex_or_gender_id_v <- as.character(stringr::str_replace_all(
            string = checked_gender_v,
            pattern = c(
              male = "Q6581097",
              female = "Q6581072"
            )
          ))

          sex_or_gender_id_v[!tidywikidatar::tw_check_qid(id = sex_or_gender_id_v, logical_vector = TRUE)] <- as.character(NA)

          output_df$sex_or_gender[logical_filter_v] <- sex_or_gender_id_v

          output_df$instance_of[logical_filter_v][is.na(sex_or_gender_id_v) == FALSE] <- "Q5"
          output_df$instance_of_label[logical_filter_v][is.na(sex_or_gender_id_v) == FALSE] <- "human"
        }
      }


      if (manual_check_columns == TRUE) {
        output_df <- output_df %>%
          dplyr::mutate(
            gisco_id = x,
            tick_if_wrong = "",
            fixed_named_after_id = "",
            fixed_human = "",
            fixed_sex_or_gender = "",
            fixed_category = "",
            fixed_n_dedicated_to = "",
            named_after_custom_label = "",
            fixed_ignore = "",
          ) %>%
          dplyr::select(
            .data$gisco_id,
            .data$name,
            # .data$name_clean,
            .data$id,
            .data$label,
            .data$description,
            .data$tick_if_wrong,
            .data$fixed_human,
            .data$fixed_named_after_id,
            .data$fixed_sex_or_gender,
            .data$fixed_category,
            .data$fixed_n_dedicated_to,
            .data$named_after_custom_label,
            .data$fixed_ignore,
            dplyr::everything()
          )
      }

      if (is.null(export_format) == TRUE) {
        output_df
      } else if (export_format == "csv") {
        print(stringr::str_c(current_file))
        readr::write_csv(x = output_df, file = current_file)

        readr::write_csv(
          x = output_df %>%
            dplyr::filter(is.na(.data$instance_of) == FALSE) %>%
            dplyr::filter(.data$instance_of == "Q5" | stringr::str_detect(
              string = .data$instance_of,
              pattern = "Q5;"
            ) | stringr::str_detect(
              string = .data$instance_of,
              pattern = "Q5$"
            )),
          file = current_file_humans
        )

        readr::write_csv(
          x = output_df %>%
            dplyr::filter(is.na(.data$instance_of) | stringr::str_detect(
              string = .data$instance_of,
              pattern = "Q5;"
            ) == FALSE) %>%
            dplyr::filter(is.na(.data$instance_of) | .data$instance_of != "Q5"),
          file = current_file_not_humans
        )

        output_df
      } else if (export_format == "geojson") {
        current_sf <- latlon2map::ll_osm_get_lau_streets(
          gisco_id = gisco_id,
          unnamed_streets = FALSE,
          streets_sf = streets_sf
        ) %>%
          dplyr::left_join(y = output_df, by = "name")

        sf::st_write(
          obj = current_sf,
          dsn = current_file
        )

        sf::st_write(
          obj = current_sf %>%
            dplyr::filter(is.na(.data$instance_of) == FALSE) %>%
            dplyr::filter(.data$instance_of == "Q5" | stringr::str_detect(
              string = .data$instance_of,
              pattern = "Q5;"
            ) | stringr::str_detect(
              string = .data$instance_of,
              pattern = "Q5$"
            )),
          dsn = current_file_humans
        )

        sf::st_write(
          obj = current_sf %>%
            dplyr::filter(is.na(.data$instance_of) | stringr::str_detect(
              string = .data$instance_of,
              pattern = "Q5;"
            ) == FALSE) %>%
            dplyr::filter(is.na(.data$instance_of) | .data$instance_of != "Q5"),
          dsn = current_file_not_humans
        )
        current_sf
      }
    }
  )

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = connection_db,
    disconnect_db = disconnect_db,
    language = language
  )

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = connection_search_db,
    disconnect_db = disconnect_db,
    language = language
  )

  all_df
}

#' Get a pre-defined set of details about
#'
#' @param id Wikidata id
#' @param collapse_lists Defaults to FALSE. If TRUE, collapses multiple values in a single string, with each value separated by ";". Useful for exports in tabular format.
#' @param language
#' @param connection
#' @param cache Logical, defaults to TRUE.
#'
#' @return
#' @export
#'
#' @examples
sn_get_details_legacy <- function(id,
                                  collapse_lists = FALSE,
                                  language = tidywikidatar::tw_get_language(),
                                  connection = NULL,
                                  cache = TRUE) {
  db_connection <- tidywikidatar::tw_connect_to_cache(connection = connection)

  qid_tw_df <- tidywikidatar::tw_get(
    id = id,
    language = language,
    cache = cache,
    cache_connection = db_connection,
    disconnect_db = FALSE
  )

  df_l <- tibble::tibble(qid = id) %>%
    dplyr::mutate(
      label = tidywikidatar::tw_get_label(qid, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df),
      description = tidywikidatar::tw_get_description(qid, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)
    ) %>%
    dplyr::mutate(instance_of_qid = tidywikidatar::tw_get_p(id = qid, p = "P31", language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(instance_of_label = purrr::map(
      .x = instance_of_qid,
      function(x) {
        x %>%
          unlist() %>%
          tidywikidatar::tw_get_label(language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)
      }
    )) %>%
    dplyr::mutate(sex_or_gender_qid = tidywikidatar::tw_get_p(id = qid, p = "P21", language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(sex_or_gender_label = purrr::map(
      .x = sex_or_gender_qid,
      function(x) {
        x %>%
          unlist() %>%
          tidywikidatar::tw_get_label(language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)
      }
    )) %>%
    dplyr::mutate(occupation_qid = tidywikidatar::tw_get_p(id = qid, p = "P106", language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(occupation_label = purrr::map(
      .x = occupation_qid,
      function(x) {
        x %>%
          unlist() %>%
          tidywikidatar::tw_get_label(language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)
      }
    )) %>%
    dplyr::mutate(date_of_birth = tidywikidatar::tw_get_p(qid, p = "P569", only_first = TRUE, preferred = TRUE, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(year_of_birth = stringr::str_extract(
      string = date_of_birth,
      pattern = "[[:print:]][[:digit:]]{4}"
    ) %>%
      as.numeric()) %>%
    dplyr::mutate(place_of_birth_qid = tidywikidatar::tw_get_p(qid, p = "P19", only_first = TRUE, preferred = TRUE, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(
      place_of_birth_label = tidywikidatar::tw_get_label(place_of_birth_qid, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache),
      place_of_birth_country_qid = tidywikidatar::tw_get_p(place_of_birth_qid, p = "P17", only_first = TRUE, preferred = TRUE, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)
    ) %>%
    dplyr::mutate(
      place_of_birth_country_label = tidywikidatar::tw_get_label(place_of_birth_country_qid, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache),
      place_of_birth_coordinates = tidywikidatar::tw_get_p(id = place_of_birth_qid, p = "P625", only_first = TRUE, preferred = TRUE, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)
    ) %>%
    tidyr::separate(
      col = place_of_birth_coordinates,
      into = c("place_of_birth_latitude", "place_of_birth_longitude"),
      sep = ",",
      remove = TRUE,
      convert = TRUE
    ) %>%
    dplyr::mutate(date_of_death = tidywikidatar::tw_get_p(qid, p = "P570", only_first = TRUE, preferred = TRUE, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(year_of_death = stringr::str_extract(
      string = date_of_death,
      pattern = "[[:print:]][[:digit:]]{4}"
    ) %>%
      as.numeric()) %>%
    dplyr::mutate(place_of_death_qid = tidywikidatar::tw_get_p(qid, p = "P20", only_first = TRUE, preferred = TRUE, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(
      place_of_death_label = tidywikidatar::tw_get_label(place_of_death_qid, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache),
      place_of_death_country_qid = tidywikidatar::tw_get_p(place_of_death_qid, p = "P17", only_first = TRUE, preferred = TRUE, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)
    ) %>%
    dplyr::mutate(
      place_of_death_country_label = tidywikidatar::tw_get_label(place_of_death_country_qid, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache),
      place_of_death_coordinates = tidywikidatar::tw_get_p(id = place_of_death_qid, p = "P625", only_first = TRUE, preferred = TRUE, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)
    ) %>%
    tidyr::separate(
      col = place_of_death_coordinates,
      into = c("place_of_death_latitude", "place_of_death_longitude"),
      sep = ",",
      remove = TRUE,
      convert = TRUE
    ) %>%
    dplyr::mutate(cause_of_death_qid = tidywikidatar::tw_get_p(qid, p = "P509", only_first = TRUE, preferred = TRUE, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(cause_of_death_label = tidywikidatar::tw_get_label(cause_of_death_qid, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)) %>%
    dplyr::mutate(manner_of_death_qid = tidywikidatar::tw_get_p(qid, p = "P1196", only_first = TRUE, preferred = TRUE, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(manner_of_death_label = tidywikidatar::tw_get_label(manner_of_death_qid, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)) %>%
    dplyr::mutate(position_held_qid = tidywikidatar::tw_get_p(id = qid, p = "P39", language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(position_held_label = purrr::map(
      .x = position_held_qid,
      function(x) {
        x %>%
          unlist() %>%
          tidywikidatar::tw_get_label(language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)
      }
    )) %>%
    dplyr::mutate(religious_order_qid = tidywikidatar::tw_get_p(qid, p = "P611", only_first = TRUE, preferred = TRUE, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(religious_order_label = tidywikidatar::tw_get_label(religious_order_qid, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)) %>%
    dplyr::mutate(religion_qid = tidywikidatar::tw_get_p(qid, p = "P140", only_first = TRUE, preferred = TRUE, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(religion_label = tidywikidatar::tw_get_label(religion_qid, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)) %>%
    dplyr::mutate(canonization_status_qid = tidywikidatar::tw_get_p(qid, p = "P411", only_first = TRUE, preferred = TRUE, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(canonization_status_label = tidywikidatar::tw_get_label(canonization_status_qid, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)) %>%
    dplyr::mutate(military_branch_qid = tidywikidatar::tw_get_p(id = qid, p = "P241", language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(military_branch_label = purrr::map(
      .x = military_branch_qid,
      function(x) {
        x %>%
          unlist() %>%
          tidywikidatar::tw_get_label(language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)
      }
    )) %>%
    dplyr::mutate(military_rank_qid = tidywikidatar::tw_get_p(id = qid, p = "P410", language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(military_rank_label = purrr::map(
      .x = military_rank_qid,
      function(x) {
        x %>%
          unlist() %>%
          tidywikidatar::tw_get_label(language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)
      }
    )) %>%
    dplyr::mutate(noble_title_qid = tidywikidatar::tw_get_p(id = qid, p = "P97", language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(noble_title_label = purrr::map(
      .x = noble_title_qid,
      function(x) {
        x %>%
          unlist() %>%
          tidywikidatar::tw_get_label(language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)
      }
    )) %>%
    dplyr::mutate(family_qid = tidywikidatar::tw_get_p(id = qid, p = "P97", language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df, only_first = TRUE, preferred = TRUE)) %>%
    dplyr::mutate(family_label = tidywikidatar::tw_get_label(family_qid, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)) %>%
    dplyr::mutate(family_instance_of_qid = tidywikidatar::tw_get_p(id = family_qid, p = "P31", language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)) %>%
    dplyr::mutate(family_instance_of_label = purrr::map(
      .x = family_instance_of_qid,
      function(x) {
        x %>%
          unlist() %>%
          tidywikidatar::tw_get_label(language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)
      }
    )) %>%
    dplyr::mutate(conflict_qid = tidywikidatar::tw_get_p(id = qid, p = "P607", language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(conflict_label = purrr::map(
      .x = conflict_qid,
      function(x) {
        x %>%
          unlist() %>%
          tidywikidatar::tw_get_label(language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)
      }
    )) %>%
    dplyr::mutate(participant_in_qid = tidywikidatar::tw_get_p(id = qid, p = "P607", language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(participant_in_label = purrr::map(
      .x = participant_in_qid,
      function(x) {
        x %>%
          unlist() %>%
          tidywikidatar::tw_get_label(language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache)
      }
    )) %>%
    dplyr::mutate(
      picture = tidywikidatar::tw_get_image_same_length(id = qid, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df, only_first = TRUE),
      wikipedia = tidywikidatar::tw_get_wikipedia(id = qid, language = language, cache_connection = db_connection, disconnect_db = FALSE, cache = cache, id_df = qid_tw_df)
    )

  tidywikidatar::tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db_connection,
    disconnect_db = disconnect_db,
    language = language
  )

  if (collapse_lists) {
    df_l %>%
      dplyr::group_by(qid) %>%
      dplyr::mutate(dplyr::across(
        where(is.list),
        function(x) stringr::str_c(unique(unlist(x)), collapse = ";")
      )) %>%
      dplyr::ungroup()
  } else {
    df_l
  }
}
