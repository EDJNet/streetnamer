#' Exports checked file for further analysis
#'
#' @param gisco_id Defaults to NULL. If given, a valid code starting with a two letter country code. Either `gisco_id` or `country` must be given.
#' @param country Defaults to NULL. If given, must be a two-letter code. Returns data only for the given country.
#' @param source Source for data. Valid values include "database" and "fixed_csv".
#' @param fixed_folder Base location of folder where csv files with manual fixes are stored. They can be located in subfolders.
#' @param unlist Defaults to FALSE. If TRUE, all data are unlisted in place, with values separated by a `;`.
#' @param export_folder Defaults to `sn_data_export`, created if not existing, overwrite files by default. If set to TRUE, unlist is also automatically set to TRUE.
#' @param export_format Character vector, defaults to NULL. If given, it exports outputs in the given format. Available values include "csv" and "geojson".
#'
#' @return
#' @export
#'
#' @examples
sn_export_checked_legacy <- function(gisco_id = NULL,
                                     country = NULL,
                                     source = "fixed_csv",
                                     additional_properties = c("P39", "P509", "P140", "P611", "P411", "P241", "P410", "P97", "P607", "P27", "P172"),
                                     include_image_credits = TRUE,
                                     unlist = FALSE,
                                     fixed_folder = "sn_data_fixed",
                                     export_folder = "sn_data_export",
                                     export_format = NULL,
                                     cache = NULL,
                                     language = tidywikidatar::tw_get_language(),
                                     overwrite_cache = FALSE,
                                     cache_connection = NULL,
                                     disconnect_db = TRUE) {
  if (is.null(export_format) == FALSE) {
    if (export_format == "csv") {
      unlist <- TRUE
    }
  } else {
    export_format <- "csv"
    unlist <- TRUE
  }

  connection_db <- tidywikidatar::tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  if (source == "fixed_csv") {
    local_files <- fs::dir_ls(
      path = fs::path(fixed_folder),
      recurse = TRUE,
      type = "file",
      glob = "*.csv"
    )

    if (is.null(gisco_id) == FALSE) {
      files_to_keep <- local_files[stringr::str_starts(
        string = fs::path_file(path = local_files),
        pattern = stringr::str_c(gisco_id, "-")
      )]

      country_name <- sn_standard_country(
        country = stringr::str_extract(string = gisco_id, pattern = "[A-Z][A-Z]") %>%
          stringr::str_to_upper(),
        type = "name"
      )
    } else if (is.null(country) == FALSE) {
      files_to_keep <- local_files[stringr::str_starts(
        string = fs::path_file(path = local_files),
        pattern = stringr::str_c(stringr::str_to_upper(country), "_")
      )]

      country_name <- sn_standard_country(
        country = country %>%
          stringr::str_to_upper(),
        type = "name"
      )
    }
    current_confirmed_df <- purrr::map_dfr(
      .x = files_to_keep,
      .f = function(x) {
        sn_import_from_manually_fixed(
          input_df = x,
          return_df_only = TRUE
        )
      }
    ) %>%
      dplyr::filter(as.logical(checked)) %>%
      dplyr::mutate(
        label = tidywikidatar::tw_get_label(
          id = named_after_id,
          cache = cache,
          language = language,
          overwrite_cache = overwrite_cache,
          cache_connection = connection_db,
          disconnect_db = FALSE
        ),
        description = tidywikidatar::tw_get_description(
          id = named_after_id,
          cache = cache,
          language = language,
          overwrite_cache = overwrite_cache,
          cache_connection = connection_db,
          disconnect_db = FALSE
        )
      )


    city_df <- current_confirmed_df %>%
      dplyr::pull(named_after_id) %>%
      tidywikidatar::tw_get_p_wide(
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

    processed_df <- dplyr::bind_cols(current_confirmed_df, city_df) %>%
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
      ) %>%
      dplyr::mutate(row_number = dplyr::row_number()) %>%
      dplyr::mutate(
        wikipedia = tidywikidatar::tw_get_wikipedia(
          id = named_after_id,
          cache = cache,
          language = language,
          overwrite_cache = overwrite_cache,
          cache_connection = connection_db,
          disconnect_db = FALSE
        ),
        picture_embed = tidywikidatar::tw_get_image_same_length(
          id = named_after_id,
          format = "embed",
          only_first = TRUE,
          width = 300,
          cache = cache,
          language = language,
          overwrite_cache = overwrite_cache,
          cache_connection = connection_db,
          disconnect_db = FALSE
        ),
        picture_commons = tidywikidatar::tw_get_image_same_length(
          id = named_after_id,
          format = "commons",
          only_first = TRUE,
          width = 300,
          cache = cache,
          language = language,
          overwrite_cache = overwrite_cache,
          cache_connection = connection_db,
          disconnect_db = FALSE
        )
      )

    if (include_image_credits == TRUE) {
      img_metadata_df <- processed_df %>%
        dplyr::distinct(.data$named_after_id) %>%
        dplyr::pull(.data$named_after_id) %>%
        tw_get_image_metadata(
          only_first = TRUE,
          language = language,
          overwrite_cache = overwrite_cache,
          cache_connection = connection_db,
          disconnect_db = FALSE
        )

      processed_df <- processed_df %>%
        dplyr::left_join(
          y = img_metadata_df %>%
            dplyr::transmute(
              named_after_id = .data$id,
              imgage_attribution_required = .data$attribution_required,
              image_copyrighted = .data$copyrighted,
              image_restrictions = .data$restrictions,
              image_credit = .data$credit,
              image_artist = .data$artist,
              image_license_short_name = .data$license_short_name,
              image_license_url = .data$license_url,
              image_usage_terms = .data$usage_terms
            ),
          by = "named_after_id"
        )
    }

    if (unlist == TRUE) {
      output_df <- processed_df %>%
        dplyr::group_by(row_number) %>%
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
        dplyr::ungroup() %>%
        dplyr::mutate(
          gender_label_combo = dplyr::case_when(
            is.na(sex_or_gender_label) == FALSE & sex_or_gender_label != "female" & sex_or_gender_label != "male" ~ as.character("other"),
            is.na(sex_or_gender_label) == FALSE ~ sex_or_gender_label,
            is.na(sex_or_gender_label) == TRUE ~ gender,
            TRUE ~ gender
          )
        )
    } else {
      output_df <- processed_df %>%
        dplyr::group_by(row_number) %>%
        dplyr::mutate(
          unlisted_gender = stringr::str_c(unique(unlist(sex_or_gender_label)),
            collapse = "; "
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          gender_label_combo = dplyr::case_when(
            is.na(unlisted_gender) == FALSE & unlisted_gender != "female" & unlisted_gender != "male" ~ as.character("other"),
            is.na(unlisted_gender) == FALSE ~ unlisted_gender,
            is.na(unlisted_gender) == TRUE ~ gender,
            TRUE ~ gender
          )
        ) %>%
        dplyr::select(-.data$unlisted_gender)
    }
  }

  output_df <- output_df %>%
    dplyr::rename(named_after_id = .data$named_after_id)

  tidywikidatar::tw_disconnect_from_cache(
    cache = cache,
    cache_connection = connection_db,
    disconnect_db = disconnect_db,
    language = language
  )


  if (is.null(export_format) == FALSE) {
    city_name <- streetnamer::sn_lau_by_country %>%
      dplyr::filter(GISCO_ID == gisco_id) %>%
      dplyr::pull(LAU_NAME)

    fs::dir_create(path = fs::path(export_folder, country_name, export_format), recurse = TRUE)
    if (export_format == "csv") {
      readr::write_csv(
        x = output_df,
        file = fs::path(
          export_folder,
          country_name,
          export_format,
          stringr::str_c(
            stringr::str_c(
              gisco_id,
              "-",
              city_name,
              "."
            ) %>%
              fs::path_sanitize(),
            ".",
            export_format
          )
        )
      )
    } else if (export_format == "geojson") {
      if (is.null(gisco_id)) {
        usethis::ui_stop("For export in the `geojson` format, `gisco_id` must be given.")
      }

      export_sf <- latlon2map::ll_osm_get_lau_streets(
        gisco_id = gisco_id,
        unnamed_streets = FALSE
      ) %>%
        dplyr::rename(street_name = .data$name) %>%
        dplyr::left_join(
          y = output_df,
          by = "street_name"
        )

      sf::st_write(
        obj = export_sf,
        dsn = fs::path(
          export_folder,
          country_name,
          export_format,
          stringr::str_c(
            stringr::str_c(
              gisco_id,
              "-",
              city_name
            ) %>%
              fs::path_sanitize(),
            ".",
            export_format
          )
        )
      )
    }
  }

  output_df
}


#' Exports data for a country or a municipality
#'
#' @param export_format Defaults to NULL. Valid values include "csv", "geojson", "rds", "rds_sf"
#'
#' @inheritParams sn_export_checked_legacy
#'
#' @return
#' @export
#'
#' @examples
sn_export <- function(gisco_id = NULL,
                      country = NULL,
                      street_name = NULL,
                      keep_only_latest = TRUE,
                      only_checked = TRUE,
                      remove_ignored = TRUE,
                      additional_properties = c("P39", "P509", "P140", "P611", "P411", "P241", "P410", "P97", "P607", "P27", "P172"),
                      include_image_credits = TRUE,
                      unlist = FALSE,
                      export_folder = "sn_data_export",
                      export_format = NULL,
                      write_file = FALSE,
                      cache = TRUE,
                      language = tidywikidatar::tw_get_language(),
                      overwrite_cache = FALSE,
                      connection = NULL,
                      disconnect_db = FALSE) {
  if (is.null(export_format) == FALSE) {
    if (export_format == "csv") {
      unlist <- TRUE
    }
  }

  connection_db <- tidywikidatar::tw_connect_to_cache(
    connection = connection,
    language = language,
    cache = cache
  )

  stored_street_names_df <- sn_get_street_named_after_id(
    country = country,
    gisco_id = gisco_id,
    street_name = street_name,
    keep_only_latest = keep_only_latest,
    only_checked = only_checked,
    remove_ignored = remove_ignored,
    language = language,
    connection = connection_db,
    disconnect_db = FALSE
  ) %>%
    dplyr::select(-.data$session, -.data$time)

  current_confirmed_df <- stored_street_names_df %>%
    dplyr::mutate(
      label = tidywikidatar::tw_get_label(
        id = named_after_id,
        cache = cache,
        language = language,
        overwrite_cache = overwrite_cache,
        cache_connection = connection_db,
        disconnect_db = FALSE
      ),
      description = tidywikidatar::tw_get_description(
        id = named_after_id,
        cache = cache,
        language = language,
        overwrite_cache = overwrite_cache,
        cache_connection = connection_db,
        disconnect_db = FALSE
      )
    )


  city_df <- current_confirmed_df %>%
    dplyr::pull(named_after_id) %>%
    tidywikidatar::tw_get_p_wide(
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

  processed_df <- dplyr::bind_cols(
    current_confirmed_df,
    city_df
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
      place_of_birth_coordinates = tidywikidatar::tw_get_p(place_of_birth_single,
        p = "P625",
        only_first = TRUE,
        preferred = TRUE,
        cache = cache,
        language = language,
        overwrite_cache = overwrite_cache,
        cache_connection = connection_db,
        disconnect_db = FALSE
      ),
      place_of_death_coordinates = tidywikidatar::tw_get_p(place_of_death_single,
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
    ) %>%
    dplyr::mutate(row_number = dplyr::row_number()) %>%
    dplyr::mutate(
      wikipedia = tidywikidatar::tw_get_wikipedia(
        id = named_after_id,
        cache = cache,
        language = language,
        overwrite_cache = overwrite_cache,
        cache_connection = connection_db,
        disconnect_db = FALSE
      ),
      picture_embed = tidywikidatar::tw_get_image_same_length(
        id = named_after_id,
        format = "embed",
        only_first = TRUE,
        width = 300,
        cache = cache,
        language = language,
        overwrite_cache = overwrite_cache,
        cache_connection = connection_db,
        disconnect_db = FALSE
      ),
      picture_commons = tidywikidatar::tw_get_image_same_length(
        id = named_after_id,
        format = "commons",
        only_first = TRUE,
        width = 300,
        cache = cache,
        language = language,
        overwrite_cache = overwrite_cache,
        cache_connection = connection_db,
        disconnect_db = FALSE
      )
    )

  if (include_image_credits == TRUE) {
    img_metadata_df <- processed_df %>%
      dplyr::distinct(.data$named_after_id) %>%
      dplyr::pull(.data$named_after_id) %>%
      tidywikidatar::tw_get_image_metadata(
        only_first = TRUE,
        language = language,
        overwrite_cache = overwrite_cache,
        cache_connection = connection_db,
        disconnect_db = FALSE,
        cache = cache
      )

    processed_df <- processed_df %>%
      dplyr::left_join(
        y = img_metadata_df %>%
          dplyr::transmute(
            named_after_id = .data$id,
            imgage_attribution_required = .data$attribution_required,
            image_copyrighted = .data$copyrighted,
            image_restrictions = .data$restrictions,
            image_credit = .data$credit,
            image_artist = .data$artist,
            image_license_short_name = .data$license_short_name,
            image_license_url = .data$license_url,
            image_usage_terms = .data$usage_terms
          ),
        by = "named_after_id"
      )
  }

  if (unlist == TRUE) {
    output_df <- processed_df %>%
      dplyr::group_by(row_number) %>%
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
      dplyr::ungroup() %>%
      dplyr::mutate(
        gender_label_combo = dplyr::case_when(
          is.na(sex_or_gender_label) == FALSE & sex_or_gender_label != "female" & sex_or_gender_label != "male" ~ as.character("other"),
          is.na(sex_or_gender_label) == FALSE ~ sex_or_gender_label,
          is.na(sex_or_gender_label) == TRUE ~ gender,
          TRUE ~ gender
        )
      )
  } else {
    output_df <- processed_df %>%
      dplyr::group_by(row_number) %>%
      dplyr::mutate(
        unlisted_gender = stringr::str_c(unique(unlist(sex_or_gender_label)),
          collapse = "; "
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        gender_label_combo = dplyr::case_when(
          is.na(unlisted_gender) == FALSE & unlisted_gender != "female" & unlisted_gender != "male" ~ as.character("other"),
          is.na(unlisted_gender) == FALSE ~ unlisted_gender,
          is.na(unlisted_gender) == TRUE ~ gender,
          TRUE ~ gender
        )
      ) %>%
      dplyr::select(-.data$unlisted_gender)
  }


  tidywikidatar::tw_disconnect_from_cache(
    cache = cache,
    cache_connection = connection_db,
    disconnect_db = disconnect_db,
    language = language
  )

  if (is.null(export_format) == FALSE) {
    if (write_file == TRUE) {
      city_name <- streetnamer::sn_lau_by_country %>%
        dplyr::filter(GISCO_ID == gisco_id) %>%
        dplyr::pull(LAU_NAME)

      fs::dir_create(path = fs::path(export_folder, country_name, export_format), recurse = TRUE)
    }

    if (export_format == "csv") {
      if (write_file == TRUE) {
        readr::write_csv(
          x = output_df,
          file = fs::path(
            export_folder,
            country_name,
            export_format,
            stringr::str_c(
              stringr::str_c(
                gisco_id,
                "-",
                city_name,
                "."
              ) %>%
                fs::path_sanitize(),
              ".",
              export_format
            )
          )
        )
      } else {
        return(output_df)
      }
      return(output_df)
    } else if (export_format == "geojson") {
      if (is.null(gisco_id)) {
        usethis::ui_stop("For export in the `geojson` format, `gisco_id` must be given.")
      }

      export_sf <- latlon2map::ll_osm_get_lau_streets(
        gisco_id = gisco_id,
        unnamed_streets = FALSE
      ) %>%
        dplyr::rename(street_name = .data$name) %>%
        dplyr::left_join(
          y = output_df,
          by = "street_name"
        )

      if (write_file == TRUE) {
        sf::st_write(
          obj = export_sf,
          dsn = fs::path(
            export_folder,
            country_name,
            export_format,
            stringr::str_c(
              stringr::str_c(
                gisco_id,
                "-",
                city_name
              ) %>%
                fs::path_sanitize(),
              ".",
              export_format
            )
          )
        )
        return(export_sf)
      } else {
        return(export_sf)
      }
    } else if (export_format == "rds_sf") {
      export_sf <- latlon2map::ll_osm_get_lau_streets(
        gisco_id = gisco_id,
        unnamed_streets = FALSE
      ) %>%
        dplyr::rename(street_name = .data$name) %>%
        dplyr::left_join(
          y = output_df,
          by = "street_name"
        )
      if (write_file == TRUE) {
        saveRDS(
          object = export_sf,
          file = fs::path(
            export_folder,
            country_name,
            export_format,
            stringr::str_c(
              stringr::str_c(
                gisco_id,
                "-",
                city_name,
                "."
              ) %>%
                fs::path_sanitize(),
              ".",
              export_format
            )
          )
        )
      }
      return(export_sf)
    } else if (export_format == "rds") {
      if (write_file == TRUE) {
        saveRDS(
          object = output_df,
          file = fs::path(
            export_folder,
            country_name,
            export_format,
            stringr::str_c(
              stringr::str_c(
                gisco_id,
                "-",
                city_name,
                "."
              ) %>%
                fs::path_sanitize(),
              ".",
              export_format
            )
          )
        )
      }
      return(output_df)
    }
  }

  output_df
}
