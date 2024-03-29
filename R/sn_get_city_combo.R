#' Get a data frame with all streets in a city with matching, including both automatic and manually checked inputs
#'
#' @param include_checked_elsewhere_in_country Defaults to FALSE. If TRUE, checks if streets with the same name have been checked at country level. Requires `streets_sf`.
#' @inheritParams sn_write_street_named_after_id
#'
#' @return
#' @export
#'
#' @examples
sn_get_city_combo <- function(gisco_id,
                              country = NULL,
                              streets_sf = NULL,
                              street_names_df = NULL,
                              lau_year = 2020,
                              include_checked_elsewhere_in_country = FALSE,
                              connection = NULL,
                              language = tidywikidatar::tw_get_language(),
                              search_language = NULL,
                              disconnect_db = FALSE,
                              ...) {
  if (is.null(country)) {
    country <- stringr::str_extract(string = gisco_id, pattern = "[[:alnum:]]{2}") %>%
      stringr::str_to_upper()
  }
  country_code <- sn_standard_country(country = country, type = "code")
  country_name <- sn_standard_country(country = country, type = "name")

  if (include_checked_elsewhere_in_country == TRUE) {
    current_gisco_id <- gisco_id

    if (is.null(streets_sf)) {
      usethis::ui_warn("To include checked elsewhere in country, `street_names_df` must be given.")
      manually_checked_df <- sn_get_street_named_after_id(
        gisco_id = gisco_id,
        country = country,
        language = language,
        connection = connection,
        only_checked = TRUE,
        disconnect_db = FALSE
      ) %>%
        dplyr::arrange(dplyr::desc(time))
    } else {
      if (!"street_name" %in% colnames(streets_sf)) {
        streets_sf <- streets_sf %>%
          dplyr::rename(street_name = name)
      }

      manually_checked_df <- sn_get_street_named_after_id(
        street_name = streets_sf %>%
          sf::st_drop_geometry() %>%
          dplyr::distinct(street_name) %>%
          dplyr::pull(street_name),
        country = country,
        language = language,
        connection = connection,
        only_checked = TRUE,
        disconnect_db = FALSE
      ) %>%
        dplyr::mutate(check_gisco_id = current_gisco_id == .data$gisco_id) %>%
        dplyr::arrange(check_gisco_id, dplyr::desc(time)) %>%
        dplyr::select(-check_gisco_id)
    }
  } else {
    manually_checked_df <- sn_get_street_named_after_id(
      gisco_id = gisco_id,
      country = country,
      language = language,
      connection = connection,
      only_checked = TRUE,
      disconnect_db = FALSE
    ) %>%
      dplyr::arrange(dplyr::desc(time))
  }


  # basic_checked_df
  manually_checked_core_df <- manually_checked_df %>%
    dplyr::select(
      .data$street_name,
      .data$named_after_id,
      .data$checked,
      .data$ignore,
      .data$person,
      .data$gender,
      .data$category,
      .data$tag,
      .data$named_after_custom_label
    ) %>%
    dplyr::distinct(.data$street_name, .keep_all = TRUE)

  if (is.null(search_language) == TRUE) {
    search_language <- streetnamer::sn_language_defaults_by_country %>%
      dplyr::filter(.data$country_code == country_code) %>%
      dplyr::pull(.data$language_code)

    if (length(search_language) == 0) {
      search_language <- language
    } else if (length(search_language) > 1) {
      search_language <- search_language[1]
    }
  }

  if (is.null(street_names_df)) {
    current_street_names_df <- latlon2map::ll_osm_get_lau_streets(
      gisco_id = gisco_id,
      unnamed_streets = FALSE,
      streets_sf = streets_sf,
      year = lau_year
    ) %>%
      sf::st_drop_geometry() %>%
      dplyr::rename(street_name = .data$name) %>%
      dplyr::distinct(street_name) %>%
      dplyr::mutate(name_clean = sn_clean_street_name(
        street_name = street_name,
        country = country_name
      )) %>%
      dplyr::mutate(
        street_name = street_name %>% stringr::str_replace_all(pattern = stringr::fixed("\\"), replacement = " ") %>% stringr::str_squish(),
        name_clean = name_clean %>% stringr::str_replace_all(pattern = stringr::fixed("\\"), replacement = " ") %>% stringr::str_squish()
      )
  } else {
    current_street_names_df <- street_names_df %>%
      dplyr::mutate(
        street_name = street_name %>% stringr::str_replace_all(pattern = stringr::fixed("\\"), replacement = " ") %>% stringr::str_squish(),
        name_clean = name_clean %>% stringr::str_replace_all(pattern = stringr::fixed("\\"), replacement = " ") %>% stringr::str_squish()
      )
  }

  street_names_for_automatic_checking_df <- current_street_names_df %>%
    dplyr::anti_join(
      y = manually_checked_core_df,
      by = "street_name"
    )

  if (nrow(street_names_for_automatic_checking_df) > 0) {
    automatically_checked_df <- sn_search_named_after(
      gisco_id = gisco_id,
      search_language = search_language,
      response_language = language,
      connection = connection,
      street_names_df = street_names_for_automatic_checking_df,
      disconnect_db = FALSE,
      cache = TRUE,
      ...
    )

    all_wikidata_df <- tw_get(
      id = c(automatically_checked_df$named_after_id),
      language = language,
      cache = TRUE,
      overwrite_cache = FALSE,
      cache_connection = connection,
      disconnect_db = FALSE
    )

    humans_df <- all_wikidata_df %>%
      dplyr::filter(
        is.na(.data$id) == FALSE,
        property == "P31",
        value == "Q5"
      ) %>%
      dplyr::distinct(.data$id) %>%
      dplyr::transmute(
        named_after_id = .data$id,
        person = as.numeric(1)
      )

    gender_df <- all_wikidata_df %>%
      dplyr::filter(
        is.na(.data$id) == FALSE,
        property == "P21"
      ) %>%
      dplyr::distinct(.data$id, .data$value) %>%
      dplyr::mutate(gender = dplyr::case_when(
        value == "Q6581097" ~ "male",
        value == "Q6581072" ~ "female",
        value %>% stringr::str_starts(string = , pattern = "Q") ~ "other"
      )) %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(gender = dplyr::case_when(
        "other" %in% gender ~ "other",
        dplyr::row_number() > 1 ~ "other",
        TRUE ~ gender
      ), .groups = "drop") %>%
      dplyr::distinct() %>%
      dplyr::rename(named_after_id = .data$id)


    automatically_checked_core_df <- automatically_checked_df %>%
      dplyr::transmute(.data$street_name,
        .data$named_after_id,
        checked = as.integer(0),
        ignore = as.integer(NA)
      ) %>%
      dplyr::left_join(y = humans_df, by = "named_after_id") %>%
      dplyr::mutate(person = dplyr::case_when(
        person == 1 ~ person,
        is.na(named_after_id) == FALSE ~ as.numeric(0)
      )) %>%
      dplyr::left_join(y = gender_df, by = "named_after_id") %>%
      dplyr::mutate(
        category = as.character(NA),
        tag = as.character(NA)
      )
    core_df <- dplyr::bind_rows(
      manually_checked_core_df,
      automatically_checked_core_df
    )
  } else {
    core_df <- manually_checked_core_df
  }

  core_df %>%
    dplyr::arrange(street_name)
}
