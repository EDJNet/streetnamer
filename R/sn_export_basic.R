#' Export basic data for public consumption
#'
#' @inheritParams sn_derive_categories
#'
#' @return
#' @export
#'
#' @examples
sn_export_basic <- function(country = NULL,
                            gisco_id = NULL,
                            street_name = NULL,
                            streets_sf = NULL,
                            lau_year = 2020,
                            include_checked_elsewhere_in_country = FALSE,
                            occupation_categories_df = sn_occupation_categories_df,
                            canonization_religious_qid = c(
                              "Q43115",
                              "Q3464126"
                            ),
                            language = tidywikidatar::tw_get_language(),
                            connection = NULL,
                            disconnect_db = TRUE) {
  current_gisco_id <- gisco_id
  
  connection_db <- tidywikidatar::tw_connect_to_cache(
    connection = connection,
    language = language,
    cache = TRUE
  )
  
  with_categories_df <- sn_derive_categories(
    country = country,
    gisco_id = gisco_id,
    street_name = street_name,
    streets_sf = streets_sf,
    lau_year = lau_year,
    include_checked_elsewhere_in_country = include_checked_elsewhere_in_country,
    occupation_categories_df = occupation_categories_df,
    canonization_religious_qid = canonization_religious_qid,
    language = language,
    connection = connection_db,
    disconnect_db = FALSE
  )

  tw_df <- tidywikidatar::tw_get(
    id = with_categories_df[["named_after_id"]],
    language = language,
    cache = TRUE,
    overwrite_cache = FALSE,
    cache_connection = connection_db,
    disconnect_db = FALSE,
    wait = 0
  ) %>%
    dplyr::filter(is.na(id) == FALSE)

  export_df <- with_categories_df %>%
    dplyr::mutate(
      label = tidywikidatar::tw_get_label(
        id = named_after_id,
        language = language,
        id_df = tw_df,
        cache = TRUE,
        overwrite_cache = FALSE,
        cache_connection = connection_db,
        disconnect_db = FALSE
      ),
      description = tidywikidatar::tw_get_description(
        id = named_after_id,
        language = language,
        id_df = tw_df,
        cache = TRUE,
        overwrite_cache = FALSE,
        cache_connection = connection_db,
        disconnect_db = FALSE
      ),
      instance_of_label = tidywikidatar::tw_get_p1(
        id = named_after_id,
        p = "P31",
        language = language,
        id_df = tw_df,
        cache = TRUE,
        overwrite_cache = FALSE,
        cache_connection = connection_db,
        disconnect_db = FALSE
      ) %>%
        tidywikidatar::tw_get_label(
          language = language,
          id_df = tw_df,
          cache = TRUE,
          overwrite_cache = FALSE,
          cache_connection = connection_db,
          disconnect_db = FALSE
        ),
      date_of_birth = tidywikidatar::tw_get_p1(
        id = named_after_id,
        p = "P569",
        language = language,
        id_df = tw_df,
        cache = TRUE,
        overwrite_cache = FALSE,
        cache_connection = connection_db,
        disconnect_db = FALSE
      ),
      date_of_death = tidywikidatar::tw_get_p1(
        id = named_after_id,
        p = "P570",
        language = language,
        id_df = tw_df,
        cache = TRUE,
        overwrite_cache = FALSE,
        cache_connection = connection_db,
        disconnect_db = FALSE
      )
    ) %>% 
    dplyr::mutate(gisco_id = current_gisco_id) %>% 
    dplyr::select(gisco_id,
                  street_name,
                  country,
                  person,
                  gender,
                  category,
                  named_after_n,
                  named_after_id,
                  label,
                  description,
                  instance_of_label,
                  date_of_birth, 
                  date_of_death
                  )
  
  tidywikidatar::tw_disconnect_from_cache(
    cache = TRUE,
    cache_connection = connection_db,
    disconnect_db = disconnect_db,
    language = language
  )
  
  export_df
  
}
