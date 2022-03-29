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
sn_get_details <- function(id,
                           collapse_lists = FALSE,
                           language = tidywikidatar::tw_get_language(),
                           connection = NULL, 
                           cache = TRUE) {
  
  qid_tw_df <- tidywikidatar::tw_get(id = id,
                                         language = language,
                                         cache = cache,
                                         cache_connection = connection)
  
  df_l <- tibble::tibble(qid = id) %>% 
    dplyr::mutate(label = tidywikidatar::tw_get_label(qid, language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df), 
                  description = tidywikidatar::tw_get_description(qid, language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>% 
    dplyr::mutate(instance_of_qid = tidywikidatar::tw_get_p(id = qid, p = "P31", language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>% 
    dplyr::mutate(instance_of_label = purrr::map(.x = instance_of_qid,
                                                     function(x) {
                                                       x %>% 
                                                         unlist() %>% 
                                                         tidywikidatar::tw_get_label(language = language, cache_connection = connection, cache = cache) 
                                                     })) %>% 
    dplyr::mutate(sex_or_gender_qid = tidywikidatar::tw_get_p(id = qid, p = "P21", language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(sex_or_gender_label = purrr::map(.x = sex_or_gender_qid,
                                                   function(x) {
                                                     x %>% 
                                                       unlist() %>% 
                                                       tidywikidatar::tw_get_label(language = language, cache_connection = connection, cache = cache) 
                                                   })) %>% 
    dplyr::mutate(occupation_qid = tidywikidatar::tw_get_p(id = qid, p = "P106", language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(occupation_label = purrr::map(.x = occupation_qid,
                                                function(x) {
                                                  x %>% 
                                                    unlist() %>% 
                                                    tidywikidatar::tw_get_label(language = language, cache_connection = connection, cache = cache) 
                                                })) %>% 

    dplyr::mutate(date_of_birth = tidywikidatar::tw_get_p(qid, p = "P569", only_first = TRUE, preferred = TRUE, language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>% 
    dplyr::mutate(year_of_birth = stringr::str_extract(
      string = date_of_birth,
      pattern = "[[:print:]][[:digit:]]{4}"
    ) %>% 
      as.numeric()) %>% 
    dplyr::mutate(place_of_birth_qid = tidywikidatar::tw_get_p(qid, p = "P19", only_first = TRUE, preferred = TRUE, language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>% 
    dplyr::mutate(place_of_birth_label = tidywikidatar::tw_get_label(place_of_birth_qid, language = language, cache_connection = connection, cache = cache),
                  place_of_birth_country_qid = tidywikidatar::tw_get_p(place_of_birth_qid, p = "P17", only_first = TRUE, preferred = TRUE, language = language, cache_connection = connection, cache = cache)) %>% 
    dplyr::mutate(place_of_birth_country_label = tidywikidatar::tw_get_label(place_of_birth_country_qid, language = language, cache_connection = connection, cache = cache),
                  place_of_birth_coordinates = tidywikidatar::tw_get_p(id = place_of_birth_qid, p = "P625", only_first = TRUE, preferred = TRUE, language = language, cache_connection = connection, cache = cache)) %>% 
    tidyr::separate(
      col = place_of_birth_coordinates,
      into = c("place_of_birth_latitude","place_of_birth_longitude"),
      sep = ",",
      remove = TRUE,
      convert = TRUE) %>% 
    dplyr::mutate(date_of_death = tidywikidatar::tw_get_p(qid, p = "P570", only_first = TRUE, preferred = TRUE, language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>% 
    dplyr::mutate(year_of_death = stringr::str_extract(
      string = date_of_death,
      pattern = "[[:print:]][[:digit:]]{4}"
    ) %>% 
      as.numeric()) %>% 
    dplyr::mutate(place_of_death_qid = tidywikidatar::tw_get_p(qid, p = "P20", only_first = TRUE, preferred = TRUE, language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>% 
    dplyr::mutate(place_of_death_label = tidywikidatar::tw_get_label(place_of_death_qid, language = language, cache_connection = connection, cache = cache),
                  place_of_death_country_qid = tidywikidatar::tw_get_p(place_of_death_qid, p = "P17", only_first = TRUE, preferred = TRUE, language = language, cache_connection = connection, cache = cache)) %>% 
    dplyr::mutate(place_of_death_country_label = tidywikidatar::tw_get_label(place_of_death_country_qid, language = language, cache_connection = connection, cache = cache),
                  place_of_death_coordinates = tidywikidatar::tw_get_p(id = place_of_death_qid, p = "P625", only_first = TRUE, preferred = TRUE, language = language, cache_connection = connection, cache = cache)) %>% 
    tidyr::separate(
      col = place_of_death_coordinates,
      into = c("place_of_death_latitude","place_of_death_longitude"),
      sep = ",",
      remove = TRUE,
      convert = TRUE) %>% 
    dplyr::mutate(cause_of_death_qid = tidywikidatar::tw_get_p(qid, p = "P509", only_first = TRUE, preferred = TRUE, language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>% 
    dplyr::mutate(cause_of_death_label = tidywikidatar::tw_get_label(cause_of_death_qid, language = language, cache_connection = connection, cache = cache)) %>% 
  
    dplyr::mutate(manner_of_death_qid = tidywikidatar::tw_get_p(qid, p = "P1196", only_first = TRUE, preferred = TRUE, language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>% 
    dplyr::mutate(manner_of_death_label = tidywikidatar::tw_get_label(manner_of_death_qid, language = language, cache_connection = connection, cache = cache)) %>% 
   
    dplyr::mutate(position_held_qid = tidywikidatar::tw_get_p(id = qid, p = "P39", language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(position_held_label = purrr::map(.x = position_held_qid,
                                                function(x) {
                                                  x %>% 
                                                    unlist() %>% 
                                                    tidywikidatar::tw_get_label(language = language, cache_connection = connection, cache = cache) 
                                                })) %>% 
    dplyr::mutate(religious_order_qid = tidywikidatar::tw_get_p(qid, p = "P611", only_first = TRUE, preferred = TRUE, language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(religious_order_label = tidywikidatar::tw_get_label(religious_order_qid, language = language, cache_connection = connection, cache = cache)) %>% 
    
    dplyr::mutate(religion_qid = tidywikidatar::tw_get_p(qid, p = "P140", only_first = TRUE, preferred = TRUE, language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(religion_label = tidywikidatar::tw_get_label(religion_qid, language = language, cache_connection = connection, cache = cache)) %>% 
    
    dplyr::mutate(canonization_status_qid = tidywikidatar::tw_get_p(qid, p = "P411", only_first = TRUE, preferred = TRUE, language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(canonization_status_label = tidywikidatar::tw_get_label(canonization_status_qid, language = language, cache_connection = connection, cache = cache)) %>% 

    dplyr::mutate(military_branch_qid = tidywikidatar::tw_get_p(id = qid, p = "P241", language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(military_branch_label = purrr::map(.x = military_branch_qid,
                                              function(x) {
                                                x %>% 
                                                  unlist() %>% 
                                                  tidywikidatar::tw_get_label(language = language, cache_connection = connection, cache = cache) 
                                              })) %>%
    dplyr::mutate(military_rank_qid = tidywikidatar::tw_get_p(id = qid, p = "P410", language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(military_rank_label = purrr::map(.x = military_rank_qid,
                                                     function(x) {
                                                       x %>% 
                                                         unlist() %>% 
                                                         tidywikidatar::tw_get_label(language = language, cache_connection = connection, cache = cache) 
                                                     })) %>%
    dplyr::mutate(noble_title_qid = tidywikidatar::tw_get_p(id = qid, p = "P97", language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(noble_title_label = purrr::map(.x = noble_title_qid,
                                                   function(x) {
                                                     x %>% 
                                                       unlist() %>% 
                                                       tidywikidatar::tw_get_label(language = language, cache_connection = connection, cache = cache) 
                                                   })) %>%
    dplyr::mutate(family_qid = tidywikidatar::tw_get_p(id = qid, p = "P97", language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(family_label = purrr::map(.x = family_qid,
                                                 function(x) {
                                                   x %>% 
                                                     unlist() %>% 
                                                     tidywikidatar::tw_get_label(language = language, cache_connection = connection, cache = cache) 
                                                 })) %>%
    dplyr::mutate(family_instance_of_qid = tidywikidatar::tw_get_p(id = qid, p = "P31", language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(family_instance_of_label = purrr::map(.x = family_instance_of_qid,
                                            function(x) {
                                              x %>% 
                                                unlist() %>% 
                                                tidywikidatar::tw_get_label(language = language, cache_connection = connection, cache = cache) 
                                            })) %>%
    
    
    
    dplyr::mutate(conflict_qid = tidywikidatar::tw_get_p(id = qid, p = "P607", language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(conflict_label = purrr::map(.x = conflict_qid,
                                              function(x) {
                                                x %>% 
                                                  unlist() %>% 
                                                  tidywikidatar::tw_get_label(language = language, cache_connection = connection, cache = cache) 
                                              })) %>%   
    dplyr::mutate(participant_in_qid = tidywikidatar::tw_get_p(id = qid, p = "P607", language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)) %>%
    dplyr::mutate(participant_in_label = purrr::map(.x = participant_in_qid,
                                              function(x) {
                                                x %>% 
                                                  unlist() %>% 
                                                  tidywikidatar::tw_get_label(language = language, cache_connection = connection, cache = cache) 
                                              })) %>%   
    dplyr::mutate(
      picture = tidywikidatar::tw_get_image_same_length(id = qid, language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df, only_first = TRUE),
      wikipedia = tidywikidatar::tw_get_wikipedia(id = qid, language = language, cache_connection = connection, cache = cache, id_df = qid_tw_df)
    )
    
  if (collapse_lists) {
    df_l %>% 
      dplyr::group_by(qid) %>% 
      dplyr::mutate(dplyr::across(where(is.list),
                                  function(x) stringr::str_c(unique(unlist(x)), collapse = ";"))) %>% 
      dplyr::ungroup()
  } else {
    df_l
  }




}

