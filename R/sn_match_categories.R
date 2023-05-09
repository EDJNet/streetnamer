#' Derive categories based on available information
#'
#' This function attributes a category to a subject first based on manuale
#' categorisation. If there is no match, and Wikidata is available, it checks
#' for the canonisation status and sets "religion" for all saints. Then it
#' attributes categories based on a list of occcupations.
#'
#' @param occupation_categories_df A data frame of matches between occupation
#'   (P106) and category
#' @param canonization_religious_qid A vector of QID. All those that have one of
#'   these QIDs as canonisation status (P411) will be categorised as "religion".
#' @inheritParams sn_get_street_named_after_id
#'
#' @return
#' @export
#'
#' @examples
sn_derive_categories <- function(country = NULL,
                                gisco_id = NULL,
                                street_name = NULL,
                                streets_sf = NULL,
                                lau_year = 2020,
                                include_checked_elsewhere_in_country = FALSE,
                                occupation_categories_df = sn_occupation_categories_df,
                                canonization_religious_qid = c("Q43115",
                                                               "Q3464126"),
                                language = tidywikidatar::tw_get_language(),
                                connection = NULL,
                                disconnect_db = TRUE) {
  city_df <- sn_get_street_named_after_id(
    country = country,
    gisco_id = gisco_id,
    street_name = street_name,
    streets_sf = streets_sf,
    lau_year = lau_year,
    include_checked_elsewhere_in_country = include_checked_elsewhere_in_country,
    keep_only_latest = TRUE,
    remove_ignored = TRUE,
    language = language,
    connection = connection,
    disconnect_db = disconnect_db
  )
  
  category_manually_set_df <- city_df %>% 
    dplyr::filter(is.na(category) == FALSE & category != "") 
  
  
  religion_df <- city_df %>% 
    dplyr::filter(!(is.na(category) == FALSE & category != "")) %>% 
    dplyr::filter(is.na(named_after_id)==FALSE) %>% 
    dplyr::mutate(canonization_qid = tidywikidatar::tw_get_p(id = named_after_id,
                                                              p = "P411",
                                                              cache = TRUE,
                                                              overwrite_cache = FALSE,
                                                              cache_connection = connection,
                                                              disconnect_db = FALSE)) %>% 
    tidyr::unnest(canonization_qid) %>% 
    dplyr::filter(is.na(canonization_qid)==FALSE) %>% 
    dplyr::filter(canonization_qid %in% canonization_religious_qid) %>% 
    dplyr::mutate(category = "religion")  %>% 
    dplyr::distinct(street_name, named_after_id, category, .keep_all = TRUE)
  
  occupation_df <- city_df %>% 
    dplyr::filter(!(is.na(category) == FALSE & category != "")) %>% 
    dplyr::filter(is.na(named_after_id)==FALSE) %>% 
    dplyr::anti_join(y = religion_df,
                     by = "street_name") %>% 
    dplyr::mutate(occupation_qid = tidywikidatar::tw_get_p1(id = named_after_id,
                                                            p = "P106",
                                                           cache = TRUE,
                                                           overwrite_cache = FALSE,
                                                           cache_connection = connection,
                                                           disconnect_db = FALSE)) %>% 
    dplyr::select(-category) %>% 
    dplyr::left_join(occupation_categories_df %>% 
                       dplyr::rename(occupation_qid = qid) %>% 
                       dplyr::filter(is.na(occupation_qid)==FALSE),
                     by = "occupation_qid") %>% 
    dplyr::filter(is.na(category)==FALSE)

  
   all_pre_df <- dplyr::bind_rows(category_manually_set_df, 
                   religion_df,
                   occupation_df
                   )
   
   without_category_df <- city_df %>% 
     dplyr::anti_join(y = all_pre_df, 
                      by = c("street_name"))
   
   tidywikidatar::tw_disconnect_from_cache(cache = TRUE,
                                           cache_connection = connection,
                                           disconnect_db = disconnect_db)
   
   dplyr::bind_rows(all_pre_df, 
                    without_category_df)
   
   
  }
