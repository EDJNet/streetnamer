#' Get name_clean_df for cities with bilingual city names
#' 
#' So far supported:
#' 
#' - Brussels
#'
#' @param gisco_id A gisco identifier
#'
#' @return
#' @export
#'
#' @examples
sn_get_clean_street_name_bilingual_df <- function(gisco_id) {
  current_street_names_df <- latlon2map::ll_osm_get_lau_streets(
    gisco_id = gisco_id,
    unnamed_streets = FALSE,
    streets_sf = NULL
  ) %>%
    sf::st_drop_geometry() %>%
    dplyr::distinct(name)
  
  if (gisco_id %in% c("BE_21004", "BE_21015", "BE_21001", "BE_21012", "BE_21016")) {
    name_clean_df <- current_street_names_df %>% 
      tidyr::separate(col = name, into = c("French", "Flemish"), sep = " - ", remove = FALSE) %>% 
      dplyr::transmute(name, name_clean = French) %>% 
      dplyr::mutate(name_clean = sn_clean_street_name(street_name = .data$name_clean,
                                                      country = "Belgium"))
  } else {
    usethis::ui_warn("Doing standard name cleaning, as no custom rule for the given gisco_id is currently available.")
    name_clean_df <- current_street_names_df %>%  
      dplyr::mutate(name_clean = sn_clean_street_name(
        street_name = name,
        country = country_name
      )) 
  }
  
  output_df <- name_clean_df %>% 
    dplyr::mutate(name = name %>% stringr::str_replace_all(pattern = stringr::fixed("\\"), replacement = " ") %>% stringr::str_squish(),
                  name_clean = name_clean %>% stringr::str_replace_all(pattern = stringr::fixed("\\"), replacement = " ") %>% stringr::str_squish())
  
  output_df
}
