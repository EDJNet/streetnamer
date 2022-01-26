#' Gets roads within city boundaries, crops them, and keeps only those with names
#' 
#' This function downloads all roads for a given country, downloads boundaries for a given administrative unit, and removes roads with no names. 
#'
#' @param gisco_id Gisco id of the municipality
#'
#' @return
#' @export
#'
#' @examples
sn_get_named_city_streets <- function(gisco_id,
                                      cache_names = FALSE,
                                      cache_sf = FALSE) {
  country_code <- stringr::str_extract(string = gisco_id, pattern = "[A-Z][A-Z]") %>% 
    stringr::str_to_upper()
  city_code <- stringr::str_extract(string = gisco_id, pattern = "[[:digit:]]+")
  
  cached_file <- fs::path(sn_get_data_folder(),
                          stringr::str_to_lower(country_code),
                          "named_city_roads_sf",
                          stringr::str_c(gisco_id,
                                         ".rds"))
  
  if (fs::file_exists(cached_file)) {
    return(readr::read_rds(cached_file))
  }
  
  if (country_code == "IT") {
    named_city_roads <- ll_osm_extract_it(level = "comuni",
                                           code = city_code) %>%
      dplyr::filter(is.na(name)==FALSE, is.na(highway)==FALSE) %>% 
      sf::st_intersection(y = ll_get_nuts_it(level = "lau",
                                             resolution = "high") %>% 
                                dplyr::filter(PRO_COM_T == city_code)) %>% 
      dplyr::group_by(name) %>%
      dplyr::summarise() 

  } else {
    country_full_name <- countrycode::codelist %>% 
      dplyr::filter(iso2c == country_code) %>% 
      dplyr::pull(iso.name.en)
    
    named_city_roads <- ll_osm_get_roads(country = country_full_name) %>%
      dplyr::filter(is.na(name)==FALSE, is.na(fclass)==FALSE) %>% 
      sf::st_intersection(ll_get_lau_eu() %>% 
                            dplyr::filter(GISCO_ID == gisco_id)) %>% 
      dplyr::group_by(name) %>%
      dplyr::summarise() 
  }

  if (cache_names == TRUE) {
    sn_write_lau_street_names(df = named_city_roads %>% 
                                sf::st_drop_geometry() %>% 
                                dplyr::transmute(gisco_id = gisco_id, name), 
                              type = "osm_lau_street_names",
                              country = "it")
  }

  if (cache_sf == TRUE) {
    fs::dir_create(fs::path(sn_get_data_folder(),
                            stringr::str_to_lower(country_code),
                            "named_city_roads_sf"))
    
    named_city_roads %>% 
      saveRDS(file = cached_file)
  }
  named_city_roads
  
}