#' Get city boundaries
#'
#' @param city The name of a city/municipality.
#' @param country The name of the country. Requested to ensure correct identification of city.
#' @param administrative Defaults to NULL. If TRUE, filters only boundaries recorded as administrative. For more information, see: https://wiki.openstreetmap.org/wiki/Tag\%3aboundary=administrative
#' @param admin_level Defaults to 6. For more information see: https://wiki.openstreetmap.org/wiki/Tag:boundary\%3Dadministrative#10_admin_level_values_for_specific_countries
#' @param cache Logical, defaults to TRUE. If TRUE, stores data in local subfolder data/cities/country_name/city_name.rds
#' @return An sf polygon.
#' @examples
#'
#' sn_get_osm_city_boundaries(search = "Sibiu, Romania")
#'
#' @export
#'

sn_get_osm_city_boundaries <- function(city,
                                       country,
                                       admin_level = 6,
                                       administrative = NULL,
                                       cache = TRUE) {
  query <- paste(city, country, sep = ", ")

  if (cache==TRUE) {
    streetnamer::sn_create_cache_folder()
    base_folder <- fs::path(streetnamer::sn_get_cache_folder(), "osm_city_boundaries")
    fs::dir_create(path = base_folder)
    file_location <- fs::path(base_folder,
                              stringr::str_to_lower(country),
                              paste0(stringr::str_to_lower(iconv(x = city,
                                                                 to = "ASCII//TRANSLIT")),
                                     ".rds"))
    if (fs::file_exists(file_location)==TRUE) {
      return(readr::read_rds(file = file_location))
    }
  }

  if (is.null(administrative)) {

    temp <- osmdata::opq(bbox = query) %>%
      osmdata::add_osm_feature(key = "admin_level",
                               value = admin_level) %>%
      osmdata::add_osm_feature(key = "place",
                               value = "city") %>%
      osmdata::osmdata_sf()

    if (is.null(temp$osm_polygons)==FALSE) {
      city_boundary <- temp$osm_polygons
    } else if (is.null(temp$osm_multipolygons)==FALSE) {
      city_boundary <- temp$osm_multipolygons
    }

  } else {
    temp <- osmdata::opq(bbox = query) %>%
      osmdata::add_osm_feature(key = "boundary",
                               value = "administrative") %>%
      osmdata::add_osm_feature(key = "admin_level",
                               value = admin_level) %>%
      osmdata::add_osm_feature(key = "place",
                               value = "city") %>%
      osmdata::osmdata_sf()

    if (is.null(temp$osm_polygons)==FALSE) {
      city_boundary <- temp$osm_polygons
    } else if (is.null(temp$osm_multipolygons)==FALSE) {
      city_boundary <- temp$osm_multipolygons
    }
  }
  if (is.null(city_boundary)) {
    usethis::ui_oops("City boundary not found.")
  } else {
    if (cache == TRUE) {
      fs::dir_create(path = fs::path(base_folder,
                                     stringr::str_to_lower(country)))
      saveRDS(object = city_boundary,
              file = file_location)
    }
  }
  city_boundary
}

