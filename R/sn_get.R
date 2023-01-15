#' Extract shape files of roads from previously downloaded
#'
#' @param countries The name of one or more geographic entities from files typically previously downloaded with `sn_download_osm()`
#' @return Nothing, used for its side effects (extracts shapefiles from country-level zip files)
#' @examples
#' \dontrun{
#' sn_extract_streets(countries = "Romania")
#' }
#'
#' @export
#'

sn_extract_streets <- function(countries) {
  streetnamer::sn_create_cache_folder()

  fs::dir_create(path = fs::path(streetnamer::sn_get_cache_folder(), "streets_shp"))

  purrr::walk(
    .x = tolower(countries),
    .f = function(current_country) {
      current_country_zip_folder <- fs::path(
        streetnamer::sn_get_cache_folder(),
        "countries_shp_zip",
        current_country
      )
      if (fs::file_exists(current_country_zip_folder) == FALSE) {
        usethis::ui_info(glue::glue("'{current_country}' is not available locally. You can download it with 'sn_download_osm('{current_country}')'."))
        usethis::ui_stop(glue::glue("{current_country} not available."))
      } else {
        local_files <- fs::dir_ls(
          path = current_country_zip_folder,
          recurse = FALSE,
          type = "file",
          glob = "*.shp.zip"
        )



        purrr::walk(
          .x = local_files,
          .f = function(current_zip_file) {
            files_to_extract <- unzip(
              zipfile = current_zip_file,
              list = TRUE
            ) %>%
              tibble::as_tibble() %>%
              dplyr::filter(stringr::str_detect(string = Name, pattern = "roads")) %>%
              dplyr::pull(Name)

            current_street_shp_folder <-
              fs::path(
                streetnamer::sn_get_cache_folder(),
                "streets_shp",
                current_country,
                current_zip_file %>%
                  fs::path_file() %>%
                  stringr::str_remove(pattern = "-latest-free.shp.zip")
              )


            unzip(
              zipfile = current_zip_file,
              files = files_to_extract,
              exdir = current_street_shp_folder
            )
          }
        )
      }
    }
  )
}

#' Extract shape files of roads from previously downloaded
#'
#' @param country The name of one or more geographic entities from files typically previously downloaded with `sn_download_osm()`
#' @return Nothing, used for its side effects (extracts shapefiles from country-level zip files)
#' @examples
#' \dontrun{
#' sn_get_streets(country = "Romania")
#' }
#'
#' @export
#'

sn_get_streets <- function(country) {
  country <- tolower(country)

  country_street_shp_folder <-
    fs::path(
      streetnamer::sn_get_cache_folder(),
      "streets_shp",
      country
    )
  
  if (fs::file_exists(country_street_shp_folder) == FALSE) {
    sn_extract_streets(countries = country)
  }

  street_folders <- fs::dir_ls(
    path = country_street_shp_folder,
    type = "directory",
    recurse = FALSE
  )

  purrr::map_dfr(
    .x = street_folders,
    .f = function(x) sf::st_read(dsn = x)
  )
}

#' Extract shape files of places from previously downloaded
#'
#' @param countries The query to be searched.
#' @param export_rds Stores imported shape files as an rds file locally.
#' @param export_csv Stores imported shape files (excluding the geographic information) as a csv file locally.
#' @return A data.frame with geographic data (sf).
#' @examples
#'
#' sn_extract_places(countries = "Romania")
#' @export
#'

sn_extract_places <- function(countries,
                              export_rds = FALSE,
                              export_csv = FALSE) {
  dir.create(path = file.path("data", "places_shp"), showWarnings = FALSE)
  countries <- tolower(countries)

  for (i in countries) {
    if (is.element(i, big_countries) == TRUE) {
      filenames <- list.files(path = file.path("data", "shp_zip", i), pattern = "shp.zip", full.names = TRUE)
      for (j in seq_along(filenames)) {
        file_location <- filenames[j]

        files_to_extract <- unzip(zipfile = file_location, list = TRUE) %>%
          tibble::as_tibble() %>%
          dplyr::pull(Name)

        unzip(
          zipfile = file_location,
          files = files_to_extract[stringr::str_detect(string = files_to_extract, pattern = "places")],
          exdir = file.path(
            "data",
            "places_shp",
            i,
            stringr::str_remove(
              string = filenames[j],
              pattern = stringr::fixed(paste0("data/shp_zip/", i, "/"))
            ) %>%
              stringr::str_remove(pattern = "-latest-free.shp.zip")
          )
        )
      }
      regions <- list.files(path = file.path("data", "shp_zip", i)) %>% stringr::str_remove(pattern = stringr::fixed("-latest-free.shp.zip"))
      places <- purrr::map_dfr(.x = regions, .f = function(x) sf::st_read(dsn = file.path("data", "places_shp", i, x)))
    } else {
      file_location <- file.path("data", "shp_zip", paste0(i, "-latest-free.shp.zip"))
      if (file.exists(file_location) == FALSE) {
        warning(paste0("File not available. Please download the data first with `download_OSM('", i, "')`"))
      } else {
        files_to_extract <- unzip(zipfile = file_location, list = TRUE) %>%
          tibble::as_tibble() %>%
          dplyr::pull(Name)
        unzip(
          zipfile = file_location,
          files = files_to_extract[stringr::str_detect(string = files_to_extract, pattern = "places")],
          exdir = file.path("data", "places_shp", i)
        )

        places <- sf::st_read(dsn = file.path("data", "places_shp", i))
      }
    }
    if (export_rds == TRUE) {
      dir.create(path = file.path("data", "places_rds"), showWarnings = FALSE)
      dir.create(path = file.path("data", "places_rds", i), showWarnings = FALSE)
      saveRDS(
        object = places,
        file = file.path(file.path("data", "places_rds", paste0(i, "_places.rds")))
      )
    }
    if (export_csv == TRUE) {
      dir.create(path = file.path("data", "places_csv"), showWarnings = FALSE)
      dir.create(path = file.path("data", "places_csv", i), showWarnings = FALSE)
      readr::write_csv(
        x = places %>% st_set_geometry(NULL),
        path = file.path(file.path("data", "places_csv", paste0(i, "_places.csv")))
      )
    }
    return(places)
  }
}


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
#' sn_get_city_boundaries(city = "Sibiu", country = "Romania")
#' @export
#'

sn_get_city_boundaries <- function(city,
                                   country,
                                   admin_level = 6,
                                   administrative = NULL,
                                   cache = TRUE) {
  query <- paste(city, country, sep = ", ")

  dir.create(path = "data", showWarnings = FALSE)
  dir.create(path = file.path("data", "city_boundaries"), showWarnings = FALSE)
  dir.create(path = file.path("data", "city_boundaries", tolower(country)), showWarnings = FALSE)

  file_location <- file.path("data", "city_boundaries", tolower(country), paste0(tolower(iconv(x = city, to = "ASCII//TRANSLIT")), ".rds"))
  if (file.exists(file_location) == FALSE) {
    if (is.null(administrative)) {
      temp <- osmdata::opq(bbox = query) %>%
        osmdata::add_osm_feature(key = "admin_level", value = admin_level) %>%
        osmdata::add_osm_feature(key = "place", value = "city") %>%
        osmdata::osmdata_sf()

      if (is.null(temp$osm_polygons) == FALSE) {
        city_boundary <- temp$osm_polygons
      } else if (is.null(temp$osm_multipolygons) == FALSE) {
        city_boundary <- temp$osm_multipolygons
      }
    } else {
      temp <- osmdata::opq(bbox = query) %>%
        osmdata::add_osm_feature(key = "boundary", value = "administrative") %>%
        osmdata::add_osm_feature(key = "admin_level", value = admin_level) %>%
        osmdata::add_osm_feature(key = "place", value = "city") %>%
        osmdata::osmdata_sf()

      if (is.null(temp$osm_polygons) == FALSE) {
        city_boundary <- temp$osm_polygons
      } else if (is.null(temp$osm_multipolygons) == FALSE) {
        city_boundary <- temp$osm_multipolygons
      }
    }
    if (is.null(city_boundary)) {
      usethis::ui_oops("City boundary not found.")
    } else {
      if (cache == TRUE) {
        dir.create(path = file.path("data", "city_boundaries"), showWarnings = FALSE)
        saveRDS(object = city_boundary, file = file_location)
      }
    }
  } else {
    city_boundary <- readRDS(file = file_location)
  }
  return(city_boundary)
}


#' Get boundary by id
#'
#' @param id A numeric vector of length 1, must correspond to the id of a boundary object on OpenStreetMap.
#' @param type Defaults to "way".
#' @param cache Logical, defaults to TRUE. If TRUE, stores data in local subfolder data/cities/country_name/city_name.rds
#' @return An sf polygon.
#' @examples
#'
#' sn_get_boundary_by_id(id = c(Arad = 45422208))
#' @export
#'
sn_get_boundary_by_id <- function(id,
                                  type = "way",
                                  cache = TRUE) {
  fs::dir_create(
    path = file.path("data", "city_boundaries", "by_id"),
    recursive = TRUE
  )

  if (is.numeric(id) == TRUE) {
    query_by_id <- sn_create_city_boundary_id_combo(id = id, type = type)
  } else if (class(id) == "list") {
    query_by_id <- id
  } else if (is.na(as.numeric(id) == FALSE)) {
    query_by_id <- sn_create_city_boundary_id_combo(id = as.numeric(id), type = type)
  } else {
    stop("Wrong id format. `id` must be an integer or an object created with `create_city_boundary_id_combo()`.")
  }

  file_location <- file.path("data", "city_boundaries", "by_id", paste0(tolower(query_by_id$type), "-", query_by_id$id, ".rds"))

  if (file.exists(file_location) == FALSE) {
    temp <- osmdata::opq_osm_id(
      type = query_by_id$type,
      id = query_by_id$id
    ) %>%
      osmdata::opq_string() %>%
      osmdata::osmdata_sf()

    if (is.null(temp$osm_polygons) == FALSE) {
      city_boundary <- temp$osm_polygons
    } else if (is.null(temp$osm_multipolygons) == FALSE) {
      city_boundary <- temp$osm_multipolygons
    }

    if (cache == TRUE) {
      saveRDS(object = city_boundary, file = file_location)
      if (is.null(query_by_id$city) == FALSE & is.null(query_by_id$country) == FALSE) {
        file_city_location <- file.path("data", "city_boundaries", tolower(query_by_id$country), paste0(tolower(iconv(x = query_by_id$city, to = "ASCII//TRANSLIT")), ".rds"))
        saveRDS(object = city_boundary, file = file_city_location)
      }
    }
  } else {
    city_boundary <- readRDS(file = file_location)
  }
  return(city_boundary)
}

#' Create a city/id combination for proper caching
#'
#' @param id A numeric vector of length 1, must correspond to the id of a boundary object on OpenStreetMap.
#' @param city The name of a city/municipality.
#' @param country The name of the country. Requested to ensure correct identification of city.
#' @param type One of either "way", "relation", or "node".
#' @param cache Logical, defaults to TRUE. If TRUE, stores data in local subfolder data/cities/country_name/city_name.rds
#' @return A list, typically to be fed into `get_boundary_by_id()``
#' @examples
#'
#' create_city_boundary_id_combo(id = 45422208, type = "way", city = "Arad", country = "Romania")
#'
#' # https://www.openstreetmap.org/relation/46663
#' create_city_boundary_id_combo(id = 46663, type = "relation", city = "Trento", country = "Italy", )
#' @export
sn_create_city_boundary_id_combo <- function(id,
                                             type,
                                             city = NULL,
                                             country = NULL,
                                             cache = TRUE) {
  combo <- list(
    city = city,
    country = country,
    id = as.numeric(id),
    type = type
  )
  if (cache == TRUE) {
    fs::dir_create(
      path = file.path("data", "city_boundary_id_combo", country),
      recursive = TRUE
    )
    saveRDS(
      object = combo,
      file = file.path(
        "data",
        "city_boundary_id_combo",
        country,
        paste0(
          tolower(iconv(x = city, to = "ASCII//TRANSLIT")),
          ".rds"
        )
      )
    )
  }
  return(combo)
}

#' Keep only roads within a given boundary.
#'
#' @param boundary An object typically created with `get_city_boundaries()`
#' @param country The name of the country. Requested to ensure correct identification of city.
#' @return A data frame of the sf class including all roads insidet the given boundary
#' @examples
#'
#' subset_roads(city_boundary, roads)
#' @export
#'
sn_subset_streets <- function(boundary, streets) {
  streets[sf::st_within(streets, boundary) %>%
    lengths() > 0, ]
}
