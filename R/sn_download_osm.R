#' Download OSM data for whole countries from Geofabrik.
#'
#' N.B. Names do not always correspond to official name of countries and may include different geographic entities.
#' For a full list of available "countries" as made available by Geofabrik, see the internal dataset `sn_osm_countries`.
#' Be considered in downloading file.
#'
#' @param countries One or more country names. For details on available country names see the dataset included in this package: `sn_osm_countries`
#' @param overwrite Logical, defaults to FALSE. If true, downloads new files even if already present.
#' @return Used only for its side effects (downloads osm data).
#' @examples
#' \dontrun{
#' sn_download_osm(countries = "Romania")
#' sn_download_osm(countries = c("chile", "colombia"))
#' }
#' @export
sn_download_osm <- function(countries,
                            overwrite = FALSE) {
  countries_available_l <- is.element(tolower(countries), sn_osm_countries$country)
  if (Reduce(x = countries_available_l, f = `&`) == FALSE) {
    missing_countries <- glue::glue_collapse(x = countries[!countries_available_l], sep = ", ", last = ", and ")
    usethis::ui_oops("The following countries are not available: {missing_countries}")
    usethis::ui_info("See the internal dataset `sn_osm_countries` for a list of available countries and geographic entities")
    usethis::ui_stop("Please input an accepted geographic entity name")
  }

  downloads_df <- tibble::tibble(country = tolower(countries)) %>%
    dplyr::left_join(y = sn_osm_countries, by = "country") %>%
    tidyr::unnest(link)

  streetnamer::sn_create_cache_folder()
  base_folder <- fs::path(sn_get_cache_folder(), "countries_shp_zip")
  fs::dir_create(path = base_folder)

  purrr::pwalk(
    .l = downloads_df,
    .f = function(country, continent, link) {
      country_folder <- fs::path(
        base_folder,
        country
      )
      local_file <- fs::path(country_folder, fs::path_file(link))

      if (fs::file_exists(local_file) == FALSE | overwrite == TRUE) {
        fs::dir_create(country_folder)
        download.file(url = link, destfile = local_file)
      }
    }
  )
}
