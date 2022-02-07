#' Countries and geographic entities for which shapefiles are made availabile by Geofabrik
#'
#' A dataset with all names of countries, continents, as included in the Geofabrik database.
#' They are used to download files with `sn_download_osm()`
#'
#' Links to shapefiles are stored as tibbles. Unnest to see them, e.g.
#' `sn_osm_countries %>% tidyr::unnest(link)`
#' or for a single country:
#' `sn_osm_countries %>% dplyr::filter(country == "italy") %>% tidyr::unnest(link)`
#'
#' @format A tibble
#' \describe{
#'   \item{continent}{Name of the continent}
#'   \item{country}{Name of the country}
#'   \item{link}{Link to shapefiles in a tibble}
#' }
#' @source \url{http://download.geofabrik.de/}
"sn_osm_countries"


#' A dataset with all European LAUs, with the correspondent NUTS regions
#'
#' Based on 2020 LAUs and 2021 NUTS regions.
#' 
#' For details, see https://edjnet.github.io/lau_centres/
#'
#' @source \url{https://edjnet.github.io/lau_centres/}
"sn_lau_by_nuts"
