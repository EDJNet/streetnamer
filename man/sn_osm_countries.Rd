% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{sn_osm_countries}
\alias{sn_osm_countries}
\title{Countries and geographic entities for which shapefiles are made availabile by Geofabrik}
\format{
A tibble
\describe{
  \item{continent}{Name of the continent}
  \item{country}{Name of the country}
  \item{link}{Link to shapefiles in a tibble}
}
}
\source{
\url{http://download.geofabrik.de/}
}
\usage{
sn_osm_countries
}
\description{
A dataset with all names of countries, continents, as included in the Geofabrik database.
They are used to download files with `sn_download_osm()`
}
\details{
Links to shapefiles are stored as tibbles. Unnest to see them, e.g.
`sn_osm_countries %>% tidyr::unnest(link)`
or for a single country:
`sn_osm_countries %>% dplyr::filter(country == "italy") %>% tidyr::unnest(link)`
}
\keyword{datasets}
