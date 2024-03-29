% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sn_get_city_combo.R
\name{sn_get_city_combo}
\alias{sn_get_city_combo}
\title{Get a data frame with all streets in a city with matching, including both automatic and manually checked inputs}
\usage{
sn_get_city_combo(
  gisco_id,
  country = NULL,
  streets_sf = NULL,
  street_names_df = NULL,
  lau_year = 2020,
  include_checked_elsewhere_in_country = FALSE,
  connection = NULL,
  language = tidywikidatar::tw_get_language(),
  search_language = NULL,
  disconnect_db = FALSE,
  ...
)
}
\arguments{
\item{gisco_id}{Identifier of a municipality, typically a gisco identifier.
Can be any code, as long as it used consistently, and it starts with a
two-letter country code.}

\item{country}{Defaults to NULL. A character string, expected to be a
two-letter country code. If not given, tentatively extracted from
`gisco_id`.}

\item{include_checked_elsewhere_in_country}{Defaults to FALSE. If TRUE, checks if streets with the same name have been checked at country level. Requires `streets_sf`.}

\item{connection}{Defaults to NULL. If NULL, and caching is enabled,
`streetnamer` will use a local sqlite database. A custom connection to
other databases can be given (see vignette `caching` for details).}

\item{disconnect_db}{Defaults to TRUE. If FALSE, leaves the connection to
cache open.}
}
\description{
Get a data frame with all streets in a city with matching, including both automatic and manually checked inputs
}
