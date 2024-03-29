% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sn_search_named_after.R
\name{sn_search_named_after}
\alias{sn_search_named_after}
\title{Try to guess to whom a street is dedicated to}
\usage{
sn_search_named_after(
  gisco_id,
  search_language = NULL,
  response_language = tidywikidatar::tw_get_language(),
  check_named_after_original = TRUE,
  check_named_after_original_n = 1,
  check_named_after = TRUE,
  drop_if_street = TRUE,
  drop_if_disambiguation_page = TRUE,
  streets_sf = NULL,
  street_names_df = NULL,
  checked_df = NULL,
  cache = TRUE,
  overwrite_cache = FALSE,
  append = TRUE,
  connection = NULL,
  disconnect_db = TRUE
)
}
\arguments{
\item{gisco_id}{A gisco identifier of a European LAU.}

\item{search_language}{Defaults to NULL, guessed based on country.}

\item{response_language}{Defaults to `tidywikidatar::tw_get_language()`. Used
for defining the language in which label and description are returned.}

\item{check_named_after_original}{Defaults to FALSE. If TRUE, a search is
performed on the original street name. If the first result has values for
the property "named after", this takes precedence over other methods.}

\item{check_named_after_original_n}{Defaults to 1.}

\item{check_named_after}{Defaults to FALSE. If TRUE, a search is performed on
the "cleaned" name of the street. If the result is a street, road, square,
or similar, and this has values for the property "named after", this is
kept instead of the standard method.}

\item{drop_if_street}{Defaults to TRUE. If the result found is primarily an
instance of "street", "square", or such, as the result is probably the
street itself, not what or who it is dedicated to.}

\item{streets_sf}{Defaults to NULL. If given, used to speed up processing.
Must be an sf object such as the ones output by `ll_osm_get_roads()`.
Passed to `ll_osm_get_lau_streets()`.}

\item{street_names_df}{Defaults to NULL. If TRUE, must be a data frame with
two columns, named "street_name" and "name_clean" respectively. If given, these
are passed directly to the search routine. Useful when name cleaning
provided by the package is not satisfying, e.g. in places such as some
Belgian cities where street names are given in more than one language.}

\item{checked_df}{Defaults to NULL. If given, a data frame with a `street_name` and
`named_after_id` column. Takes precedence over searches.}
}
\description{
Try to guess to whom a street is dedicated to
}
