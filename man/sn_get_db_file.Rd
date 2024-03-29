% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{sn_get_db_file}
\alias{sn_get_db_file}
\title{Gets location of cache file}
\usage{
sn_get_db_file(type = "osm", country)
}
\arguments{
\item{type}{Defaults to "item". Type of cache file to output. Values typically used by `tidywikidatar` include "item", "search", and "qualifier".}

\item{language}{Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all}
}
\value{
A character vector of length one with location of item cache file.
}
\description{
Gets location of cache file
}
\examples{

tw_set_cache_folder(path = tempdir())
sqlite_cache_file_location <- sn_get_db_file() # outputs location of cache file
}
