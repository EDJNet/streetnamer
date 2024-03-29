% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{sn_check_data_folder}
\alias{sn_check_data_folder}
\title{Checks if db folder exists, if not returns an informative message}
\usage{
sn_check_data_folder(path = NULL)
}
\value{
If the cache folder exists, returns TRUE. Otherwise throws an error.
}
\description{
Checks if db folder exists, if not returns an informative message
}
\examples{

# If cache folder does not exist, it throws an error
tryCatch(tw_check_cache_folder(),
  error = function(e) {
    return(e)
  }
)

# Create cache folder
sn_set_data_folder(path = fs::path(
  tempdir(),
  "sn_data_folder"
))
sn_create_data_folder(ask = FALSE)

sn_check_data_folder()
}
