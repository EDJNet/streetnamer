#' Set folder for db data
#'
#' Consider using a folder out of your current project directory, e.g. `sn_set_data_folder("~/R/sn_data/")`: you will be able to use the same cache in different projects, and prevent cached files from being sync-ed if you use services such as Nextcloud or Dropbox.
#'
#' @param path A path to a location used for caching data. If the folder does not exist, it will be created.
#'
#' @return The path to the caching folder, if previously set; the same path as given to the function; or the default, `tw_data` is none is given.
#' @export

#' @examples
#' \donttest{
#' if (interactive()) {
#'   sn_set_data_folder(fs::path(fs::path_home_r(), "R", "sn_data"))
#' }
#' }
sn_set_data_folder <- function(path = NULL) {
  if (is.null(path)) {
    path <- Sys.getenv("sn_data_folder")
  } else {
    Sys.setenv(sn_data_folder = path)
  }
  if (path == "") {
    path <- fs::path("sn_data")
  }
  invisible(path)
}

#' @rdname sn_set_data_folder
#' @examples
#' sn_get_data_folder()
#' @export
sn_get_data_folder <- sn_set_data_folder


#' Checks if db folder exists, if not returns an informative message
#'
#' @return If the cache folder exists, returns TRUE. Otherwise throws an error.
#' @export
#'
#' @examples
#'
#' # If cache folder does not exist, it throws an error
#' tryCatch(tw_check_cache_folder(),
#'   error = function(e) {
#'     return(e)
#'   }
#' )
#'
#' # Create cache folder
#' sn_set_data_folder(path = fs::path(
#'   tempdir(),
#'   "tw_cache_folder"
#' ))
#' sn_create_data_folder(ask = FALSE)
#'
#' sn_check_data_folder()
sn_check_data_folder <- function() {
  if (fs::file_exists(sn_get_data_folder()) == FALSE) {
    usethis::ui_stop(paste(
      "Cache folder does not exist. Set it with",
      usethis::ui_code("sn_get_data_folder()"),
      "and create it with",
      usethis::ui_code("sn_create_data_folder()")
    ))
  }
  TRUE
}


#' Ensure that connection to cache is disconnected consistently
#'
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param db_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#'
#' sn_disconnect_from_db()
sn_disconnect_from_db <- function(cache = NULL,
                                  db_connection = NULL,
                                  disconnect_db = TRUE,
                                  country = NULL) {
  if (sn_check_db(cache) == TRUE) {
    db <- sn_connect_to_db(
      connection = db_connection,
      country = country
    )

    if (DBI::dbIsValid(dbObj = db)) {
      if (disconnect_db == TRUE) {
        DBI::dbDisconnect(db)
      }
    }
  }
}



#' Gets location of cache file
#'
#' @param type Defaults to "item". Type of cache file to output. Values typically used by `tidywikidatar` include "item", "search", and "qualifier".
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return A character vector of length one with location of item cache file.
#' @export
#'
#' @examples
#'
#' tw_set_cache_folder(path = tempdir())
#' sqlite_cache_file_location <- sn_get_db_file() # outputs location of cache file
sn_get_db_file <- function(type = "osm",
                           country) {
  fs::path(
    streetnamer::sn_get_data_folder(),
    stringr::str_to_lower(country),
    stringr::str_c(
      "sn_",
      type,
      "_db_",
      stringr::str_to_lower(country),
      ".sqlite"
    )
  )
}

#' Gets name of table inside the database
#'
#' @param type Defaults to "item". Type of cache file to output. Values typically used by `tidywikidatar` include "item", "search", and "qualifier".
#' @param country Defaults to language set with `tw_set_language()`; "en" if not set. Used to limit the data to be cached. Use "all_available" to keep all data. For available values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return A character vector of length one with the name of the relevant table in the cache file.
#' @export
#'
#' @examples
#' # outputs name of table used in  of cache file
#' sn_get_db_table_name(type = "item", language = "en")
sn_get_db_table_name <- function(type,
                                 country) {
  stringr::str_c(
    "sn_",
    stringr::str_to_lower(country),
    "_",
    stringr::str_to_lower(type)
  )
}



#' Return a connection to be used for db
#'
#' @param connection Defaults to NULL. If NULL, uses local SQLite database. If given, must be a connection object (see example)
#' @param country Defaults to NULL.
#'
#' @return A connection object.
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   cache_connection <- DBI::dbConnect(
#'     RSQLite::SQLite(), # or e.g. odbc::odbc(),
#'     Driver =  ":memory:", # or e.g. "MariaDB",
#'     Host = "localhost",
#'     database = "example_db",
#'     UID = "example_user",
#'     PWD = "example_pwd"
#'   )
#'   sn_connect_to_db(cache_connection)
#' }
#' }
#'
sn_connect_to_db <- function(connection = NULL,
                             type = "osm",
                             country = NULL) {
  if (is.null(connection)) {
    sn_check_data_folder()
    db_file <- sn_get_db_file(
      type = type,
      country = country
    )
    db <- DBI::dbConnect(
      drv = RSQLite::SQLite(),
      db_file
    )
    db
  } else {
    connection
  }
}
