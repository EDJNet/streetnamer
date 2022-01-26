#' Creates the folder where `streetnamer` stores data.
#'
#' @param ask Logical, defaults to TRUE. If FALSE, and cache folder does not exist, it just creates it without asking (useful for interactive sessions).
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' sn_create_data_folder()
#' }
sn_create_data_folder <- function(ask = TRUE) {
  if (fs::file_exists(streetnamer::sn_get_data_folder()) == FALSE) {
    if (ask == FALSE) {
      fs::dir_create(path = streetnamer::sn_get_data_folder(), recurse = TRUE)
    } else {
      usethis::ui_info(glue::glue("The cache folder {{usethis::ui_path(sn_get_data_folder())}} does not exist. If you prefer to cache files elsewhere, reply negatively and set your preferred cache folder with `sn_set_cache_folder()`"))
      check <- usethis::ui_yeah(glue::glue("Do you want to create {{usethis::ui_path(sn_get_data_folder())}} for caching data?"))
      if (check == TRUE) {
        fs::dir_create(path = streetnamer::sn_get_data_folder(), recurse = TRUE)
      }
    }
    if (fs::file_exists(streetnamer::sn_get_data_folder()) == FALSE) {
      usethis::ui_stop("This function requires a valid cache folder.")
    }
  }
}
