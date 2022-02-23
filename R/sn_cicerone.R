#' Create Cicerone guide
#'
#' @return
#' @export
#'
#' @examples
#' if (interactive) {
#'   guide <- sn_cicerone()
#' }
sn_cicerone <- function() {
  cicerone::Cicerone$
    new()$ 
    step(
      el = "ignore_street",
      title = "Not a street name?",
      description = "Click here to add to the ignore list street names that are not actually street names"
    )$
    step(
      el = "current_city_sn_dt",
      title = "Select a street to see more details",
      description = "Find out to what or whom the selected street is named after, or contribute this information if it is not yet available"
    )$
    step(
      el = "[data-value='Contribute']",
      title = "Improve our data!",
      description = "You can contribute by matching Wikidata identifiers to street names, or tag those that are not on Wikidata",
      is_id = FALSE
    )$
    step(
      el = "[data-value='Export']",
      title = "You can export data from here",
      description = "You will need to login before downloading",
      is_id = FALSE
    )
}