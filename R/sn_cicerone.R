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
    el = "[data-value='About']",
    title = "Welcome!",
    description = "To find out more about Mapping Diversity and this interface, check out the 'About' tab, where you will find more details and relvant links. If you are interested in aggregated data or analyses for major European cities, you should start from here.",
    is_id = FALSE
  )$
    step(
    el = "current_gisco_id",
    title = "Select a municipality",
    description = "All things in this interface are based on a given city or municipality. So... first thing in your journey, select the municipality you are interested in. If you are the first person to check for a given muncipality, relevant data will need to be retrieved... the interface may become unrespnsive, but leave this tab open and come back after a couple of minutes.",
    is_id = FALSE
  )$
    step(
    el = "[data-value='About this municipality']",
    title = "Just curious about some basic statistics about a given municipality?",
    description = "Click here for some summary statistics.",
    is_id = FALSE
  )$
    step(
    el = "[data-value='Contribute']",
    title = "Improve our data!",
    description = "You can contribute by matching Wikidata identifiers to street names, or tag those that are not on Wikidata",
    is_id = FALSE
  )$
    step(
    el = "[data-value='Instructions']",
    title = "Instructions",
    description = "If you plan to contribute, or when you're in doubt, check out the 'instructions' tab for more information.",
    is_id = FALSE
  )$
    step(
    el = "current_city_sn_dt",
    title = "Ready to contribute? Select a street",
    description = "Find out to what or whom the selected street is named after, or contribute this information if it is not yet available."
  )$
    step(
    el = "confirm_match",
    title = "Confirm and move on!",
    description = "If you have contributed some information, make sure to click on the 'Confirm' button. This will store data in our database, and move the selector to the next street."
  )$
    step(
    el = "ignore_street",
    title = "Not a street name?",
    description = "Click here to add to the ignore list street names that are not actually street names, such as digits or service lanes without a meaningful name."
  )$
    step(
    el = "take_a_tour",
    title = "That's it!",
    description = "Keep in mind that this web is not a finished, polished, product. If the app becomes unresponsive, you mostly will just need to wait for a few seconds. If you're processing lots of data for the first time, it can take many minutes... just leave the tab open and come back after a while. You will find in the 'About' tab more information about giving feedback or reporting some issues."
  )
}
