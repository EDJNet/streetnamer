#' Get gender label based on a Wikidata Q identifier
#'
#' @param wikidata_id Wikidata Q identifier.
#' @param language  to be passed to `tidywikidatar` for caching. Label is always returned in English.
#' @param cache_connection Defaults to NULL, Passed to `tidywikidatar`.
#' @param cache Defaults to TRUE. Passed to `tidywikidatar`.
#'
#' @return
#' @export
#'
#' @examples
#' 
#' if (interactive()) {
#' sn_get_gender_label("Q7186")
#' }
sn_get_gender_label <- function(wikidata_id, 
                                language = tidywikidatar::tw_get_language(), 
                                cache_connection = NULL, 
                                cache = TRUE
) {
  if (is.null(wikidata_id)) {
    return(NULL)
  }
  gender_id <- tidywikidatar::tw_get_property_same_length(
    id = wikidata_id,
    p = "P21",
    preferred = TRUE,
    only_first = TRUE,
    language = language,
    cache_connection = cache_connection,
    cache = cache
  )
  if (is.na(gender_id)) {
    gender_selected <- as.character(NA)
  } else if (gender_id == "Q6581097") {
    gender_selected <- "male"
  } else if (gender_id == "Q6581072") {
    gender_selected <- "female"
  } else if (stringr::str_starts(string = gender_id, pattern = "Q")) {
    gender_selected <- "other"
  }
  gender_selected
}