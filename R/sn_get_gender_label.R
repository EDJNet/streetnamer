#' Get gender label based on a Wikidata Q identifier
#'
#' @param named_after_id Wikidata Q identifier.
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
#'   sn_get_gender_label("Q7186")
#' }
sn_get_gender_label <- function(named_after_id,
                                language = tidywikidatar::tw_get_language(),
                                cache_connection = NULL,
                                cache = TRUE) {
  if (is.null(named_after_id)) {
    return(NULL)
  }
  gender_id <- tidywikidatar::tw_get_property_same_length(
    id = named_after_id,
    p = "P21",
    preferred = TRUE,
    only_first = TRUE,
    language = language,
    cache_connection = cache_connection,
    cache = cache
  )
  purrr::map_chr(
    .x = gender_id,
    .f = function(x) {
      if (is.na(x)) {
        gender_selected <- as.character(NA)
      } else if (x == "Q6581097") {
        gender_selected <- "male"
      } else if (x == "Q6581072") {
        gender_selected <- "female"
      } else if (stringr::str_starts(string = x, pattern = "Q")) {
        gender_selected <- "other"
      }
      gender_selected
    }
  )

}
