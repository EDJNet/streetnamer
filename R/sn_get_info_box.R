#' Get data from Wikidata and generate a box with key information
#'
#' @param wikidata_id A wikidata identifiers, must start with "Q".
#' @param language Defaults to "en". Must correspond to a two letter-code recognised by Wikidata.
#'
#' @return A Shiny taglist / HTML code.
#' @export
#'
#' @examples
#'
#' sn_get_info_box("Q676555")
sn_get_info_box <- function(wikidata_id,
                            language = "en") {
  if (is.na(wikidata_id)) {
    return(shiny::tagList())
  }

  item_df <- tidywikidatar::tw_get(id = wikidata_id)

  label_df <- item_df %>%
    dplyr::filter(
      stringr::str_starts(
        string = .data$property,
        pattern = stringr::str_c("label", "_")
      ),
      stringr::str_ends(
        string = .data$property,
        pattern = stringr::str_c(language,
          collapse = "|"
        )
      )
    ) %>%
    dplyr::distinct(.data$id,
      .keep_all = TRUE
    )

  if (nrow(label_df) > 0) {
    label <- label_df[["value"]][[1]]
  } else {
    label <- as.character("")
  }

  # check if human

  instance_of_df <- item_df %>%
    dplyr::filter(.data$property == "P31")

  if (nrow(instance_of_df) == 0) {
    subtitle <- ""
  } else if (instance_of_df$value %in% "Q5") {
    # it's a human!
    year_of_birth_df <- item_df %>%
      dplyr::filter(.data$property == "P569")
    if (nrow(year_of_birth_df) > 0) {
      year_of_birth <- year_of_birth_df %>%
        dplyr::pull(value) %>%
        stringr::str_extract(pattern = "[[:digit:]]{4}") %>%
        as.numeric() %>%
        head(1)
    } else {
      year_of_birth <- ""
    }

    year_of_death_df <- item_df %>%
      dplyr::filter(.data$property == "P570")
    if (nrow(year_of_death_df) > 0) {
      year_of_death <- year_of_death_df %>%
        dplyr::pull(value) %>%
        stringr::str_extract(pattern = "[[:digit:]]{4}") %>%
        as.numeric() %>%
        head(1)
    } else {
      year_of_death <- ""
    }
    if (year_of_birth == "" & year_of_death == "") {
      subtitle <- ""
    } else {
      subtitle <- glue::glue("({year_of_birth}-{year_of_death})")
    }
  } else {
    # not a human!
    instance_of_label <- tidywikidatar::tw_get_label(instance_of_df$value[1],
      language = language
    )

    if (is.na(instance_of_label)) {
      subtitle <- ""
    } else {
      subtitle <- instance_of_label
    }
  }


  description_df <- item_df %>%
    dplyr::filter(
      stringr::str_starts(
        string = .data$property,
        pattern = stringr::str_c("description", "_")
      ),
      stringr::str_ends(
        string = .data$property,
        pattern = stringr::str_c(language,
          collapse = "|"
        )
      )
    ) %>%
    dplyr::distinct(.data$id,
      .keep_all = TRUE
    )

  if (nrow(description_df) > 0) {
    description <- description_df[["value"]][[1]]
  } else {
    description <- as.character("")
  }

  shiny::tagList(
    shiny::h5(label),
    shiny::HTML(subtitle),
    shiny::tags$i(description)
  )
}
