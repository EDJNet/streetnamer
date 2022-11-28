#' Get data from Wikidata and generate a box with key information
#'
#' @param named_after_id A wikidata identifiers, must start with "Q".
#' @param language Defaults to "en". Must correspond to a two letter-code recognised by Wikidata.
#' @param connection A database connection, or a list of parameters compatible with `tidywikidatar`.
#'
#' @return A Shiny taglist / HTML code.
#' @export
#'
#' @examples
#'
#' sn_get_info_box("Q676555")
sn_get_info_box <- function(named_after_id,
                            language = "en",
                            connection = NULL,
                            disconnect_db = TRUE) {
  if (is.na(named_after_id)) {
    return(shiny::tagList())
  }

  current_db_connection <- tidywikidatar::tw_connect_to_cache(
    connection = connection,
    language = language,
    cache = TRUE
  )
  
  item_df <- tidywikidatar::tw_get(
    id = named_after_id,
    language = language,
    cache_connection = current_db_connection,
    cache = TRUE,
    disconnect_db = FALSE
  )

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
  } else if ("Q5" %in% instance_of_df$value) {
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
      language = language,
      cache_connection = current_db_connection,
      cache = TRUE,
      disconnect_db = FALSE
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

  wikidata_link <- htmltools::a(
    href = stringr::str_c("https://www.wikidata.org/wiki/", named_after_id),
    "Wikidata",
    target = "_blank",
    style = "text-decoration: underline;",
    .noWS = "outside"
  )

  wikipedia_base_link <- item_df %>%
    dplyr::filter(.data$property == stringr::str_c("sitelink_", language, "wiki")) %>%
    dplyr::pull("value")
  
  if (length(wikipedia_base_link)==0) {
    wikipedia_link <- NA_character_
  } else {
    wikipedia_link <- stringr::str_c("https://",
                                     language,
                                     ".wikipedia.org/wiki/",
                                     wikipedia_base_link)
  }

  if (is.na(wikipedia_link) == TRUE) {
    link_tag <- htmltools::tagList(
      "View on ",
      wikidata_link
    )
  } else {
    link_tag <- htmltools::tagList(
      wikidata_link,
      " / ",
      htmltools::a(href = wikipedia_link, "Wikipedia", target = "_blank", style = "text-decoration: underline;", .noWS = "outside")
    )
  }

  tidywikidatar::tw_disconnect_from_cache(
    cache = TRUE,
    cache_connection = current_db_connection,
    disconnect_db = disconnect_db,
    language = language
  )
  
  shiny::tagList(
    shiny::h3(label),
    shiny::p(subtitle),
    shiny::tags$i(description),
    shiny::p(link_tag)
  )
}
