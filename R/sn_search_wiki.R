#' Search string in Wikidata and return Wikidata id, label, and description.
#'
#' @param search A string to be searched in Wikidata
#' @param language Language to be used for the search
#' @param limit Maximum numbers of responses to be given.
#' @param wait In seconds, defaults to 1. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied.
#' @param cache Logical, defaults to TRUE. If TRUE, search queries are stored in a local sqlite database located in the `wiki_search_db` folder within the local cache folder.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' sn_search_wiki(search = "Mihai Eminescu", language = "en")
#' sn_search_wiki(search = "Mihai Eminescu", language = "ro")
#' }
#'
sn_search_wiki <- function(search,
                           language = "en",
                           limit = 10,
                           wait = 1,
                           cache = TRUE) {
  if (is.null(search)) {
    usethis::ui_stop("A search string must be given.")
  }

  if (cache == TRUE) {
    streetnamer::sn_create_cache_folder()
    db_folder <- fs::path(
      streetnamer::sn_get_cache_folder(),
      "wiki_search_db"
    )
    fs::dir_create(db_folder)
    db_file <- fs::path(
      db_folder,
      stringr::str_c(language, ".sqlite")
    )
    db <- DBI::dbConnect(drv = RSQLite::SQLite(), db_file)
    db_result <- tryCatch(
      DBI::dbReadTable(
        conn = db,
        name = stringr::str_to_lower(search)
      ),
      error = function(e) {
        logical(1L)
      }
    )
    if (is.data.frame(db_result)) {
      return(db_result)
    }
  }

  Sys.sleep(time = wait)

  search_response <- tryCatch(WikidataR::find_item(
    search_term = search,
    language = language,
    limit = limit
  ),
  error = function(e) {
    warning(e)
    tibble::tibble(
      id = NA,
      label = NA,
      description = NA
    )
  }
  )
  if (length(search_response) == 0) {
    search_response_df <- tibble::tibble(
      id = NA,
      label = NA,
      description = NA
    )
  } else if (tibble::is_tibble(search_response) == TRUE) {
    search_response_df <- search_response
  } else {
    search_response_df <- purrr::map_dfr(
      .x = search_response,
      .f = function(x) {
        tibble::tibble(
          id = x %>% purrr::pluck("id"),
          label = dplyr::if_else(is.null(x %>% purrr::pluck("label")), as.character(NA), x %>% purrr::pluck("label")),
          description = dplyr::if_else(
            condition = is.null(x %>% purrr::pluck("description")),
            true = as.character(NA),
            false = x %>% purrr::pluck("description")
          )
        )
      }
    )
  }

  if (cache == TRUE) {
    RSQLite::dbWriteTable(
      conn = db,
      name = stringr::str_to_lower(string = search),
      value = search_response_df
    )
    DBI::dbDisconnect(db)
  }
  search_response_df
}


#' Return (almost) all information from a Wikidata item in a tidy format
#'
#' @param id A characther vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart
#' @param cache Logical, defaults to TRUE. If TRUE, it stores all retrieved data in a local sqlite database.
#'
#' @return A data.frame (a tibble) with two columns: property and value
#' @export
#'
#' @examples
#' \dontrun{
#' sn_extract_wiki("Q254")
#' }
#'
sn_tidy_wiki <- function(id,
                         cache = TRUE) {
  if (cache == TRUE) {
    streetnamer::sn_create_cache_folder()
    db_folder <- fs::path(
      streetnamer::sn_get_cache_folder(),
      "wiki_q_db"
    )
    fs::dir_create(db_folder)
    db_file <- fs::path(
      db_folder,
      stringr::str_c("wiki_q_db", ".sqlite")
    )
    db <- DBI::dbConnect(drv = RSQLite::SQLite(), db_file)
    db_result <- tryCatch(
      DBI::dbReadTable(
        conn = db,
        name = stringr::str_to_upper(id)
      ),
      error = function(e) {
        logical(1L)
      }
    )
    if (is.data.frame(db_result)) {
      return(db_result)
    }
  }


  item <- tryCatch(WikidataR::get_item(id = id),
    error = function(e) {
      return(tibble::tibble(id = NA))
    }
  )
  labels <- item %>% purrr::pluck(1, "labels")

  labels_df <- purrr::map_dfr(
    .x = labels,
    function(current_label_l) {
      tibble::tibble(
        property = paste0("label_", current_label_l$language),
        value = current_label_l$value
      )
    }
  )

  aliases <- item %>% purrr::pluck(1, "aliases")

  aliases_df <- purrr::map_dfr(
    .x = aliases,
    function(current_alias_l) {
      tibble::tibble(
        property = paste0("alias_", current_alias_l$language),
        value = current_alias_l$value
      )
    }
  )

  claims <- item %>% purrr::pluck(1, "claims")

  claims_df <- purrr::map_dfr(
    .x = claims,
    .f = function(current_claim_l) {
      property <- current_claim_l$mainsnak$property

      value_pre <- claims[[unique(property)]][["mainsnak"]][["datavalue"]][["value"]]

      if (is.null(value_pre)) {
        value <- as.character("NA")
      } else if (is.data.frame(value_pre)) {
        if (is.element("time", names(value_pre))) {
          value <- value_pre$time
        } else if (is.element("text", names(value_pre))) {
          value <- value_pre$text
        } else if (is.element("amount", names(value_pre))) {
          value <- value_pre$amount
        } else {
          value <- value_pre$id
        }
      } else if (is.character(value_pre)) {
        value <- value_pre
      }

      tibble::tibble(
        property = property,
        value = value
      )
    }
  )


  descriptions <- item %>% purrr::pluck(1, "descriptions")

  descriptions_df <- purrr::map_dfr(
    .x = descriptions,
    function(current_description_l) {
      tibble::tibble(
        property = paste0("description_", current_description_l$language),
        value = current_description_l$value
      )
    }
  )


  sitelinks <- item %>% purrr::pluck(1, "sitelinks")

  sitelinks_df <- purrr::map_dfr(
    .x = sitelinks,
    function(current_sitelink_l) {
      tibble::tibble(
        property = paste0("sitelink_", current_sitelink_l$site),
        value = current_sitelink_l$title
      )
    }
  )

  everything_df <- dplyr::bind_rows(
    labels_df,
    aliases_df,
    claims_df,
    descriptions_df,
    sitelinks_df
  )
  if (cache == TRUE) {
    RSQLite::dbWriteTable(
      conn = db,
      name = stringr::str_to_upper(string = id),
      value = everything_df
    )
    DBI::dbDisconnect(db)
  }
  everything_df
}
