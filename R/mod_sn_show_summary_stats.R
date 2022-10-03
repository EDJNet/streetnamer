#' mod_show_summary_stats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sn_show_summary_stats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("summary_stats_ui"))
  )
}

#' mod_show_summary_stats Server Functions
#'
#' @noRd
mod_sn_show_summary_stats_server <- function(id,
                                             gisco_id,
                                             gisco_label = NULL,
                                             country = NULL,
                                             streets_sf = NULL,
                                             street_names_df = NULL,
                                             include_checked_elsewhere_in_country = TRUE,
                                             connection = NULL,
                                             language = tidywikidatar::tw_get_language(),
                                             search_language = NULL,
                                             disconnect_db = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    w <- waiter::Waiter$new(id = ns("summary_stats_ui"),
                            html = waiter::spin_loaders(id = 15,color = "#FF5454"),
                            fadeout = TRUE,
                            color = "#fef7ed")
    
    if (is.null(gisco_label)) {
      gisco_label <- ""
    }

  
    output$summary_stats_ui <- shiny::renderUI({
      w$show()
      
      country_code <- sn_standard_country(country = country, type = "code")
      
      if (country_code %in% sn_countries_with_streets_as_qid) {
        core_df <- sn_get_city_combo(
          gisco_id = gisco_id,
          country = country,
          streets_sf = streets_sf,
          street_names_df = street_names_df,
          check_named_after_original = TRUE, 
          check_named_after = FALSE,
          include_checked_elsewhere_in_country = include_checked_elsewhere_in_country,
          connection = connection,
          language = language,
          search_language = search_language,
          disconnect_db = disconnect_db
        )
      } else {
        core_df <- sn_get_city_combo(
          gisco_id = gisco_id,
          country = country,
          streets_sf = streets_sf,
          street_names_df = street_names_df,
          include_checked_elsewhere_in_country = include_checked_elsewhere_in_country,
          connection = connection,
          language = language,
          search_language = search_language,
          disconnect_db = disconnect_db,
          check_named_after_original = FALSE, 
          check_named_after = FALSE
        )
      }
      
      total_streets <- nrow(core_df)
      total_checked <- nrow(core_df %>% dplyr::filter(as.logical(checked)))
      total_ignored <- nrow(core_df %>% dplyr::filter(as.logical(ignore)))
      total_valid <- total_streets - total_ignored
      total_with_named_after_id <- nrow(core_df %>%
                                          dplyr::filter(
                                            is.na(ignore) | as.logical(ignore) == FALSE,
                                            is.na(named_after_id) == FALSE
                                          ))
      total_with_named_after_id_checked <- nrow(core_df %>%
                                                  dplyr::filter(
                                                    is.na(ignore) | as.logical(ignore) == FALSE,
                                                    is.na(named_after_id) == FALSE,
                                                    as.logical(checked)
                                                  ))
      
      total_tentative_humans <- nrow(core_df %>%
                                       dplyr::filter(
                                         is.na(ignore) | as.logical(ignore) == FALSE,
                                         as.logical(person)
                                       ))
      
      total_humans_checked <- nrow(core_df %>%
                                     dplyr::filter(
                                       is.na(ignore) | as.logical(ignore) == FALSE,
                                       as.logical(checked),
                                       as.logical(person)
                                     ))
      
      total_humans_with_id_checked <- nrow(core_df %>%
                                             dplyr::filter(
                                               is.na(ignore) | as.logical(ignore) == FALSE,
                                               is.na(named_after_id) == FALSE,
                                               as.logical(checked),
                                               as.logical(person)
                                             ))
      
      total_humans_with_gender <- nrow(core_df %>%
                                         dplyr::filter(
                                           is.na(ignore) | as.logical(ignore) == FALSE,
                                           as.logical(checked),
                                           as.logical(person),
                                           is.na(gender) == FALSE
                                         ))
      
      
      core_confirmed_individuals_with_id_df <- core_df %>%
        dplyr::filter(
          is.na(ignore) | as.logical(ignore) == FALSE,
          is.na(named_after_id) == FALSE,
          as.logical(checked),
          as.logical(person)
        ) %>%
        dplyr::distinct(.data$named_after_id)
      
      items_df <- tidywikidatar::tw_get(
        id = core_confirmed_individuals_with_id_df$named_after_id,
        language = language,
        cache = TRUE,
        overwrite_cache = FALSE,
        cache_connection = connection,
        disconnect_db = FALSE
      )
      
      
      qid_with_gender <- items_df %>%
        dplyr::filter(property == "P21") %>%
        # gender
        dplyr::distinct(.data$id) %>%
        base::nrow()
      
      qid_with_dob <- items_df %>%
        dplyr::filter(property == "P569") %>%
        # date of birth
        dplyr::distinct(.data$id) %>%
        base::nrow()
      
      qid_with_dod <- items_df %>%
        dplyr::filter(property == "P570") %>%
        # date of death
        dplyr::distinct(.data$id) %>%
        base::nrow()
      
      qid_with_pob <- items_df %>%
        dplyr::filter(property == "P19") %>%
        # place of birth
        dplyr::distinct(.data$id) %>%
        base::nrow()
      
      qid_with_pod <- items_df %>%
        dplyr::filter(property == "P20") %>%
        # place of death
        dplyr::distinct(.data$id) %>%
        base::nrow()
      
      qid_with_occupation <- items_df %>%
        dplyr::filter(property == "P106") %>%
        # place of death
        dplyr::distinct(.data$id) %>%
        base::nrow()
      
      
      summary_taglist <- shiny::tagList(
        shiny::h2(stringr::str_c("Summary statistics for ", gisco_label, " (", gisco_id, ")")),
        shiny::tags$ul(
          purrr::map(
            .x = c(
              stringr::str_c("Streets in OpenStreetMap: ", scales::number(total_streets)),
              stringr::str_c(
                "Streets to be ignored: ", scales::number(total_ignored),
                " (", scales::percent(total_ignored / total_streets), " of total)"
              ),
              stringr::str_c(
                "Streets with valid street name: ", scales::number(total_valid),
                " (", scales::percent(total_valid / total_streets), " of total)"
              ),
              stringr::str_c(
                "Streets checked: ", scales::number(total_checked),
                " (", scales::percent(total_checked / total_valid), " of valid street names)"
              ),
              stringr::str_c(
                "Streets named after entity with Wikidata identifier: ", scales::number(total_with_named_after_id),
                " (", scales::percent(total_with_named_after_id / total_valid), " of valid street names),",
                " including ", scales::number(total_with_named_after_id_checked), " that were manually checked and ",
                scales::number(total_valid - total_with_named_after_id_checked), " that have been tentatively matched automatically"
              )
            ),
            function(.x) shiny::tags$li(.x)
          )
        ),
        shiny::h2("Summary statistics about streets named after identifiable individuals"),
        shiny::tags$ul(
          purrr::map(
            .x = c(
              stringr::str_c("Total humans (tentative, including both checked and not checked): ", scales::number(total_tentative_humans)),
              stringr::str_c("Total humans (confirmed): ", scales::number(total_humans_checked)),
              stringr::str_c("Total humans with id (confirmed): ", scales::number(total_humans_with_id_checked))
            ),
            function(.x) shiny::tags$li(.x)
          )
        ),
        shiny::h2("Things to do to achieve full coverage for streets named after identifiable individuals"),
        shiny::tags$ul(
          purrr::map(
            .x = c(
              stringr::str_c(stringr::str_c("Streets to be checked: ", scales::number(total_valid - total_checked))),
              stringr::str_c(stringr::str_c("Dedicated to people, but gender missing: ", scales::number(total_humans_checked - total_humans_with_gender))),
              stringr::str_c(stringr::str_c("Dedicated to people, but identifier missing: ", scales::number(total_humans_checked - total_humans_with_id_checked)))
            ),
            function(.x) shiny::tags$li(.x)
          )
        ),
        shiny::h2("How much does Wikidata know about confirmed individuals?"),
        shiny::tags$ul(
          purrr::map(
            .x = c(
              stringr::str_c("Total confirmed individuals with Wikidata identifier: ", scales::number(nrow(core_confirmed_individuals_with_id_df))),
              stringr::str_c("With gender: ", scales::number(qid_with_gender)),
              stringr::str_c("With date of birth: ", scales::number(qid_with_dob)),
              stringr::str_c("With date of death: ", scales::number(qid_with_dod)),
              stringr::str_c("With place of birth: ", scales::number(qid_with_pob)),
              stringr::str_c("With place of death: ", scales::number(qid_with_pod))
            ),
            function(.x) shiny::tags$li(.x)
          )
        ),
        shiny::p(shiny::tags$i("* Since streets may be dedicated to more than one individual, totals may not match exactly, as it is expected that there are slightly more individuals than streets named after individuals."))
      )
      
      
      
      
      summary_taglist 
    })

    core_df_r <- shiny::reactive({
      core_df
    })

    shiny::reactive(core_df_r())
  })
}

## To be copied in the UI
# mod_sn_show_summary_stats_ui("mod_sn_show_summary_stats_1")

## To be copied in the server
# mod_sn_show_summary_stats_server("mod_sn_show_summary_stats_1")


#' A minimal shiny app used for showing summary statistics
#'
#' @param gisco_id Identifier of the city.
#' @param country Two letter country code
#' @param language Two letter language code.
#'
#' @return
#' @export
#'
#' @examples
#'
#' if (interactive) {
#'   mod_sn_show_summary_stats_app(
#'     gisco_id = "IT_022205",
#'     gisco_label = "Trento",
#'     country = "IT"
#'   )
#' }
mod_sn_show_summary_stats_app <- function(gisco_id,
                                          gisco_label = NULL,
                                          country = NULL,
                                          connection = NULL,
                                          language = tidywikidatar::tw_get_language()) {
  ui <- shiny::fluidPage(
    waiter::useWaiter(),
    mod_sn_show_summary_stats_ui("mod_sn_show_summary_stats_1")
  )
  server <- function(input, output, session) {
    mod_sn_show_summary_stats_server(
      id = "mod_sn_show_summary_stats_1",
      gisco_id = gisco_id,
      gisco_label = gisco_label,
      country = country,
      connection = connection,
      language = language
    )
  }
  shiny::shinyApp(ui, server)
}
