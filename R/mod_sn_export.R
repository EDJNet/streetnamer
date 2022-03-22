#' sn_export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sn_export_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("download_buttons_ui"))
  )
}

#' sn_export Server Functions
#'
#' @noRd
mod_sn_export_server <- function(id,
                                 country,
                                 gisco_id,
                                 language = tidywikidatar::tw_get_language(),
                                 connection = NULL,
                                 enable = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    output$download_current_country <- downloadHandler(
      filename = function() {
        country_v <- country
        country_name <- sn_lau_by_nuts %>%
          dplyr::distinct(.data$country, .data$country_name) %>%
          dplyr::filter(.data$country == country_v) %>%
          dplyr::pull(.data$country_name)

        stringr::str_c(
          stringr::str_to_lower(iconv(
            x = country_name,
            to = "ASCII//TRANSLIT"
          )),
          "-",
          as.integer(Sys.time()),
          ".csv"
        )
      },
      content = function(con) {
        download_df <- sn_get_street_name_wikidata_id(
          country = country,
          language = language,
          connection = connection
        )

        readr::write_csv(download_df, con)
      }
    )


    output$download_current_municipality <- downloadHandler(
      filename = function() {
        gisco_id_v <- gisco_id

        lau_label <- sn_lau_by_nuts %>%
          dplyr::filter(.data$gisco_id == gisco_id_v) %>%
          dplyr::pull(.data$lau_label)

        stringr::str_c(
          gisco_id,
          "-",
          stringr::str_to_lower(iconv(
            x = lau_label,
            to = "ASCII//TRANSLIT"
          )),
          "-",
          as.integer(Sys.time()),
          ".csv"
        )
      },
      content = function(con) {
        download_df <- sn_get_street_name_wikidata_id(
          gisco_id = gisco_id,
          country = country,
          language = language,
          connection = connection
        )
        if (is.null(download_df)) {
          return(NULL)
        }

        readr::write_csv(download_df, con)
      }
    )

    ### prepare UI ###
    if (enable == TRUE) {
      output$download_buttons_ui <- renderUI(tagList(
        downloadButton(
          outputId = ns("download_current_municipality"),
          label = "Download municipality"
        ),
        downloadButton(
          outputId = ns("download_current_country"),
          label = "Download country"
        )
      ))
    } else {
      output$download_buttons_ui <- renderUI(tagList())
    }
  })
}

## To be copied in the UI
# mod_sn_export_ui("sn_export_ui_1")

## To be copied in the server
# mod_sn_export_server("sn_export_ui_1")



#' A minimal shiny app used for categorising streets
#'
#' @param gisco_id
#' @param country
#' @param language
#'
#' @return
#' @export
#'
#' @examples
#'
#' if (interactive) {
#'   mod_sn_export_app(
#'     country = "IT",
#'     gisco_id = "IT_022205",
#'   )
#' }
mod_sn_export_app <- function(country,
                              gisco_id,
                              language = tidywikidatar::tw_get_language()) {
  ui <- shiny::fluidPage(
    mod_sn_export_ui("snm_export_ui_1")
  )
  server <- function(input, output, session) {
    mod_sn_export_server(
      id = "snm_export_ui_1",
      gisco_id = gisco_id,
      country = country,
      language = language
    )
  }
  shiny::shinyApp(ui, server)
}
