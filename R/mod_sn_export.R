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
        download_df <- sn_get_street_named_after_id(
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
        download_df <- sn_get_street_named_after_id(
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



    output$download_current_municipality_with_details_csv <- downloadHandler(
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
          "-with_details_",
          as.integer(Sys.time()),
          ".csv"
        )
      },
      content = function(con) {
        download_df <- sn_export(
          gisco_id = gisco_id,
          country = country,
          export_format = "csv",
          unlist = TRUE,
          write_file = FALSE,
          language = language,
          connection = connection, cache = TRUE
        )
        if (is.null(download_df)) {
          return(NULL)
        }
        readr::write_csv(download_df, con)
      }
    )

    output$download_current_municipality_with_details_geojson <- downloadHandler(
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
          "-with_details_",
          as.integer(Sys.time()),
          ".geojson"
        )
      },
      content = function(con) {
        download_sf <- sn_export(
          gisco_id = gisco_id,
          country = country,
          export_format = "geojson",
          unlist = TRUE,
          write_file = FALSE,
          language = language,
          connection = connection, cache = TRUE
        )
        if (is.null(download_sf)) {
          return(NULL)
        }
        sf::st_write(
          obj = download_sf,
          dsn = con
        )
      }
    )

    output$download_current_municipality_with_details_rds_sf <- downloadHandler(
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
          "-with_details_",
          as.integer(Sys.time()),
          ".rds"
        )
      },
      content = function(con) {
        download_sf <- sn_export(
          gisco_id = gisco_id,
          country = country,
          export_format = "rds_sf",
          unlist = FALSE,
          write_file = FALSE,
          language = language,
          connection = connection, cache = TRUE
        )
        if (is.null(download_sf)) {
          return(NULL)
        }
        saveRDS(object = download_sf, file = con)
      }
    )


    output$download_current_municipality_with_details_rds_sf <- downloadHandler(
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
          "-with_details_",
          as.integer(Sys.time()),
          ".rds"
        )
      },
      content = function(con) {
        download_df <- sn_export(
          gisco_id = gisco_id,
          country = country,
          export_format = "rds",
          unlist = FALSE,
          write_file = FALSE,
          language = language,
          connection = connection, cache = TRUE
        )
        if (is.null(download_df)) {
          return(NULL)
        }
        saveRDS(object = download_df, file = con)
      }
    )


    ### prepare UI ###
    if (enable == TRUE) {
      output$download_buttons_ui <- renderUI(tagList(
        downloadButton(
          outputId = ns("download_current_municipality"),
          label = "Export checked streets in current municipality"
        ),
        downloadButton(
          outputId = ns("download_current_country"),
          label = "Export checked streets in current country"
        ),
        downloadButton(
          outputId = ns("download_current_municipality_with_details_csv"),
          label = "Export checked streets with details in current municipality (csv)"
        ),
        downloadButton(
          outputId = ns("download_current_municipality_with_details_geojson"),
          label = "Export checked streets with details in current municipality (geojson)"
        ),
        downloadButton(
          outputId = ns("download_current_municipality_with_details_rds"),
          label = "Export checked streets with details in current municipality (rds)"
        ),
        downloadButton(
          outputId = ns("download_current_municipality_with_details_rds_sf"),
          label = "Export checked streets with details in current municipality (rds_sf)"
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
                              language = tidywikidatar::tw_get_language(), 
                              connection = NULL) {
  ui <- shiny::fluidPage(
    mod_sn_export_ui("snm_export_ui_1")
  )
  server <- function(input, output, session) {
    mod_sn_export_server(
      id = "snm_export_ui_1",
      gisco_id = gisco_id,
      country = country,
      language = language,
      connection = connection
    )
  }
  shiny::shinyApp(ui, server)
}
