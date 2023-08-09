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
    shiny::selectInput(
      inputId = ns("language_selector"),
      label = "Language to be used for labels",
      choices = setNames(sn_available_languages$language_code, sn_available_languages$language_name),
      selected = tidywikidatar::tw_get_language(),
      multiple = FALSE,
      selectize = TRUE
    ),
    shiny::uiOutput(ns("download_buttons_ui"))
  )
}

#' sn_export Server Functions
#'
#' @noRd
mod_sn_export_server <- function(id,
                                 country,
                                 gisco_id,
                                 include_checked_elsewhere_in_country = FALSE,
                                 streets_sf = NULL,
                                 lau_year = 2020,
                                 language = tidywikidatar::tw_get_language(),
                                 available_languages = streetnamer::sn_available_languages,
                                 connection = NULL,
                                 enable = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    #### waiter ####
    w <- waiter::Waiter$new(
      id = ns("download_buttons_ui"),
      html = waiter::spin_loaders(id = 15, color = "#FF5454"),
      fadeout = TRUE,
      color = "#fef7ed"
    )

    #### prepare export data with reactive ####

    export_r <- reactive({
      if (is.null(input$export_type)) {
        return(NULL)
      } else if (input$export_type == "") {
        return(NULL)
      }

      w$show()

      if (input$export_type == "Export checked streets in current country") {
        export_df <- sn_get_street_named_after_id(
          country = country,
          language = input$language_selector,
          connection = connection,
          remove_ignored = FALSE
        )
        return(export_df)
      } else if (input$export_type == "Export checked streets in current municipality") {
        export_df <- sn_get_street_named_after_id(
          gisco_id = gisco_id,
          country = country,
          language = input$language_selector,
          connection = connection,
          remove_ignored = FALSE
        )
        return(export_df)
      } else if (input$export_type == "Export checked streets in current municipality with basic details (csv)") {
        pre_export_df <- sn_get_street_named_after_id(
          gisco_id = gisco_id,
          country = country,
          language = input$language_selector,
          connection = connection,
          remove_ignored = FALSE
        )
        tw_df <- tidywikidatar::tw_get(id = pre_export_df[["named_after_id"]], 
                                       language = language,
                                       cache = TRUE, 
                                       overwrite_cache = FALSE, 
                                       cache_connection = connection,
                                       disconnect_db = FALSE,
                                       wait = 0) %>% 
          dplyr::filter(is.na(id)==FALSE)
        
        export_df <- pre_export_df %>% 
          dplyr::mutate(label = tidywikidatar::tw_get_label(id = named_after_id,
                                                            language = language,
                                                            id_df = tw_df,
                                                            cache = TRUE,
                                                            overwrite_cache = FALSE,
                                                            cache_connection = connection,
                                                            disconnect_db = FALSE),
                        description = tidywikidatar::tw_get_description(id = named_after_id,
                                                                        language = language,
                                                                        id_df = tw_df,
                                                                        cache = TRUE,
                                                                        overwrite_cache = FALSE,
                                                                        cache_connection = connection,
                                                                        disconnect_db = FALSE),
                        instance_of_label = tidywikidatar::tw_get_p1(id = named_after_id,
                                                                     p = "P31",
                                                                     language = language,
                                                                     id_df = tw_df,
                                                                     cache = TRUE,
                                                                     overwrite_cache = FALSE,
                                                                     cache_connection = connection,
                                                                     disconnect_db = FALSE) %>% 
                          tidywikidatar::tw_get_label(language = language,
                                                      id_df = tw_df,
                                                      cache = TRUE,
                                                      overwrite_cache = FALSE,
                                                      cache_connection = connection,
                                                      disconnect_db = FALSE),
                        date_of_birth = tidywikidatar::tw_get_p1(id = named_after_id,
                                                                 p = "P569",
                                                                 language = language,
                                                                 id_df = tw_df,
                                                                 cache = TRUE,
                                                                 overwrite_cache = FALSE,
                                                                 cache_connection = connection,
                                                                 disconnect_db = FALSE),
                        date_of_death = tidywikidatar::tw_get_p1(id = named_after_id,
                                                                 p = "P570",
                                                                 language = language,
                                                                 id_df = tw_df,
                                                                 cache = TRUE,
                                                                 overwrite_cache = FALSE,
                                                                 cache_connection = connection,
                                                                 disconnect_db = FALSE))
        return(export_df)
      } else if (input$export_type == "Export checked streets with details in current municipality (csv)") {
        export_df <- sn_export(
          gisco_id = gisco_id,
          country = country,
          export_format = "csv",
          include_checked_elsewhere_in_country = include_checked_elsewhere_in_country,
          streets_sf = streets_sf,
          unlist = TRUE,
          lau_year = lau_year,
          write_file = FALSE,
          language = input$language_selector,
          connection = connection,
          cache = TRUE
        )
        return(export_df)
      } else if (input$export_type == "Export checked streets with details in current municipality (geojson)") {
        export_sf <- sn_export(
          gisco_id = gisco_id,
          country = country,
          export_format = "geojson",
          include_checked_elsewhere_in_country = include_checked_elsewhere_in_country,
          streets_sf = streets_sf,
          unlist = TRUE,
          lau_year = lau_year,
          write_file = FALSE,
          language = input$language_selector,
          connection = connection,
          cache = TRUE
        )
        return(export_sf)
      } else if (input$export_type == "Export checked streets with details in current municipality (rds_sf)") {
        export_sf <- sn_export(
          gisco_id = gisco_id,
          country = country,
          export_format = "rds_sf",
          include_checked_elsewhere_in_country = include_checked_elsewhere_in_country,
          streets_sf = streets_sf,
          unlist = TRUE,
          lau_year = lau_year,
          write_file = FALSE,
          language = input$language_selector,
          connection = connection,
          cache = TRUE
        )
        return(export_sf)
      }
    })

    #### prepare reactive UI with custom download button ####

    observeEvent(
      export_r(),
      output$download_buttons_ui <- renderUI({
        if (is.null(export_r())) {
          return(NULL)
        }

        if (is.data.frame(export_r()) == FALSE) {
          return(NULL)
        }


        shiny::fluidRow(
          column(
            width = 12,
            shiny::tagList(
              shiny::selectInput(
                inputId = ns("export_type"),
                label = "Select type and format of data you wish to export",
                choices = c(
                  "",
                  "Export checked streets in current municipality",
                  "Export checked streets in current country",
                  "Export checked streets in current municipality with basic details (csv)",
                  "Export checked streets with details in current municipality (csv)",
                  "Export checked streets with details in current municipality (geojson)",
                  "Export checked streets with details in current municipality (rds)",
                  "Export checked streets with details in current municipality (rds_sf)"
                ),
                multiple = FALSE,
                selectize = TRUE,
                selected = input$export_type,
                width = "80%"
              ),
              downloadButton(
                outputId = ns("export"),
                label = stringr::str_replace(
                  string = input$export_type,
                  pattern = "Export ",
                  replacement = "Download "
                )
              )
            )
          )
        )
      })
    )

    #### Actual download ####

    output$export <- downloadHandler(
      filename = function() {
        if (input$export_type == "Export checked streets with details in current municipality (rds)" | input$export_type == "Export checked streets with details in current municipality (rds_sf)") {
          file_format <- ".rds"
        } else if (input$export_type == "Export checked streets with details in current municipality (geojson)") {
          file_format <- ".geojson"
        } else {
          file_format <- ".csv"
        }

        if (input$export_type == "Export checked streets in current country") {
          country_v <- country
          country_name <- sn_lau_by_nuts %>%
            dplyr::distinct(.data$country, .data$country_name) %>%
            dplyr::filter(.data$country == country_v) %>%
            dplyr::pull(.data$country_name)

          file_name <- stringr::str_c(
            stringr::str_to_lower(iconv(
              x = country_name,
              to = "ASCII//TRANSLIT"
            )),
            "-",
            as.integer(Sys.time()),
            file_format
          )
        } else {
          gisco_id_v <- gisco_id

          lau_label <- sn_lau_by_nuts %>%
            dplyr::filter(.data$gisco_id == gisco_id_v) %>%
            dplyr::pull(.data$lau_label)

          if (input$export_type == "Export checked streets in current municipality") {
            file_name <- stringr::str_c(
              gisco_id,
              "-",
              stringr::str_to_lower(iconv(
                x = lau_label,
                to = "ASCII//TRANSLIT"
              )),
              "-",
              as.integer(Sys.time()),
              file_format
            )
          } else {
            file_name <- stringr::str_c(
              gisco_id,
              "-",
              stringr::str_to_lower(iconv(
                x = lau_label,
                to = "ASCII//TRANSLIT"
              )),
              "-with_details-",
              as.integer(Sys.time()),
              file_format
            )
          }
        }
        file_name
      },
      content = function(con) {
        if (input$export_type == "Export checked streets with details in current municipality (rds)") {
          file_format <- ".rds"
          saveRDS(export_r(), con)
        } else if (input$export_type == "Export checked streets with details in current municipality (rds)") {
          saveRDS(export_r(), con)
        } else if (input$export_type == "Export checked streets with details in current municipality (geojson)") {
          sf::st_write(
            obj = export_r(),
            dsn = con
          )
        } else {
          readr::write_csv(export_r(), con)
        }
      }
    )

    #### prepare UI ####
    if (enable == TRUE) {
      output$download_buttons_ui <- renderUI(tagList(
        shiny::selectInput(
          inputId = ns("export_type"),
          label = "Select type and format of data you wish to export",
          choices = c(
            "",
            "Export checked streets in current municipality",
            "Export checked streets in current country",
            "Export checked streets in current municipality with basic details (csv)",
            "Export checked streets with details in current municipality (csv)",
            "Export checked streets with details in current municipality (geojson)",
            "Export checked streets with details in current municipality (rds)",
            "Export checked streets with details in current municipality (rds_sf)"
          ),
          multiple = FALSE,
          selectize = TRUE,
          width = "80%"
        ),
        shiny::p(shiny::tags$i("N.B. Exporting datasets with details from Wikidata can take just a few seconds if all data has been previously cached, but may take many minutes if the data are being exported for the first time."))
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
