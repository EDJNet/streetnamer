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
    shiny::uiOutput(ns("summary_stats_info_box"))
  )
}

#' mod_show_summary_stats Server Functions
#'
#' @noRd
mod_sn_show_summary_stats_server <- function(id,
                                             gisco_id,
                                             country = NULL,
                                             streets_sf = NULL,
                                             street_names_df = NULL,
                                             connection = NULL,
                                             language = tidywikidatar::tw_get_language(),
                                             search_language = NULL,
                                             disconnect_db = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    core_df <- sn_get_city_combo(
      gisco_id = gisco_id,
      country = country,
      streets_sf = streets_sf,
      street_names_df = street_names_df,
      connection = connection,
      language = language,
      search_language = search_language,
      disconnect_db = disconnect_db
    )

    output$street_name_info_box <- shiny::renderUI(
      shiny::tagList(shiny::h2("Summary stats"))
    )

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
#'     country = "IT"
#'   )
#' }
mod_sn_show_summary_stats_app <- function(gisco_id,
                                          country = NULL,
                                          connection = NULL,
                                          language = tidywikidatar::tw_get_language()) {
  ui <- shiny::fluidPage(
    mod_sn_street_info_ui("mod_sn_show_summary_stats_1")
  )
  server <- function(input, output, session) {
    mod_sn_show_summary_stats_server(
      id = "mod_sn_show_summary_stats_1",
      gisco_id = gisco_id,
      country = country,
      connection = connection,
      language = language
    )
  }
  shiny::shinyApp(ui, server)
}
