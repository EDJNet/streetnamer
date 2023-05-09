#' mod_show_summary_stats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sn_show_basic_municipality_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("summary_stats_ui"))
  )
}

#' mod_show_summary_stats Server Functions
#'
#' @noRd
mod_sn_show_basic_municipality_server <- function(id,
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
    
    w <- waiter::Waiter$new(
      id = ns("summary_stats_ui"),
      html = waiter::spin_loaders(id = 15, color = "#FF5454"),
      fadeout = TRUE,
      color = "#fef7ed"
    )
    
    if (is.null(gisco_label)) {
      gisco_label <- ""
    }
    

    
    output$summary_stats_ui <- shiny::renderUI({
      w$show()
      
      country_code <- sn_standard_country(country = country, type = "code")
      
      basic_df <- sn_export_basic(gisco_id = gisco_id,
                                  streets_sf = streets_sf,
                                  include_checked_elsewhere_in_country = TRUE,
                                  connection = connection,
                                  disconnect_db = FALSE)
      
      output$export <- downloadHandler(
        filename = function() {
          paste0(gisco_id, ".csv")
        },
        content = function(file) {
          readr::write_csv(basic_df, file)
        }
      )
      
      output$table <- renderTable(basic_df %>% 
                                    dplyr::filter(person == 1) %>% 
                                    dplyr::group_by(gender, category) %>% 
                                    dplyr::tally() %>% 
                                    dplyr::ungroup() %>% 
                                    tidyr::pivot_wider(values_from = n, names_from = gender) %>% 
                                    dplyr::mutate(category = factor(category,
                                                                    levels = c("politics",
                                                                               "culture",
                                                                               "religion",
                                                                               "military", 
                                                                               "other"))) %>% 
                                    dplyr::arrange(category) %>% 
                                    dplyr::add_row(tibble::tibble(category = "total", 
                                                                  female = sum(basic_df$gender=="female", na.rm = TRUE),
                                                                  male = sum(basic_df$gender=="male", na.rm = TRUE),
                                                                  other = sum(basic_df$gender=="other", na.rm = TRUE))))
      
      
      summary_taglist <- shiny::tagList(
        tableOutput(ns("table")),
        downloadButton(
          outputId = ns("export"),
          label = "Download basic data for this municipality"
        )
      )
      
      summary_taglist
    })
    
    basic_df_r <- shiny::reactive({
      basic_df
    })
    
    shiny::reactive(basic_df_r())
  })
}

## To be copied in the UI
# mod_sn_show_basic_municipality_ui("mod_sn_show_basic_municipality_1")

## To be copied in the server
# mod_sn_show_basic_municipality_server("mod_sn_show_basic_municipality_1")


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
#'   mod_sn_show_basic_municipality_app(
#'     gisco_id = "IT_022205",
#'     gisco_label = "Trento",
#'     country = "IT"
#'   )
#' }
mod_sn_show_basic_municipality_app <- function(gisco_id,
                                          gisco_label = NULL,
                                          country = NULL,
                                          connection = NULL,
                                          language = tidywikidatar::tw_get_language()) {
  ui <- shiny::fluidPage(
    waiter::useWaiter(),
    mod_sn_show_basic_municipality_ui("mod_sn_show_basic_municipality_1")
  )
  server <- function(input, output, session) {
    mod_sn_show_basic_municipality_server(
      id = "mod_sn_show_basic_municipality_1",
      gisco_id = gisco_id,
      gisco_label = gisco_label,
      country = country,
      connection = connection,
      language = language
    )
  }
  shiny::shinyApp(ui, server)
}
