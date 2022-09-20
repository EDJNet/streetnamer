#' sn_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sn_import_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("upload_buttons_ui")),
    shiny::uiOutput(ns("upload_confirmed_ui")),
    shiny::uiOutput(ns("preview_ui"))
  )
}

#' sn_import Server Functions
#'
#' @noRd
mod_sn_import_server <- function(id,
                                 language = tidywikidatar::tw_get_language(),
                                 connection = NULL,
                                 enable = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    if (is.null(enable)) {
      return(NULL)
    } 
    
    ### prepare UI ###
    if (enable == TRUE) {
      output$upload_buttons_ui <- renderUI(tagList(
        shiny::fileInput(inputId = ns("upload_file"),
                         label = "Upload pre-processed data",
                         accept = ".csv",
                         multiple = FALSE
        )
      )
      )
      
      manually_fixed_df <- reactive({
        file_df <- input$upload_file
        
        validate(need(fs::path_ext(file_df$datapath) == "csv", "Please upload a csv file"))
        
        readr::read_csv(file = file_df$datapath,
                        col_types = 
                          readr::cols(
                            gisco_id = readr::col_character(),
                            street_name = readr::col_character(),
                            country = readr::col_character(),
                            named_after_id = readr::col_character(),
                            person = readr::col_double(),
                            gender = readr::col_character(),
                            category = readr::col_character(),
                            tag = readr::col_character(),
                            checked = readr::col_double(),
                            ignore = readr::col_double(),
                            named_after_n = readr::col_double(),
                            named_after_custom_label = readr::col_character(),
                            session = readr::col_character(),
                            time = readr::col_datetime(format = "")
                          ))
      })
      
      output$preview_ui <- renderUI(
        {
          
          if (nrow(manually_fixed_df())>0) {
            confirm_button_tag <- tagList(shiny::actionButton(inputId = ns("confirm_upload"),
                                                              label = "Confirm upload"),
                                          shiny::p("Preview of first rows of files to be uploaded:"))
          } else {
            confirm_button_tag <- tagList()
          }
          
          tagList(
            confirm_button_tag,
            shiny::renderTable(head(manually_fixed_df()))
          )
        })
    } else {
      output$upload_buttons_ui <- renderUI(tagList())
    }
    
    observeEvent(input$confirm_upload, {
      
      sn_write_street_named_after_id(df_to_write = manually_fixed_df(),
                                     connection = connection)
      
      
      
      output$upload_confirmed_ui <- renderUI(tagList(htmltools::strong(stringr::str_c("Data on the following municipalities stored in database: ", stringr::str_c(unique(manually_fixed_df()[["gisco_id"]]), collapse = "; ", sep = "; ")))))
    })
    
    output$upload_confirmed_ui <- renderUI(tagList())
    
  })
}

## To be copied in the UI
# mod_sn_import_ui("sn_import_ui_1")

## To be copied in the server
# mod_sn_import_server("sn_import_ui_1")



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
#'   mod_sn_import_app(
#'   )
#' }
mod_sn_import_app <- function(language, 
                              connection = NULL,
                              enable = TRUE) {
  ui <- shiny::fluidPage(
    mod_sn_import_ui("snm_import_ui_1")
  )
  server <- function(input, output, session) {
    mod_sn_import_server(
      id = "snm_import_ui_1",
      language = language,
      connection = connection,
      enable = enable
    )
  }
  shiny::shinyApp(ui, server)
}
