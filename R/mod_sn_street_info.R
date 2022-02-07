#' snm_street_info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sn_street_info_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("street_name_info_box"))
  )
}
    
#' snm_street_info Server Functions
#'
#' @noRd 
mod_sn_street_info_server <- function(id, street_name, gisco_id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    current_lau <- streetnamer::sn_lau_by_nuts %>% 
      dplyr::filter(country_name == gisco_id)
    
    # check if street in database
    details_from_db <- sn_get_street_name_wikidata_id(gisco_id = gisco_id,
                                                      street_name = street_name)
    
    if (is.null(details_from_db)==FALSE&&nrow(details_from_db)==1) {
      guessing <- FALSE
      checked_switch_selected <- as.logical(details_from_db[["checked"]])
      wikidata_id_selected <- details_from_db[["wikidata_id"]]
      gender_selected <- details_from_db[["gender"]]
    } else {
      checked_switch_selected <- FALSE

      # try to guess wikidata id
      if (stringr::str_to_lower(current_lau$CNTR_CODE)=="it") {
        language <- "it"
      }
      guessing <- TRUE
      
      search_df <- stringr::str_remove(string = street_name,
                                       pattern = "^\\S*\\b") %>% 
        stringr::str_squish() %>% 
        tidywikidatar::tw_search(language = language)
      
      if (nrow(search_df)>0) {
        wikidata_id_selected <- search_df[["id"]][[1]]
        gender_id <- tidywikidatar::tw_get_property_same_length(id = wikidata_id_selected,
                                                   p = "P21",
                                                   only_first = TRUE,
                                                   language = language)
        if (is.na(gender_id)) {
          gender_selected <- as.character(NA)
        } else if (gender_id == "Q6581097") {
          gender_selected <- "male"
        } else if (gender_id == "Q6581072") {
          gender_selected <- "female"
        } else if (stringr::str_starts(string = gender_id, pattern = "Q")) {
          gender_selected <- "other"
        }
        
      } else {
        wikidata_id_selected <- as.character(NA)
        gender_selected <- as.character(NA)
      }
    }
    
    ### Store data in database
    
    # TODO Introduce category
    category <- as.character(NA)
    
    shiny::observeEvent(
      eventExpr = input$confirm_action,
      handlerExpr = {
        streetnamer::sn_write_street_name_wikidata_id(gisco_id = gisco_id,
                                                      street_name = street_name,
                                                      wikidata_id = as.character(wikidata_id_selected),
                                                      gender = as.character(gender_selected),
                                                      category = as.character(category),
                                                      checked = as.integer(TRUE),
                                                      overwrite = TRUE
                                                      )
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    
    # shiny::observeEvent(
    #   eventExpr = input$checked_switch,
    #   handlerExpr = {
    #     print("check")
    #     
    #     streetnamer::sn_write_street_name_wikidata_id(gisco_id = gisco_id,
    #                                                   street_name = street_name,
    #                                                   wikidata_id = as.character(wikidata_id),
    #                                                   category = as.character(category),
    #                                                   checked = input$checked_switch,
    #                                                   overwrite = TRUE
    #     )
    #   }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    ### Prepare output
    output$street_name_info_box <- renderUI(tagList(
      shiny::h3(street_name),
      shiny::p(paste0(current_lau$LAU_NAME, ", ", current_lau$CNTR_NAME)),
      shiny::hr(),
      shiny::tags$b(ifelse(guessing, "N.B. Showing first Wikipedia match, review carefully", "")),
      shinyWidgets::radioGroupButtons(
        inputId = "gender_switch",
        selected = gender_selected,
        choices = c("female", "male", "other", "not a person", "NA"),
        individual = TRUE,
        checkIcon = list(yes = icon("ok", lib = "glyphicon")),
       justified = TRUE),
      sn_get_info_box(wikidata_id = wikidata_id_selected),
               shinyWidgets::switchInput(inputId = "wikidata_panel_switch",
                                         label = "Change Wikidata id?",
                                         onLabel = "Yes",
                                         offLabel = "No",
                                         size = "large",
                                         value = FALSE,
                                         labelWidth = "280px",
                                         handleWidth = "80px",
                                         width = "90%"),
               conditionalPanel(condition = "input.wikidata_panel_switch == true",
                                shiny::textInput(inputId = "wikidata_search",
                                                 label = "Search on Wikidata",
                                                 placeholder = "search...",
                                                 value = "placeholder_default_search",
                                                 width = "100%"),
                                DT::DTOutput(outputId = "search_results_dt"),
                                shiny::textInput(inputId = "wikidata_new_id",
                                                 label = "or enter custom Wikidata id",
                                                 width = "100%"),
                                actionButton(
                                  inputId = ns("set_id"),
                                  label = "Set new id!"
                                )
               ),
      shinyWidgets::switchInput(inputId = ns("checked_switch"),
                                label = "Manually checked?",
                                onLabel = "Yes",
                                offLabel = "No",
                                size = "large",
                                value = checked_switch_selected,
                                labelWidth = "280px",
                                handleWidth = "80px",
                                width = "90%"),
      shiny::actionButton(inputId = ns("confirm_action"), label = "Confirm!")
      )
    )
    
  })
}

## To be copied in the UI
# mod_sn_street_info_ui("snm_street_info_ui_1")
    
## To be copied in the server
# mod_sn_street_info_server("snm_street_info_ui_1")


mod_app_sn_street_info <- function(street_name, gisco_id) {
  ui <- fluidPage(
    mod_sn_street_info_ui("snm_street_info_ui_1")
  )
  server <- function(input, output, session) {
    mod_sn_street_info_server(id = "snm_street_info_ui_1",
                              street_name = street_name, 
                              gisco_id = gisco_id)
  }
  shinyApp(ui, server)
}

# mod_app_sn_street_info(street_name = "Belvedere San Francesco", gisco_id = "IT_022205")
