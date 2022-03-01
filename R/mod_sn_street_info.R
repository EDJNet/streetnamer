#' snm_street_info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sn_street_info_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("street_name_info_box"))
  )
}

#' snm_street_info Server Functions
#'
#' @noRd
mod_sn_street_info_server <- function(id,
                                      street_name,
                                      gisco_id,
                                      country,
                                      connection = NULL,
                                      language = tidywikidatar::tw_get_language()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # current_lau <- streetnamer::sn_lau_by_nuts %>%
    #   dplyr::filter(country_name == gisco_id)

    # check if street in database
    details_from_db <- sn_get_street_name_wikidata_id(
      gisco_id = gisco_id,
      street_name = street_name,
      country = country,
      language = language,
      connection = connection
    )

    if (is.null(details_from_db) == FALSE && nrow(details_from_db) == 1) {
      guessing <- FALSE
      checked_switch_selected <- as.logical(details_from_db[["checked"]])
      wikidata_id_selected <- details_from_db[["wikidata_id"]]
      gender_selected <- details_from_db[["gender"]]
    } else {
      checked_switch_selected <- FALSE

      # try to guess wikidata id based on country
      if (country == "IT") {
        search_language <- "it"
      } else {
        search_language <- language
      }
      guessing <- TRUE

      # fix guesser by language

      search_string_v <- stringr::str_remove(
        string = street_name,
        pattern = "^\\S*\\b"
      ) %>%
        stringr::str_squish()

      if (nchar(search_string_v) <= 1) {
        search_string_v <- street_name
      }

      search_df <- tidywikidatar::tw_search(
        search = search_string_v,
        language = search_language,
        cache_connection = connection,
        cache = TRUE
      )


      if (nrow(search_df) > 0) {
        wikidata_id_selected <- search_df[["id"]][[1]]
        gender_id <- tidywikidatar::tw_get_property_same_length(
          id = wikidata_id_selected,
          p = "P21",
          preferred = TRUE,
          only_first = TRUE,
          language = language,
          cache_connection = connection,
          cache = TRUE
        )
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


    gisco_id_v <- gisco_id

    lau_label_v_pre <- sn_lau_by_nuts %>%
      dplyr::filter(.data$gisco_id == gisco_id_v) %>%
      dplyr::pull(.data$lau_label)

    lau_label_v <- dplyr::if_else(condition = length(lau_label_v_pre) == 1,
      true = lau_label_v_pre,
      false = gisco_id
    )
    checked_lv <- details_from_db$checked
    if (is.null(details_from_db$checked)) {
      checked_lv <- FALSE
    } else {
      checked_lv <- as.logical(checked_lv)
    }


    status_v <- dplyr::case_when(
      guessing ~ "Automatic guess",
      checked_lv ~ "Manually checked",
      TRUE ~ "Undetermined"
    )

    #### Search Wikidata box #####


    search_results <- eventReactive(input$wikidata_search, {
      if (is.null(input$wikidata_search)) {
        return(NULL)
      }
      if (input$wikidata_search == "") {
        return(NULL)
      }
      # print(input$wikidata_search)
      tidywikidatar::tw_search(
        search = input$wikidata_search,
        language = search_language,
        cache = TRUE,
        cache_connection = connection
      )
    })


    output$search_results_dt <- DT::renderDT(
      expr = {
        if (is.null(search_results())) {
          return(NULL)
        }
        search_results_df <- search_results()

        if (nrow(search_results_df) == 0) {
          DT::datatable(
            data = tibble::tibble(`No results` = ""),
            options = list(
              dom = "t",
              pageLength = 1
            ),
            rownames = FALSE,
            escape = FALSE,
            selection = "single"
          )
        } else {
          DT::datatable(
            data = search_results_df %>%
              head(5) %>%
              dplyr::mutate(id = glue::glue("<a href='https://www.wikidata.org/wiki/{id}' target=\"_blank\">{id}</a>")),
            options = list(
              dom = "t",
              pageLength = 10
            ),
            rownames = FALSE,
            escape = FALSE,
            selection = "single"
          )
        }
      },
      server = TRUE
    )

    observeEvent(input$search_results_dt_rows_selected,
      {
        shiny::updateTextInput(
          inputId = ns("wikidata_new_id"),
          value = search_results()$id[input$search_results_dt_rows_selected],
          session = session
        )
      },
      ignoreNULL = TRUE
    )


    #### End of search Wikidata box #####


    ## store data when "confirm" is clicked


    shiny::observeEvent(
      eventExpr = input$confirm_action,
      handlerExpr = {
        streetnamer::sn_write_street_name_wikidata_id(
          gisco_id = gisco_id,
          street_name = street_name,
          country = country,
          wikidata_id = as.character(wikidata_id_selected),
          person = as.integer(input$person_switch),
          gender = as.character(input$gender_radio),
          category = as.character(input$category_radio),
          tag = as.character(input$tag_selectize),
          checked = as.integer(TRUE),
          ignore = as.integer(FALSE),
          session = session$token,
          time = Sys.time(),
          append = TRUE,
          connection = connection
        )
      }, ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    ## store data when "set id" is clicked
    
    
    shiny::observeEvent(
      eventExpr = input$set_id,
      handlerExpr = {
        streetnamer::sn_write_street_name_wikidata_id(
          gisco_id = gisco_id,
          street_name = street_name,
          country = country,
          wikidata_id = as.character(wikidata_id_selected),
          person = as.integer(input$person_switch),
          gender = as.character(input$gender_radio),
          category = as.character(input$category_radio),
          tag = as.character(input$tag_selectize),
          checked = as.integer(TRUE),
          ignore = as.integer(FALSE),
          session = session$token,
          time = Sys.time(),
          append = TRUE,
          connection = connection
        )
      }, ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
    

    ### Prepare output
    output$street_name_info_box <- renderUI(tagList(
      shiny::h3(street_name),
      shiny::p(lau_label_v),
      shiny::p("Status: ", shiny::strong(status_v)),
      shiny::hr(),
      shiny::p("Named after:"),
      sn_get_info_box(wikidata_id = wikidata_id_selected),
      shiny::hr(),
      shinyWidgets::switchInput(
        inputId = ns("checked_switch"),
        label = "Manually checked?",
        onLabel = "Yes",
        offLabel = "No",
        size = "large",
        value = checked_switch_selected,
        labelWidth = "280px",
        handleWidth = "80px",
        width = "90%"
      ),
      shiny::tags$b(ifelse(guessing, "N.B. Showing first Wikipedia match, review carefully", "")),
      shinyWidgets::switchInput(
        inputId = ns("person_switch"),
        label = "Is it a person?",
        onLabel = "Yes",
        offLabel = "No",
        size = "large",
        value = dplyr::if_else(is.na(gender_selected), FALSE, TRUE),
        labelWidth = "280px",
        handleWidth = "80px",
        width = "90%"
      ),
      shiny::conditionalPanel(
        condition = "input.person_switch == true",
        ns = ns,
        shiny::p("Select gender:"),
        shinyWidgets::radioGroupButtons(
          inputId = ns("gender_switch"),
          selected = gender_selected,
          choices = c("female", "male", "other", "uncertain"),
          individual = TRUE,
          checkIcon = list(yes = icon("ok", lib = "glyphicon")),
          justified = TRUE,
          width = "98%"
        ),
        shiny::p("Select scope:"),
        shinyWidgets::radioGroupButtons(
          inputId = ns("category_radio"),
          selected = "other",
          choices = c(
            "",
            "religion",
            "military",
            "politics",
            "culture",
            "other",
            "NA"
          ),
          individual = TRUE,
          checkIcon = list(yes = icon("ok", lib = "glyphicon")),
          justified = TRUE,
          width = "98%"
        ),
        shiny::selectizeInput(
          inputId = ns("tag_selectize"),
          label = "Add a tag",
          choices = c(
            "",
            "colonialism",
            "slave trade",
            "partisan",
            "communism",
            "fascism",
            "sport"
          ),
          options = list(create = TRUE)
        )
      ),
      conditionalPanel(
        condition = "input.person_switch == false",
        ns = ns,
        # shinyWidgets::radioGroupButtons(
        #   inputId = ns("category_radio"),
        #   selected = "other",
        #   choices = c("place", "event", "other"),
        #   individual = TRUE,
        #   checkIcon = list(yes = icon("ok", lib = "glyphicon")),
        #   justified = TRUE
        # ),
        shiny::selectizeInput(
          inputId = ns("selectize_tag"),
          label = "Add a tag",
          choices = c(
            "",
            "place",
            "event",
            "profession",
            "plant",
            "animal"
          ),
          options = list(create = TRUE)
        )
      ),
      shinyWidgets::switchInput(
        inputId = ns("wikidata_panel_switch"),
        label = "Change Wikidata id?",
        onLabel = "Yes",
        offLabel = "No",
        size = "large",
        value = FALSE,
        labelWidth = "280px",
        handleWidth = "80px",
        width = "90%"
      ),
      conditionalPanel(
        condition = "input.wikidata_panel_switch == true",
        ns = ns,
        shiny::textInput(
          inputId = ns("wikidata_search"),
          label = "Search on Wikidata",
          placeholder = "search...",
          value = street_name,
          width = "100%"
        ),
        DT::DTOutput(outputId = ns("search_results_dt")),
        shiny::textInput(
          inputId = ns("wikidata_new_id"),
          label = "or enter custom Wikidata id",
          width = "100%"
        ),
        actionButton(
          inputId = ns("set_id"),
          label = "Set new id!"
        )
      ),
      shiny::actionButton(
        inputId = ns("confirm_action"),
        label = "Confirm!"
      )
    ))

    ## Return
    wikidata_id_selected
  })
}

## To be copied in the UI
# mod_sn_street_info_ui("snm_street_info_ui_1")

## To be copied in the server
# mod_sn_street_info_server("snm_street_info_ui_1")


#' A minimal shiny app used for categorising streets
#'
#' @param street_name
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
#'   mod_sn_street_info_app(
#'     street_name = "Belvedere San Francesco",
#'     gisco_id = "IT_022205",
#'     country = "IT"
#'   )
#' }
mod_sn_street_info_app <- function(street_name,
                                   gisco_id,
                                   country,
                                   connection = NULL,
                                   language = tidywikidatar::tw_get_language()) {
  ui <- shiny::fluidPage(
    mod_sn_street_info_ui("snm_street_info_ui_1")
  )
  server <- function(input, output, session) {
    mod_sn_street_info_server(
      id = "snm_street_info_ui_1",
      street_name = street_name,
      gisco_id = gisco_id,
      country = country,
      connection = connection,
      language = language
    )
  }
  shiny::shinyApp(ui, server)
}
