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
                                      wikidata_id = NULL,
                                      country = NULL,
                                      connection = NULL,
                                      language = tidywikidatar::tw_get_language()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # current_lau <- streetnamer::sn_lau_by_nuts %>%
    #   dplyr::filter(country_name == gisco_id)


    if (is.null(country)) {
      country <- stringr::str_extract(string = gisco_id, pattern = "[[:alnum:]]{2}") %>%
        stringr::str_to_upper()
    }
    country_code <- sn_standard_country(country = country, type = "code")
    country_name <- sn_standard_country(country = country, type = "name")

    checked_lv <- NULL

    if (is.null(wikidata_id) == FALSE) {
      if (is.na(wikidata_id)) {
        wikidata_id_selected <- as.character(NA)
        gender_selected <- as.character(NA)
        checked_switch_selected <- TRUE
        guessing <- FALSE
      } else if (wikidata_id == "drop") {
        wikidata_id_selected <- as.character(NA)
        gender_selected <- as.character(NA)
        checked_switch_selected <- TRUE
        guessing <- FALSE
      } else {
        guessing <- FALSE
        checked_switch_selected <- TRUE
        wikidata_id_selected <- wikidata_id
        gender_selected <- sn_get_gender_label(
          wikidata_id = wikidata_id_selected,
          language = language,
          cache_connection = connection,
          cache = TRUE
        )
      }
    } else {
      # check if street in database
      details_from_db <- sn_get_street_name_wikidata_id(
        gisco_id = gisco_id,
        street_name = street_name,
        country = country_code,
        language = language,
        connection = connection
      )
      if (is.null(details_from_db) == FALSE && nrow(details_from_db) == 1) {
        guessing <- FALSE
        checked_switch_selected <- as.logical(details_from_db[["checked"]])
        wikidata_id_selected <- details_from_db[["wikidata_id"]]
        gender_selected <- details_from_db[["gender"]]
        checked_lv <- details_from_db$checked
      } else {
        guessing <- TRUE

        checked_switch_selected <- FALSE

        # try to guess wikidata id based on country
        search_language <- streetnamer::sn_language_defaults_by_country %>%
          dplyr::filter(.data$country == country_name) %>%
          dplyr::pull(.data$language_code)

        if (length(search_language) == 0) {
          search_language <- language
        } else if (length(search_language) > 1) {
          search_language <- search_language[1]
        }


        # fix guesser by language

        search_string_v <- sn_clean_street_name(
          street_name = street_name,
          country = country_name
        )

        search_df <- tidywikidatar::tw_search(
          search = search_string_v,
          language = search_language,
          cache_connection = connection,
          cache = TRUE
        )


        if (nrow(search_df) > 0) {
          wikidata_id_selected <- search_df[["id"]][[1]]
          gender_selected <- sn_get_gender_label(
            wikidata_id = wikidata_id_selected,
            language = language,
            cache_connection = connection,
            cache = TRUE
          )
        } else {
          wikidata_id_selected <- as.character(NA)
          gender_selected <- as.character(NA)
        }
      }
    }

    ### Store data in database

    # TODO Introduce category
    category <- as.character(NA)


    gisco_id_v <- gisco_id

    lau_label_v_pre <- sn_lau_by_nuts %>%
      dplyr::filter(.data$gisco_id == gisco_id_v) %>%
      dplyr::pull(.data$lau_label)

    lau_label_v <- dplyr::if_else(condition = length(lau_label_v_pre) == 1,
      true = lau_label_v_pre,
      false = gisco_id
    )


    if (is.null(checked_lv)) {
      checked_lv <- FALSE
    } else {
      checked_lv <- as.logical(checked_lv)
    }


    status_v <- dplyr::case_when(
      guessing ~ "Automatic guess",
      checked_lv ~ "Manually checked",
      TRUE ~ "Undetermined"
    )

    ### Prepare output
    output$street_name_info_box <- shiny::renderUI(
      shiny::tagList(
        shiny::h3(street_name),
        shiny::p(lau_label_v),
        shiny::p("Status: ", shiny::strong(status_v)),
        shiny::hr(),
        shiny::p("Named after:"),
        streetnamer::sn_get_info_box(
          wikidata_id = wikidata_id_selected,
          language = language,
          connection = connection
        ),
        shiny::hr(),
        # shinyWidgets::switchInput(
        #   inputId = ns("checked_switch"),
        #   label = "Manually checked?",
        #   onLabel = "Yes",
        #   offLabel = "No",
        #   size = "large",
        #   value = checked_switch_selected,
        #   labelWidth = "280px",
        #   handleWidth = "80px",
        #   width = "90%"
        # ),
        shiny::tags$b(ifelse(guessing,
          "N.B. Showing first Wikipedia match, review carefully",
          ""
        )),
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
            selected = character(0),
            choices = c(
              "religion",
              "military",
              "politics",
              "culture",
              "other"
            ),
            individual = TRUE,
            checkIcon = list(yes = icon("ok", lib = "glyphicon")),
            justified = TRUE,
            width = "98%"
          ),
          shiny::selectizeInput(
            inputId = ns("tag_selectize_person"),
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
          shiny::selectizeInput(
            inputId = ns("tag_selectize_not_person"),
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
          inputId = ns("dedicated_to_n_switch"),
          label = "Is it dedicated to more than one entity?",
          onLabel = "Yes",
          offLabel = "No",
          size = "large",
          value = FALSE,
          labelWidth = "280px",
          handleWidth = "80px",
          width = "90%"
        ),
        shiny::conditionalPanel(
          condition = "input.dedicated_to_n_switch == true",
          ns = ns,
          shiny::p("Input number of entities this street is dedicated to:"),
          shiny::numericInput(
            inputId = ns("dedicated_to_n"),
            label = NULL, # "Input number of entities this street is dedicated to",
            value = 1,
            min = 1,
            max = 100,
            step = 1
          )
        )
        # ,
        # shinyWidgets::switchInput(
        #   inputId = ns("exists_on_wikidata_switch"),
        #   label = "Does it exists on Wikidata?",
        #   onLabel = "Yes",
        #   offLabel = "No",
        #   size = "large",
        #   value = dplyr::if_else(is.na(wikidata_id_selected), FALSE, TRUE),
        #   labelWidth = "280px",
        #   handleWidth = "80px",
        #   width = "90%"
        # )
      )
    )

    ## Return
    selected_df_r <- shiny::reactive({
      if (as.integer(input$person_switch) == 1) {
        tag_v <- input$tag_selectize_person
      } else {
        tag_v <- input$tag_selectize_not_person
      }

      sn_write_street_name_wikidata_id(
        gisco_id = gisco_id,
        street_name = street_name,
        country = country,
        wikidata_id = as.character(wikidata_id_selected),
        person = as.integer(input$person_switch),
        gender = gender_selected,
        category = as.character(input$category_radio),
        tag = tag_v,
        checked = as.integer(TRUE),
        ignore = as.integer(FALSE),
        dedicated_to_n = as.integer(input$dedicated_to_n),
        session = session$token,
        append = TRUE,
        connection = connection,
        return_df_only = TRUE
      )
    })

    shiny::reactive(selected_df_r())
  })
}

## To be copied in the UI
# mod_sn_street_info_ui("snm_street_info_ui_1")

## To be copied in the server
# mod_sn_street_info_server("snm_street_info_ui_1")


#' A minimal shiny app used for categorising streets
#'
#' @param street_name A character string. Conceptually, the name of a street
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
#'   mod_sn_street_info_app(
#'     street_name = "Belvedere San Francesco",
#'     gisco_id = "IT_022205",
#'     country = "IT"
#'   )
#' }
mod_sn_street_info_app <- function(street_name,
                                   gisco_id,
                                   country = NULL,
                                   connection = NULL,
                                   language = tidywikidatar::tw_get_language()) {
  ui <- shiny::fluidPage(
    mod_sn_street_info_ui("snm_street_info_ui_1"),
    shiny::tableOutput(outputId = "selected_df_ui")
  )
  server <- function(input, output, session) {
    selected_df_r <- mod_sn_street_info_server(
      id = "snm_street_info_ui_1",
      street_name = street_name,
      gisco_id = gisco_id,
      country = country,
      connection = connection,
      language = language
    )

    output$selected_df_ui <- shiny::renderTable({
      selected_df_r()
    })
  }
  shiny::shinyApp(ui, server)
}
