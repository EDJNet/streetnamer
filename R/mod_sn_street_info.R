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
                                      category_choices = c(
                                        "politics",
                                        "culture",
                                        "religion",
                                        "military",
                                        "other"
                                      ),
                                      named_after_id = NULL,
                                      country = NULL,
                                      connection = NULL,
                                      language = tidywikidatar::tw_get_language()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # current_lau <- streetnamer::sn_lau_by_nuts %>%
    #   dplyr::filter(country_name == gisco_id)

    tictoc::tic(msg = "Getting tentative named_after_id")

    if (is.null(country)) {
      country <- stringr::str_extract(string = gisco_id, pattern = "[[:alnum:]]{2}") %>%
        stringr::str_to_upper()
    }
    country_code <- sn_standard_country(country = country, type = "code")
    country_name <- sn_standard_country(country = country, type = "name")

    checked_lv <- NULL
    
    current_db_connection <- tidywikidatar::tw_connect_to_cache(
      connection = connection,
      language = language,
      cache = TRUE
    )

    if (is.null(named_after_id) == FALSE) {
      if (is.na(named_after_id)) {
        named_after_id_selected <- as.character(NA)
        gender_selected <- as.character(NA)
        checked_switch_selected <- TRUE
        guessing <- FALSE
      } else if (named_after_id == "drop") {
        named_after_id_selected <- as.character(NA)
        gender_selected <- as.character(NA)
        checked_switch_selected <- TRUE
        guessing <- FALSE
      } else {
        guessing <- FALSE
        checked_switch_selected <- TRUE
        named_after_id_selected <- named_after_id
        gender_selected <- sn_get_gender_label(
          named_after_id = named_after_id_selected,
          language = language,
          cache_connection = current_db_connection,
          cache = TRUE,
          disconnect_db = FALSE
        )
      }
    } else {
      # check if street in database
      details_from_db <- sn_get_street_named_after_id(
        gisco_id = gisco_id,
        street_name = street_name,
        country = country_code,
        language = language,
        connection = current_db_connection,
        disconnect_db = FALSE
      )

      if (is.null(details_from_db) == FALSE && nrow(details_from_db) == 1) {
        guessing <- FALSE
        checked_switch_selected <- as.logical(details_from_db[["checked"]])
        named_after_id_selected <- details_from_db[["named_after_id"]]
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

        if (country_code == "BE") {
          search_string_v <- sn_get_clean_street_name_bilingual_df(
            gisco_id = gisco_id,
            street_names_df = tibble::tibble(name = street_name),
            languages = "french-flemish"
          ) %>%
            dplyr::pull(name_clean)
        } else if (country_code == "IT")  {
          search_string_v <- sn_get_clean_street_name_bilingual_df(
            gisco_id = gisco_id,
            street_names_df = tibble::tibble(name = street_name)
          ) %>%
            dplyr::pull(name_clean)
        } else {
          search_string_v <- sn_clean_street_name(
            street_name = street_name,
            country = country_name
          )
        }

        current_street_df <- tibble::tibble(
          name = street_name,
          name_clean = search_string_v
        )

        if (country_code %in% sn_countries_with_streets_as_qid) {
          search_df <- sn_search_named_after(
            gisco_id = gisco_id,
            search_language = search_language,
            response_language = language,
            check_named_after_original = TRUE,
            check_named_after_original_n = 1,
            check_named_after = FALSE,
            drop_if_street = TRUE,
            drop_if_disambiguation_page = TRUE,
            cache = TRUE,
            connection = current_db_connection,
            disconnect_db = FALSE,
            street_names_df = current_street_df
          ) %>%
            dplyr::select(id = named_after_id)
        } else {
          search_df <- sn_search_named_after(
            gisco_id = gisco_id,
            search_language = search_language,
            response_language = language,
            check_named_after_original = FALSE,
            check_named_after_original_n = 1,
            check_named_after = FALSE,
            drop_if_street = TRUE,
            drop_if_disambiguation_page = TRUE,
            cache = TRUE,
            connection = current_db_connection,
            disconnect_db = FALSE,
            street_names_df = current_street_df
          ) %>%
            dplyr::select(id = named_after_id)
        }


        if (nrow(search_df) > 0) {
          named_after_id_selected <- search_df[["id"]][[1]]
          gender_selected <- sn_get_gender_label(
            named_after_id = named_after_id_selected,
            language = language,
            cache_connection = current_db_connection,
            cache = TRUE,
            disconnect_db = FALSE
          )
        } else {
          named_after_id_selected <- as.character(NA)
          gender_selected <- as.character(NA)
        }
      }

    }

    tictoc::toc()

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

    tictoc::tic(msg = "Get details based on id")

    ### Prepare output
    output$street_name_info_box <- shiny::renderUI(
      shiny::tagList(
        shiny::h3(street_name),
        shiny::p(lau_label_v),
        shiny::p("Status: ", shiny::strong(status_v)),
        shiny::hr(),
        shiny::p("Named after:"),
        sn_get_info_box(
          # opening a new connection here, as previous got dropped somehow
          named_after_id = named_after_id_selected,
          language = language,
          connection = connection, 
          disconnect_db = TRUE
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
          shiny::p("Select category:"),
          shinyWidgets::radioGroupButtons(
            inputId = ns("category_radio"),
            selected = character(0),
            choices = category_choices,
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
          inputId = ns("named_after_n_switch"),
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
          condition = "input.named_after_n_switch == true",
          ns = ns,
          shiny::p("Input number of entities this street is dedicated to:"),
          shiny::numericInput(
            inputId = ns("named_after_n"),
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
        #   value = dplyr::if_else(is.na(named_after_id_selected), FALSE, TRUE),
        #   labelWidth = "280px",
        #   handleWidth = "80px",
        #   width = "90%"
        # )
      )
    )

    tictoc::toc() # Get details based on id

    ## Return
    selected_df_r <- shiny::reactive({
      if (length(input$person_switch) == 0) {
        tag_v <- ""
      } else if (as.integer(input$person_switch) == 1) {
        tag_v <- input$tag_selectize_person
      } else {
        tag_v <- input$tag_selectize_not_person
      }

      if (length(input$category_radio) == 0) {
        category_v <- ""
      } else {
        category_v <- input$category_radio
      }

      if (length(input$gender_switch) == 0) {
        gender_selected_v <- as.character(NA)
      } else {
        gender_selected_v <- input$gender_switch
      }

      sn_write_street_named_after_id(
        gisco_id = gisco_id,
        street_name = street_name,
        country = country,
        named_after_id = as.character(named_after_id_selected),
        person = as.integer(input$person_switch),
        gender = gender_selected_v,
        category = as.character(category_v),
        tag = as.character(tag_v),
        checked = as.integer(TRUE),
        ignore = as.integer(FALSE),
        named_after_n = as.integer(input$named_after_n),
        session = session$token,
        append = TRUE,
        connection = current_db_connection,
        disconnect_db = FALSE,
        return_df_only = TRUE
      )
    })
    
    tidywikidatar::tw_disconnect_from_cache(
      cache = TRUE,
      cache_connection = current_db_connection,
      disconnect_db = TRUE,
      language = language
    )
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
