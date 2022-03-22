#' sn_search_wikidata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sn_search_wikidata_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(outputId = ns("search_input_box_ui")),
    shiny::uiOutput(outputId = ns("repeat_input_ui")),
    DT::DTOutput(outputId = ns("search_results_dt")),
    shiny::textInput(
      inputId = ns("wikidata_new_id"),
      label = "or enter custom Wikidata id",
      width = "100%"
    ),
    shiny::actionButton(
      inputId = ns("set_id"),
      label = "Set new id!"
    )
  )
}

#' sn_search_wikidata Server Functions
#'
#' @noRd
mod_sn_search_wikidata_server <- function(id,
                                          search_string = "",
                                          search_language,
                                          connection = NULL,
                                          cache = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    search_results_df_r <- shiny::reactive({
      if (length(input$wikidata_search) == 0) {
        return(NULL)
      } else if (is.null(input$wikidata_search)) {
        return(NULL)
      } else if (is.na(input$wikidata_search)) {
        return(NULL)
      } else if (input$wikidata_search == "") {
        return(NULL)
      } else if (nchar(input$wikidata_search) > 0) {
        search_results_df <- tidywikidatar::tw_search(
          search = input$wikidata_search,
          language = search_language,
          cache = cache,
          cache_connection = connection
        ) %>%
          tidyr::drop_na()
      }

      search_results_df
    })


    output$search_input_box_ui <- shiny::renderUI({
      shiny::textInput(
        inputId = ns("wikidata_search"),
        label = "Search on Wikidata",
        placeholder = "search...",
        value = search_string,
        width = "100%"
      )
    })

    output$search_results_dt <- DT::renderDT(
      expr = {
        search_results_df <- search_results_df_r()

        if (is.null(search_results_df)) {
          return(NULL)
        }

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



    shiny::reactive(search_results_df_r()$id[input$search_results_dt_rows_selected])
  })
}

## To be copied in the UI
# mod_sn_search_wikidata_ui("sn_search_wikidata_ui_1")

## To be copied in the server
# mod_sn_search_wikidata_server("sn_search_wikidata_ui_1")


mod_sn_search_app <- function(search_string,
                              search_language,
                              cache = FALSE,
                              connection = NULL,
                              testing = FALSE) {
  ui <- shiny::fluidPage(
    mod_sn_search_wikidata_ui(id = "sn_search_wikidata_ui_1")
    # ,
    # shiny::uiOutput(outputId = "selected_wikidata_id")
  )

  server <- function(input, output, session) {
    selected_wikidata_id <- mod_sn_search_wikidata_server(
      id = "sn_search_wikidata_ui_1",
      search_string = search_string,
      search_language = search_language,
      cache = cache,
      connection = connection
    )


    #   output$selected_wikidata_id <- shiny::renderUI({
    #     shiny::p(selected_wikidata_id())
    #   })
  }
  shiny::shinyApp(ui, server)
}

# mod_sn_search_app(search_string = "example", search_language = "en", cache = TRUE, connection = connection)