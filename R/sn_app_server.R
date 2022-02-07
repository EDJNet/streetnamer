#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
sn_app_server <- function(input, output, session) {


  #### city selector ####


  cities_in_current_country <- eventReactive(
    eventExpr = input$current_country_name,
    valueExpr = {
      if (is.null(input$current_country_name)) {
        return(NULL)
      } else if (input$current_country_name == "-") {
        return(NULL)
      } else {
        current_country_df <- golem::get_golem_options("lau_by_nuts") %>%
          dplyr::filter(country_name == input$current_country_name) %>%
          dplyr::select(gisco_id, lau_label)

        current_country_gisco_v <- current_country_df %>%
          dplyr::pull(gisco_id)

        names(current_country_gisco_v) <- current_country_df %>%
          dplyr::pull(lau_label)

        return(current_country_gisco_v)
      }
    }
  )


  observeEvent(
    eventExpr = input$current_country_name,
    handlerExpr = {
      updateSelectizeInput(
        session = session,
        inputId = "current_gisco_id",
        selected = character(0),
        choices = cities_in_current_country(),
        server = TRUE
      )
    }
  )

  #### reactive UI #####

  observeEvent(input$current_gisco_id, {
    output$current_city_title <- renderUI({
      golem::get_golem_options("lau_by_nuts") %>%
        dplyr::filter(gisco_id == input$current_gisco_id) %>%
        dplyr::pull(.data$lau_label) %>%
        shiny::h2()
    })
  })


  #### reactive data load #####


  current_streets_sf_r <- shiny::eventReactive(
    eventExpr = input$current_gisco_id,
    valueExpr = {
      if (is.null(input$current_gisco_id)) {
        return(NULL)
      }

      if (input$current_gisco_id == " ") {
        return(NULL)
      }

      current_gisco_id <- golem::get_golem_options("lau_by_nuts") %>%
        dplyr::filter(gisco_id == input$current_gisco_id) %>%
        dplyr::pull(gisco_id)


      ll_osm_lau_streets(
        gisco_id = current_gisco_id,
        unnamed_streets = FALSE
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    label = "current_streets_sf_r"
  )

  current_streets_df_r <- reactive({
    if (exists(x = "current_streets_sf_r") == FALSE) {
      return(NULL)
    }
    if (is.null(current_streets_sf_r())) {
      return(NULL)
    }

    current_streets_sf_r() %>%
      sf::st_drop_geometry() %>%
      dplyr::distinct(name)
  })


  # current_streets_df_r <- eventReactive(
  #   eventExpr = input$current_gisco_id,
  #   valueExpr = {
  #     current_gisco_id <- streetnamer::sn_lau_by_country %>%
  #       dplyr::filter(CNTR_NAME == input$current_country_name,
  #                     LAU_NAME == input$current_gisco_id) %>%
  #       dplyr::pull(GISCO_ID)
  #
  #     current_country_code <- stringr::str_extract(string = current_gisco_id,
  #                                                  pattern = "[A-Z]+") %>%
  #       stringr::str_to_lower()
  #
  #     # TODO enable custom streets
  #
  #     dplyr::tbl(src = sn_connect_to_db(
  #       connection = NULL,
  #       country = current_country_code,
  #       type = "osm_lau_street_names"
  #     ),
  #     sn_get_db_table_name(type = "osm_lau_street_names",
  #                          country = current_country_code)) %>%
  #       dplyr::filter(.data$gisco_id ==  current_gisco_id) %>%
  #       dplyr::distinct(name) %>%
  #       tibble::as_tibble()
  #
  #   })

  ##### streetnamer data table #####



  output$current_city_sn_dt <- DT::renderDT(
    expr = {
      if (is.null(current_streets_df_r())) {
        return(NULL)
      }
      DT::datatable(
        data = current_streets_df_r(),
        filter = "top",
        options = list(
          dom = "tp",
          pageLength = 5,
          lengthMenu = c(
            3, 5,
            10,
            20,
            50,
            100
          ),
          stateSave = TRUE,
          ordering = FALSE
        ),
        colnames = "Street name",
        rownames = FALSE,
        selection = "single"
      )
    },
    server = TRUE
  )

  DTproxy <- DT::dataTableProxy("current_city_sn_dt")

  observeEvent(list(input$next_row),
    {
      DT::selectRows(
        DTproxy,
        sum(input$current_city_sn_dt_rows_selected, 1)
      )

      DT::selectPage(
        proxy = DTproxy,
        page = input$current_city_sn_dt_rows_selected %/% input$current_city_sn_dt_state$length + 1
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  observeEvent(list(input$previous_row),
    {
      DT::selectRows(
        DTproxy, input$current_city_sn_dt_rows_selected - 1L
      )


      DT::selectPage(
        proxy = DTproxy,
        page = input$current_city_sn_dt_rows_selected %/% input$current_city_sn_dt_state$length + 1
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  street_selected <- shiny::eventReactive(
    list(input$current_city_sn_dt_rows_selected),
    {
      if (is.null(current_streets_df_r()) == TRUE) {
        return(NULL)
      } else if (nrow(current_streets_df_r()) == 0) {
        return(NULL)
      } else if (nrow(current_streets_df_r()) > 0) {
        if (length(input$current_city_sn_dt_rows_selected) == 1) {
          current_streets_df_r() %>%
            dplyr::slice(input$current_city_sn_dt_rows_selected)
        } else {
          current_streets_df_r() %>%
            dplyr::slice(1)
        }
      } else {
        return(NULL)
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )



  #### Add details about streets #####


  output$current_street_box_UI <- shiny::renderUI({
    if (is.null(street_selected()) == TRUE) {
      return(NULL)
    }
    shiny::tagList(
      h4(street_selected()$name)
    )
  })




  ##### leaflet map #####

  output$current_city_map_leaflet <- leaflet::renderLeaflet({
    if (is.null(current_streets_sf_r())) {
      return(NULL)
    }
    if (nrow(current_streets_sf_r()) == 0) {
      return(NULL)
    }

    leaflet::leaflet(data = current_streets_sf_r()) %>%
      leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}{r}.png") %>%
      leaflet::addPolylines(
        color = "#ff5454",
        weight = 3
      )
  })

  waiter::waiter_hide()
}
