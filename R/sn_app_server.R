#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
sn_app_server <- function(input, output, session) {
  #### Cicerone ####

  observeEvent(eventExpr = input$take_a_tour, handlerExpr = {
    guide <- sn_cicerone()
    guide$init()$start()
  })

  #### authentication ####

  # if user base not given, then default to valid user
  if (is.null(golem::get_golem_options("user_base"))) {
    credentials <- reactive({
      list(user_auth = TRUE)
    })
  } else {
    credentials <- shinyauthr::loginServer(
      id = "login",
      data = golem::get_golem_options("user_base"),
      user_col = user,
      pwd_col = password,
      log_out = reactive(logout_init())
    )
  }

  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  #### end of authentication #####


  #### modules #####

  observeEvent(list(
    credentials()$user_auth,
    input$current_gisco_id
  ), {
    mod_sn_export_server(
      id = "snm_export_ui_1",
      gisco_id = input$current_gisco_id,
      country = stringr::str_extract(
        string = input$current_gisco_id,
        pattern = "[A-Z][A-Z]"
      ),
      include_checked_elsewhere_in_country = TRUE,
      streets_sf = current_streets_sf_r(),
      enable = credentials()$user_auth,
      connection = golem::get_golem_options("connection")
    )


    if (is.null(credentials()$info)) {
      return(NULL)
    } else if (is.data.frame(credentials()$info) == FALSE) {
      return(NULL)
    } else if (nrow(credentials()$info) == 0) {
      return(NULL)
    } else {
      upload_check <- credentials()$info[["permissions"]] == "import"
    }

    if (length(upload_check) == 0) {
      upload_l <- FALSE
    } else {
      upload_l <- upload_check
    }

    # credentials()$info[["permissions"]]=="upload"
    mod_sn_import_server(
      id = "snm_import_ui_1",
      connection = golem::get_golem_options("connection"),
      enable = upload_check
    )
  })



  
  current_basic_municipality_df_r <- shiny::observeEvent(
    eventExpr = list(input$update_basic_municipality),
    handlerExpr = {
      if (is.null(input$current_gisco_id)) {
        return(NULL)
      }
      
      if (input$current_gisco_id == " ") {
        return(NULL)
      }
      
      print("Getting basic municipality stats")
      
      mod_sn_show_basic_municipality_server(
        id = "mod_sn_show_basic_municipality_1",
        gisco_id = input$current_gisco_id,
        gisco_label = golem::get_golem_options("lau_by_nuts") %>%
          dplyr::filter(gisco_id == input$current_gisco_id) %>%
          dplyr::pull(.data$lau_label),
        country =  stringr::str_extract(
          string = input$current_gisco_id,
          pattern = "[A-Z][A-Z]"
        ),
        streets_sf = current_streets_sf_r(),
        connection = golem::get_golem_options("connection"),
        language = "en"
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    label = "current_basic_municipality"
  )
  

  
  current_core_df_r <- shiny::observeEvent(
    eventExpr = list(input$update_summary_stats),
    handlerExpr = {
      if (is.null(input$current_gisco_id)) {
        return(NULL)
      }

      if (input$current_gisco_id == " ") {
        return(NULL)
      }

      print("Getting summary stats")

      mod_sn_show_summary_stats_server(
        id = "mod_sn_show_summary_stats_1",
        gisco_id = input$current_gisco_id,
        gisco_label = golem::get_golem_options("lau_by_nuts") %>%
          dplyr::filter(gisco_id == input$current_gisco_id) %>%
          dplyr::pull(.data$lau_label),
        country = stringr::str_extract(
          string = input$current_gisco_id,
          pattern = "[A-Z][A-Z]"
        ),
        streets_sf = current_streets_sf_r(),
        connection = golem::get_golem_options("connection"),
        language = "en"
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    label = "current_summary_stats"
  )


  #### end of modules #####

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
        # selected = character(0),
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

      current_gisco_id <- input$current_gisco_id

      current_country_code <- stringr::str_extract(
        string = current_gisco_id,
        pattern = "[A-Z][A-Z]"
      )

      if (current_country_code == "UK") {
        # check if northern ireland
        if (stringr::str_starts(string = current_gisco_id, pattern = "UK_N")) {
          current_country_name <- "ireland-and-northern-ireland"
        } else {
          current_country_name <- "great-britain"
        }
      } else if (current_country_code == "IE") {
        current_country_name <- "ireland-and-northern-ireland"
      } else if (current_country_code == "MD") {
        current_country_name <- "moldova"
      } else {
        current_country_name <- NULL
      }


      ll_osm_get_lau_streets(
        gisco_id = current_gisco_id,
        country = current_country_name,
        unnamed_streets = FALSE,
        year = golem::get_golem_options("lau_year")
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

    if (input$streets_to_show_in_dt == "All streets (including ignored)") {
      current_streets_sf_r() %>%
        sf::st_drop_geometry() %>%
        dplyr::distinct(name)
    } else if (input$streets_to_show_in_dt == "All streets") {
      current_streets_sf_r() %>%
        sf::st_drop_geometry() %>%
        dplyr::distinct(name) %>%
        dplyr::anti_join(
          y = sn_get_street_named_after_id(
            gisco_id = input$current_gisco_id,
            country = stringr::str_extract(
              string = input$current_gisco_id,
              pattern = "[A-Z][A-Z]"
            ),
            connection = golem::get_golem_options("connection"),
            only_ignored = TRUE
          ) %>%
            dplyr::distinct(street_name) %>%
            dplyr::rename(name = street_name),
          by = "name"
        )
    } else if (input$streets_to_show_in_dt == "Not yet checked") {
      current_streets_sf_r() %>%
        sf::st_drop_geometry() %>%
        dplyr::distinct(name) %>%
        dplyr::anti_join(
          y = sn_get_street_named_after_id(
            country = stringr::str_extract(
              string = input$current_gisco_id,
              pattern = "[A-Z][A-Z]"
            ),
            remove_ignored = FALSE,
            only_checked = TRUE,
            keep_only_latest = TRUE,
            connection = golem::get_golem_options("connection")
          ) %>%
            dplyr::distinct(street_name) %>%
            dplyr::rename(name = street_name),
          by = "name"
        )
    } else if (input$streets_to_show_in_dt == "Not yet checked in this municipality") {
      current_streets_sf_r() %>%
        sf::st_drop_geometry() %>%
        dplyr::distinct(name) %>%
        dplyr::anti_join(
          y = sn_get_street_named_after_id(
            gisco_id = input$current_gisco_id,
            country = stringr::str_extract(
              string = input$current_gisco_id,
              pattern = "[A-Z][A-Z]"
            ),
            remove_ignored = FALSE,
            only_checked = TRUE,
            keep_only_latest = TRUE,
            connection = golem::get_golem_options("connection")
          ) %>%
            dplyr::distinct(street_name) %>%
            dplyr::rename(name = street_name),
          by = "name"
        )
    } else if (input$streets_to_show_in_dt == "Checked humans without confirmed gender") {
      sn_get_street_named_after_id(
        gisco_id = input$current_gisco_id,
        country = stringr::str_extract(
          string = input$current_gisco_id,
          pattern = "[A-Z][A-Z]"
        ),
        connection = golem::get_golem_options("connection")
      ) %>%
        dplyr::filter(
          checked == TRUE,
          person == TRUE,
          is.na(.data$gender) == TRUE
        ) %>%
        dplyr::distinct(street_name) %>%
        dplyr::rename(name = street_name)
    } else if (input$streets_to_show_in_dt == "Checked humans without confirmed id") {
      sn_get_street_named_after_id(
        gisco_id = input$current_gisco_id,
        country = stringr::str_extract(
          string = input$current_gisco_id,
          pattern = "[A-Z][A-Z]"
        ),
        connection = golem::get_golem_options("connection")
      ) %>%
        dplyr::filter(
          checked == TRUE,
          person == TRUE,
          is.na(.data$named_after_id) == TRUE
        ) %>%
        dplyr::distinct(street_name) %>%
        dplyr::rename(name = street_name)
    } else if (input$streets_to_show_in_dt == "Not yet checked, but likely humans") {
      current_country_v <- stringr::str_extract(
        string = input$current_gisco_id,
        pattern = "[A-Z][A-Z]"
      )

      not_checked_df <- current_streets_sf_r() %>%
        sf::st_drop_geometry() %>%
        dplyr::distinct(name) %>%
        dplyr::anti_join(
          y = sn_get_street_named_after_id(
            country = current_country_v,
            remove_ignored = FALSE,
            only_checked = TRUE,
            keep_only_latest = TRUE,
            connection = golem::get_golem_options("connection")
          ) %>%
            dplyr::distinct(street_name) %>%
            dplyr::rename(name = street_name),
          by = "name"
        )

      if (current_country_v == "BE") {
        to_search_df <- tibble::tibble(
          name = not_checked_df$name,
          name_clean = sn_clean_street_name(
            street_name = not_checked_df$name,
            country = current_country_v
          )
        )
        to_search_df <- sn_get_clean_street_name_bilingual_df(
          gisco_id = input$current_gisco_id,
          street_names_df = to_search_df
        )
      } else {
        to_search_df <- tibble::tibble(
          name = not_checked_df$name,
          name_clean = sn_clean_street_name(
            street_name = not_checked_df$name,
            country = current_country_v
          )
        )
      }


      auto_named_after_df <- sn_search_named_after(
        gisco_id = input$current_gisco_id,
        street_names_df = to_search_df,
        cache = TRUE,
        overwrite_cache = FALSE,
        connection = golem::get_golem_options("connection")
      )

      auto_named_after_humans_df <- auto_named_after_df %>%
        dplyr::mutate(instance_of = tw_get_p1(
          id = named_after_id,
          p = "P31",
          cache = TRUE,
          overwrite_cache = FALSE,
          cache_connection = golem::get_golem_options("connection"),
          disconnect_db = TRUE
        )) %>%
        dplyr::filter(is.na(instance_of) == FALSE) %>%
        dplyr::filter(instance_of == "Q5")

      auto_named_after_humans_df %>%
        dplyr::rename(name = street_name) %>%
        dplyr::distinct(name)
    } else {
      current_streets_sf_r() %>%
        sf::st_drop_geometry() %>%
        dplyr::distinct(name)
    }
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
          pageLength = 8,
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

  ### if no street is selected, and there is something in the table, then select first row

  observeEvent(input$current_city_sn_dt_rows_selected,
    {
      if (is.null(current_streets_df_r()) == TRUE) {
        return(NULL)
      } else if (nrow(current_streets_df_r()) == 0) {
        return(NULL)
      } else if (nrow(current_streets_df_r()) > 0) {
        if (length(input$current_city_sn_dt_rows_selected) == 0) {
          DT::selectRows(
            DTproxy,
            1
          )
        }
      }
    },
    ignoreNULL = FALSE
  )

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

      shinyWidgets::updateSwitchInput(
        inputId = "wikidata_search_panel_switch",
        value = FALSE
      )
    },
    ignoreNULL = FALSE,
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

      shinyWidgets::updateSwitchInput(
        inputId = "wikidata_search_panel_switch",
        value = FALSE
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  observeEvent(list(input$ignore_street),
    {
      sn_write_street_named_after_id(
        country = stringr::str_extract(
          string = input$current_gisco_id,
          pattern = "[A-Z][A-Z]"
        ),
        gisco_id = input$current_gisco_id,
        street_name = street_selected()$name,
        named_after_id = as.character(NA),
        person = as.integer(NA),
        gender = as.character(NA),
        category = as.character(NA),
        tag = as.character(NA),
        checked = as.integer(TRUE),
        ignore = as.integer(TRUE),
        session = session$token,
        append = TRUE,
        connection = golem::get_golem_options("connection")
      )


      DT::selectRows(
        DTproxy,
        sum(input$current_city_sn_dt_rows_selected, 1)
      )

      DT::selectPage(
        proxy = DTproxy,
        page = input$current_city_sn_dt_rows_selected %/% input$current_city_sn_dt_state$length + 1
      )

      shinyWidgets::updateSwitchInput(
        inputId = "wikidata_search_panel_switch",
        value = FALSE
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  observeEvent(
    list(input$confirm_match),
    {
      sn_write_street_named_after_id(
        df_to_write = selected_df_rv$df(),
        connection = golem::get_golem_options("connection"),
        append = TRUE
      )


      DT::selectRows(
        DTproxy,
        sum(input$current_city_sn_dt_rows_selected, 1)
      )

      DT::selectPage(
        proxy = DTproxy,
        page = input$current_city_sn_dt_rows_selected %/% input$current_city_sn_dt_state$length + 1
      )

      shinyWidgets::updateSwitchInput(
        inputId = "wikidata_search_panel_switch",
        value = FALSE
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

  # output$street_buttons_UI <- shiny::renderUI({
  #   if (is.null(street_selected()) == TRUE) {
  #     return(NULL)
  #   }
  #
  #
  # })

  output$current_street_box_UI <- shiny::renderUI({
    if (is.null(street_selected()) == TRUE) {
      return(NULL)
    }
    shiny::tagList(
      h4(street_selected()$name)
    )
  })

  ####  Wikidata search module ####

  shiny::observeEvent(
    list(
      street_selected()$name,
      input$wikidata_search_panel_switch
    ),
    {

      if ( input$current_gisco_id %in% sn_bilingual_gisco_id$gisco_id) {
        current_default_search_language_v <- sn_bilingual_gisco_id %>% 
          dplyr::filter(gisco_id == input$current_gisco_id) %>% 
          dplyr::pull(languages) %>% 
          stringr::str_extract(pattern = "[[:alpha:]][[:alpha:]]")
      } else {
        current_default_search_language_v <- streetnamer::sn_language_defaults_by_country %>%
          dplyr::filter(country == input$current_country_name) %>%
          dplyr::pull(language_code)
      }
      
      if (length(current_default_search_language_v) == 0) {
        current_default_search_language_v <- "en"
      } else if (length(current_default_search_language_v) > 1) {
        current_default_search_language_v <- current_default_search_language_v[1]
      }


      selected_named_after_id_from_search_r <- mod_sn_search_wikidata_server(
        id = "sn_search_wikidata_ui_1",
        search_string = sn_get_clean_street_name_bilingual_df(street_name = street_selected()$name,
                                                              gisco_id = input$current_gisco_id
          
        ) %>% 
          dplyr::pull(name_clean),
        search_language = current_default_search_language_v,
        description_language = "en",
        cache = TRUE,
        connection = golem::get_golem_options("connection")
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  selected_named_after_id_from_search_r <- mod_sn_search_wikidata_server(
    id = "sn_search_wikidata_ui_1",
    search_string = street_selected()$name,
    search_language = "en",
    description_language = "en",
    cache = TRUE,
    connection = golem::get_golem_options("connection")
  )

  #### leaflet map ####

  output$current_city_map_leaflet <- leaflet::renderLeaflet({
    if (is.null(current_streets_sf_r())) {
      return(NULL)
    }
    if (nrow(current_streets_sf_r()) == 0) {
      return(NULL)
    }

    sn_create_map(streets_sf = current_streets_sf_r(), 
                  streets_data_df = sn_get_street_named_after_id(
                    gisco_id = input$current_gisco_id,
                    connection = golem::get_golem_options("connection"),
                    remove_ignored = FALSE
                  ),
                  gisco_id = input$current_gisco_id,
                  scope = input$map_scope_selector,
                  connection = golem::get_golem_options("connection"))
    
    # leaflet::leaflet(data = current_streets_sf_r()) %>%
    #   leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}{r}.png") %>%
    #   leaflet::addPolylines(
    #     color = "#ff5454",
    #     weight = 3
    #   )
  })


  ##### Wikidata street name module #####

  selected_df_rv <- reactiveValues(df = NULL)


  shiny::observeEvent(
    eventExpr = street_selected()$name,
    handlerExpr = {
      selected_df_rv$df <- mod_sn_street_info_server(
        id = "snm_street_info_ui_1",
        street_name = street_selected()$name,
        gisco_id = input$current_gisco_id,
        country = stringr::str_extract(
          string = input$current_gisco_id,
          pattern = "[A-Z][A-Z]"
        ),
        enable_tag = golem::get_golem_options("enable_tag"),
        category_choices =  golem::get_golem_options("category_choices"),
        connection = golem::get_golem_options("connection")
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )


  shiny::observeEvent(
    eventExpr = input$drop_named_after_id_switch,
    handlerExpr = {
      if (input$drop_named_after_id_switch == TRUE) {
        selected_df_rv$df <- mod_sn_street_info_server(
          id = "snm_street_info_ui_1",
          street_name = street_selected()$name,
          gisco_id = input$current_gisco_id,
          country = stringr::str_extract(
            string = input$current_gisco_id,
            pattern = "[A-Z][A-Z]"
          ),
          enable_tag = golem::get_golem_options("enable_tag"),
          category_choices =  golem::get_golem_options("category_choices"),
          named_after_id = "drop",
          connection = golem::get_golem_options("connection")
        )


        shinyWidgets::updateSwitchInput(
          inputId = "drop_named_after_id_switch",
          value = FALSE
        )
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )


  shiny::observeEvent(
    eventExpr = selected_named_after_id_from_search_r(),
    handlerExpr = {
      if (length(selected_named_after_id_from_search_r()) == 0) {
        return(NULL)
      }
      selected_df_rv$df <- mod_sn_street_info_server(
        id = "snm_street_info_ui_1",
        street_name = street_selected()$name,
        gisco_id = input$current_gisco_id,
        country = stringr::str_extract(
          string = input$current_gisco_id,
          pattern = "[A-Z][A-Z]"
        ),
        enable_tag = golem::get_golem_options("enable_tag"),
        category_choices = golem::get_golem_options("category_choices"),
        named_after_id = selected_named_after_id_from_search_r(),
        connection = golem::get_golem_options("connection")
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )


  # output$named_after_id_selected_output <- shiny::renderUI(
  #   shiny::p(selected_df_r())
  # )

  waiter::waiter_hide()
}
