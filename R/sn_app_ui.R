#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
sn_app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(app_title = golem::get_golem_options("app_title")),
    waiter::use_waiter(),
    # waiter::useHostess(),
    cicerone::use_cicerone(),
    waiter::waiter_show_on_load(html = waiter::spin_wandering_cubes()),
    tags$head(shiny::HTML(golem::get_golem_options("custom_head_html"))),

    # Your application UI logic
    fluidPage(
      theme = bslib::bs_theme(
        bg = "#fef7ed",
        fg = "#ff5454",
        primary = "#ff5454",
        base_font = bslib::font_google(
          family = "Space Mono",
          ital = 1,
          local = FALSE
        )
      ),
      title = golem::get_golem_options("app_title"),
      #       tags$footer(
      #         title = "footer",
      #         align = "right",
      #         style = "
      # position:fixed;
      # bottom:0;
      # width:100%;
      # height:30px
      # color: white;
      # margin-left: -15px;
      # background-color: #ff5454;
      # z-index: 1000;"
      #       ),
      # waiter::autoWaiter(),
      fluidRow(
        column(
          width = 2,
          shiny::selectInput(
            inputId = "current_country_name",
            label = "Select country",
            choices = c(
              "",
              golem::get_golem_options("lau_by_nuts") %>%
                dplyr::distinct(country_name) %>%
                dplyr::arrange(country_name) %>%
                dplyr::pull(country_name)
            ),
            selected = ifelse(is.null(golem::get_golem_options("country_name")),
              FALSE,
              golem::get_golem_options("country_name")
            ),
            selectize = TRUE
          )
        ),
        column(
          width = 3,
          shiny::conditionalPanel(
            condition = "input.current_country_name != ''&&current_country_name != ' '",
            shiny::selectInput(
              inputId = "current_gisco_id",
              label = "Select municipality",
              choices = NULL,
              selected = FALSE,
              selectize = TRUE,
              width = "100%"
            )
          )
        ),
        column(
          width = 2,
          offset = 0,
          shiny::actionButton(
            inputId = "take_a_tour",
            label = "Take a tour of the app",
            icon = shiny::icon("info"),
            style = "margin-top:32px;"
          )
        ),
        column(
          width = 3,
          offset = 1,
          htmltools::h1("Mapping diversity", style = "text-align:right;")
        ),
      ),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          shiny::conditionalPanel(
            condition = "input.current_city_name != ''&&input.current_city_name != '-'",
            shiny::selectizeInput(
              inputId = "streets_to_show_in_dt",
              label = "List...",
              choices = c(
                "Not yet checked",
                "Confirmed humans without gender information",
                "Confirmed humans without id",
                "Confirmed humans without category",
                "Confirmed humans without category set explicitly",
                "Not yet checked, but likely humans",
                "Not yet checked in this municipality",
                "All streets",
                "All streets (including ignored)"
              )
            ),
            shiny::tagList(
              shiny::actionButton(inputId = "previous_row", label = "Previous"),
              shiny::actionButton(inputId = "next_row", label = "Next"),
              shiny::actionButton(inputId = "ignore_street", label = "Ignore"),
              shiny::actionButton(inputId = "confirm_match", label = "Confirm")
            ),
            # shiny::uiOutput(outputId = "street_buttons_UI"),
            shiny::hr()
          ),
          DT::DTOutput(outputId = "current_city_sn_dt"),
          shiny::uiOutput(outputId = "current_street_box_UI"),
          width = 3
        ),
        mainPanel = mainPanel(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Contribute",
              fluidRow(
                column(
                  6,
                  mod_sn_street_info_ui("snm_street_info_ui_1")
                ),
                column(
                  6,
                  shinyWidgets::switchInput(
                    inputId = "wikidata_search_panel_switch",
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
                    condition = "input.wikidata_search_panel_switch == true",
                    mod_sn_search_wikidata_ui(id = "sn_search_wikidata_ui_1")
                  ),
                  shinyWidgets::switchInput(
                    inputId = "drop_named_after_id_switch",
                    label = "Drop Wikidata id?",
                    onLabel = "Yes",
                    offLabel = "No",
                    size = "large",
                    value = FALSE,
                    labelWidth = "280px",
                    handleWidth = "80px",
                    width = "90%"
                  )
                )
              )
            ),
            tabPanel(
              "About this municipality",
              shiny::fluidRow(
                align = "center",
                shiny::column(
                  width = 10, offset = 1, align = "center",
                  shiny::actionButton(
                    inputId = "update_basic_municipality",
                    label = "Update and show basic stats for the current municipality",
                    width = "80%",
                    style = "margin-top:20px;"
                  )
                )
              ),
              shiny::fluidRow(
                align = "center",
                shiny::column(
                  width = 6,
                  offset = 3,
                  align = "center",
                mod_sn_show_basic_municipality_ui("mod_sn_show_basic_municipality_1")
                )
              ),
              shiny::fluidRow(
                align = "center",
                shiny::column(
                  width = 10, offset = 1, align = "center",
                  shiny::actionButton(
                    inputId = "update_summary_stats",
                    label = "Update and show more details on data availability",
                    width = "80%",
                    style = "margin-top:20px;"
                  )
                )
              ),
              mod_sn_show_summary_stats_ui("mod_sn_show_summary_stats_1")
            ),
            tabPanel(
              "Map",
              shiny::selectInput(
                inputId = "map_scope_selector",
                label = "Select map type",
                choices = list(`Base map` = "base",
                               `Gender` = "gender",
                               `Year of birth` = "year_of_birth")
                ),
              shiny::uiOutput(outputId = "current_city_title"),
              leaflet::leafletOutput(
                outputId = "current_city_map_leaflet",
                height = "600px"
              )
            ),
          #  tabPanel("Deduplicate", HTML("...")),
            tabPanel(
              "Export",
              shinyauthr::loginUI(id = "login"),
              mod_sn_import_ui("snm_import_ui_1"),
              mod_sn_export_ui("snm_export_ui_1")
            ),
          tabPanel(
            "Instructions",
            shiny::includeHTML(system.file("app/www/instructions.html",
                                           package="streetnamer"))
          ),
          tabPanel(
            "About",
            shiny::includeHTML(system.file("app/www/about.html",
                                           package="streetnamer"))
          )
          ),
          width = 9
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(app_title = "Mapping Diversity - European Data Journalism Network") {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = app_title
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
