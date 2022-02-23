#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
sn_app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    waiter::use_waiter(),
    waiter::waiter_show_on_load(html = waiter::spin_wandering_cubes()),

    # Your application UI logic
    fluidPage(
      theme = bslib::bs_theme(
        bg = "#fef7ed",
        fg = "#ff5454",
        primary = "#ff5454",
        base_font = bslib::font_google(
          family = "Space Mono",
          ital = 1
        )
      ),
      title = "streetnamer",
      tags$footer(
        title = "footer",
        align = "right",
        style = "
position:fixed;
bottom:0;
width:100%;
height:30px; /* Height of the footer */
color: white;
margin-left: -15px;
background-color: #ff5454;
z-index: 1000;"
      ),
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
                dplyr::pull(country_name)
            ),
            selected = ifelse(is.null(golem::get_golem_options("country_name")), 
                                      FALSE, 
                                      golem::get_golem_options("country_name")),
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
          width = 3,
          offset = 3,
          htmltools::h1("Mapping diversity", style = "text-align:right;")
        ),
      ),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          shiny::conditionalPanel(
            condition = "input.current_city_name != ''&&input.current_city_name != '-'",
            shiny::radioButtons(
              inputId = "streets_to_show_in_dt",
              label = "List...", choices = c(
                "All streets",
                "Not yet checked",
                "Not yet checked in this municipality"
              )
            ),
            shiny::tagList(shiny::actionButton(inputId = "previous_row", label = "Previous street"),
                           shiny::actionButton(inputId = "next_row", label = "Next street"),
                           shiny::actionButton(inputId = "ignore_street", label = "Ignore street!")),
            # shiny::uiOutput(outputId = "street_buttons_UI"),
            shiny::hr()
          ),
          DT::DTOutput(outputId = "current_city_sn_dt"),
          shiny::uiOutput(outputId = "current_street_box_UI")
          # ,
          # shinyWidgets::switchInput(inputId = "checked_switch",
          #                           label = "Manually checked?",
          #                           onLabel = "Yes",
          #                           offLabel = "No",
          #                           size = "large",
          #                           value = TRUE,
          #                           labelWidth = "280px",
          #                           handleWidth = "80px",
          #                           width = "90%")
        ),
        mainPanel = mainPanel(
          tabsetPanel(
            type = "tabs",
            tabPanel("Contribute", mod_sn_street_info_ui("snm_street_info_ui_1")),
            tabPanel(
              "Overview",
              shiny::uiOutput(outputId = "current_city_title"),
              leaflet::leafletOutput(
                outputId = "current_city_map_leaflet",
                height = "600px"
              )
            ),
            tabPanel("Deduplicate", HTML("...")),
            tabPanel(
              "Export",
              shinyauthr::loginUI(id = "login"),
              mod_sn_export_ui("snm_export_ui_1")
            )
          )
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
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "streetnamer"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
