#' Run the Shiny Application
#'
#' @param lau_by_nuts A list of municipalities passed to the app. Defaults to
#'   `sn_lau_by_nuts`. If given, typically a filtered version of
#'   `sn_lau_by_nuts`, or a data frame with the same columns.
#' @param country_name Full name of country to be used as default. Full country
#'   name. See `unique(sn_lau_by_nuts$country_name)` for valid values.
#' @param connection A database connection or connection parameters compatible
#'   with `tidywikidatar` to be use by both `tidywikidatar` and `streetnamer`.
#'   Necessary for deployed apps, as environment parameters are stripped by
#'   Shiny server.
#' @param user_base A data frame with users and passwords in the format used by
#'   the package `shinyauthr`.
#' @param ... arguments to pass to golem_opts. See `?golem::get_golem_options`
#'   for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
sn_run_app <- function(lau_by_nuts = sn_lau_by_nuts,
                       app_title = "Mapping Diversity - European Data Journalism Network (EDJNet)",
                       country_name = NULL,
                       category_choices = c(
                         "politics",
                         "culture",
                         "religion",
                         "military",
                         "other"
                       ),
                       connection = NULL,
                       sn_data_folder = NULL,
                       user_base = NULL,
                       custom_head_html = "",
                       onStart = NULL,
                       options = list(),
                       enableBookmarking = NULL,
                       uiPattern = "/",
                       ...) {
  with_golem_options(
    app = shinyApp(
      ui = sn_app_ui,
      server = sn_app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      lau_by_nuts = lau_by_nuts,
      app_title = app_title,
      country_name = country_name,
      category_choices = category_choices, 
      connection = connection,
      sn_data_folder = sn_data_folder,
      user_base = user_base,
      custom_head_html = custom_head_html
    )
  )
}
