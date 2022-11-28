#' Create thematic maps
#'
#' @param streets_sf
#' @param gisco_id
#' @param scope Defaults to "base". Available values include "gender", "year_of_birth"
#'
#' @return
#' @export
#'
#' @examples
sn_create_map <- function(streets_sf = NULL,
                          streets_data_df = NULL,
                          gisco_id = NULL,
                          scope = "base",
                          unnamed_streets = FALSE,
                          connection = NULL) {
  if (is.null(streets_sf)) {
    streets_sf <- latlon2map::ll_osm_get_lau_streets(
      gisco_id = gisco_id,
      unnamed_streets = unnamed_streets
    )
  }
  
  if (is.null(streets_data_df)) {
    streets_data_df <- sn_get_street_named_after_id(
        gisco_id = gisco_id,
        connection = connection,
        remove_ignored = FALSE
      )
  }
  streets_sf <- streets_sf %>%
    dplyr::rename(street_name = name) %>%
    dplyr::anti_join(
      y = streets_data_df %>%
        dplyr::filter(ignore == 1) %>%
        dplyr::distinct(street_name),
      by = "street_name"
    )

  streets_pre_sf <- streets_sf %>%
    dplyr::left_join(
      y = streets_data_df %>%
        dplyr::filter(ignore == 0 | is.na(ignore)==TRUE) %>%
        dplyr::distinct(),
      by = "street_name"
    ) %>%
    dplyr::filter(is.na(street_name) == FALSE) %>%
    dplyr::select(
      "street_name",
      "named_after_id",
      "person",
      "gender"
    ) %>% 
    dplyr::mutate(qid_label = tw_get_label(id = named_after_id,
                                           cache_connection = connection,
                                           cache = TRUE)
                  # ,
                  # qid_description = tw_get_description(id = named_after_id,
                  #                                      cache_connection = connection)
                  ) %>% 
    
    dplyr::mutate(
      popup_content = stringr::str_c(
        "<big><p>",
        street_name, 
        "</p><b><a href='https://www.wikidata.org/wiki/",
        named_after_id,
        "' target='_blank'>",
        qid_label, "</a></b><br />")
    )

  
  if (scope == "base") {
    leaflet::leaflet(data = streets_pre_sf) %>%
      leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}{r}.png") %>%
      leaflet::addPolylines(
        color = "#ff5454",
        weight = 3,
        popup = streets_pre_sf$popup_content
      ) 
  } else if (scope == "gender") {
    factpal <- leaflet::colorFactor(
      palette = c("#00E5FF", # aquamarine
                  "#004CFF", # blu
                  "#FFFF00", # giallo
                  "#00FF4D", # verde
                  "#4C00FF" # viola
                  ),
      domain = unique(streets_pre_sf$gender)
    )

    leaflet::leaflet(data = streets_pre_sf) %>%
      leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}{r}.png") %>%
      leaflet::addPolylines(
        color = ~ factpal(gender),
        weight = 3,
        popup = streets_pre_sf$popup_content
      )
  } else if (scope == "year_of_birth") {
    pre_yob_sf <- streets_pre_sf %>%
      dplyr::mutate(year_of_birth = tw_get_p1(
        id = named_after_id,
        p = "P569"
      ) %>%
        stringr::str_remove(pattern = stringr::fixed("+")) %>%
        stringr::str_extract(pattern = "[[:digit:]]+") %>%
        as.numeric()) %>%
      dplyr::mutate(
        period_of_birth =
          dplyr::case_when(
            year_of_birth > 1900 ~ "20th century",
            year_of_birth > 1800 ~ "19th century",
            year_of_birth > 1700 ~ "18th century",
            is.na(year_of_birth) == FALSE ~ "earlier",
            TRUE ~ as.character(NA)
          )
      ) %>%
      dplyr::mutate(period_of_birth = factor(
        period_of_birth,
        levels = c(
          as.character(NA),
          "earlier",
          "18th century",
          "19th century",
          "20th century"
        )
      ))

    factpal <- leaflet::colorFactor(
      palette = topo.colors(5),
      domain = unique(pre_yob_sf$period_of_birth)
    )

    pre_yob_sf %>%
      leaflet::leaflet() %>%
      leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}{r}.png") %>%
      leaflet::addPolylines(
        color = ~ factpal(period_of_birth),
        weight = 3
      ) %>%
      leaflet::addLegend("bottomright",
        pal = factpal,
        values = ~period_of_birth,
        title = "",
        opacity = 1
      )
  }
}
