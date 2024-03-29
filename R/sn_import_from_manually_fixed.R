#' Import into the database manually fixed street names
#'
#' @section How are fix columns to be used:
#'
#' - `tick_if_wrong`: expected either `x`, or empty. Since this package is mostly focused on humans, it expects that the `humans` files will be checked most thoroughly: if the `tick_if_wrong` column is left empty for a given row, then it will be assumed that the automatic matching is right. On the contrary, in the `non_humans` files, rows without the `tick_if_wrong` box will simply be ignored.
#' - `fixed_human`: if a given row has a tick (typically, `x`), then it means that the row refers to a human. If left empty, that it does not refer to a human
#' - `fixed_named_after_id`: if left empty, it is assumed that the Wikidata identifier is not known. If given, it must correspond to a Wikidata Q identifier, such as `Q539`
#' - `fixed_sex_or_gender`: if left empty, no particular assumption will be made. If the Wikidata identifier is given, this can mostly be left empty, as the information will be derived from there. If given, it should be one of the options available in the online interface, or a their shortened form: `female` (`f`), `male` (`m`), `other` (`o`), `uncertain`, (`u`).
#' - `fixed_category`: can typically be left empty
#' - `fixed_n_dedicated_to`: if left empty, assumed to be one. This can be used to express when a street is dedicated to more than one person: in that case, the row should be duplicated as many times as the needed, and the same number be included in each row of `fixed_n_dedicated_to`.
#'
#' Recently produced files may also include the following columns:
#'   - `named_after_custom_label`: this can be used when a full, clean name of the person a street is dedicated to can be desumed, or is otherwise known, but no Wikidata identifiers is available. Additional useful details can be added within brackets after the name.
#' - `fixed_ignore`: if left empty, no assumption will be made. If ticked, it will be assumed that the row does not refer to a proper street,
#'
#' After a file is processed, then it can be re-read and stored in the local database or re-uploaded to the web interface.
#'
#' @param input_df A data frame or a link to csv file.
#' @param type Defaults to NULL. Valid values are "humans" and "not_humans". If `df` is a path, it will be tentatively desumed from the name, by checking if the file name ends with either "not_humans" or only "humans".
#'
#' @inheritParams sn_write_street_named_after_id
#'
#' @return
#' @export
#'
#' @examples
sn_import_from_manually_fixed <- function(input_df,
                                          type = NULL,
                                          gisco_id = NULL,
                                          country = NULL,
                                          connection = NULL,
                                          language = tidywikidatar::tw_get_language(),
                                          return_df_only = TRUE,
                                          session = stringi::stri_rand_strings(n = 1, length = 24)) {
  if (is.data.frame(input_df) == FALSE) {
    if (is.null(gisco_id)) {
      gisco_id <- fs::path_file(path = input_df) %>%
        stringr::str_extract(pattern = "[^-]+")
    }
    if (is.null(country)) {
      country <- stringr::str_extract(string = gisco_id, pattern = "[[:alnum:]]{2}")
    }

    if (is.null(type)) {
      if (stringr::str_detect(string = input_df, pattern = stringr::fixed("not_humans."))) {
        type <- "not_humans"
      } else if (stringr::str_detect(string = input_df, pattern = stringr::fixed("non_humans."))) {
        type <- "not_humans"
      } else if (stringr::str_detect(string = input_df, pattern = stringr::fixed("humans."))) {
        type <- "humans"
      } else {
        usethis::ui_stop("`type` must be given, or be included at the end of the file name.")
      }
    }
    input_df <- readr::read_csv(file = input_df, show_col_types = FALSE)
  }

  if ("tic_if_wrong" %in% colnames(input_df)) {
    input_df <- input_df %>%
      dplyr::rename(tick_if_wrong = tic_if_wrong)
  }

  if ("fixed_wikidata_id" %in% colnames(input_df)) {
    input_df <- input_df %>%
      dplyr::rename(fixed_named_after_id = fixed_wikidata_id)
  }

  if ("name" %in% colnames(input_df)) {
    input_df <- input_df %>%
      dplyr::rename(street_name = name)
  }

  if ("id" %in% colnames(input_df)) {
    input_df <- input_df %>%
      dplyr::rename(named_after_id = id)
  }


  relevant_df <- input_df %>%
    dplyr::select(
      gisco_id,
      street_name,
      named_after_id,
      tick_if_wrong,
      fixed_human,
      fixed_named_after_id,
      fixed_sex_or_gender,
      fixed_category,
      fixed_n_dedicated_to
    ) %>%
    dplyr::mutate(
      tick_if_wrong = as.character(tick_if_wrong),
      fixed_human = as.character(fixed_human),
      fixed_named_after_id = as.character(fixed_named_after_id),
      fixed_sex_or_gender = stringr::str_to_lower(as.character(fixed_sex_or_gender)),
      fixed_category = as.character(fixed_category)
    )

  if (type == "humans") {

    # write confirmed humans

    confirmed_humans_df <- relevant_df %>%
      dplyr::filter(is.na(tick_if_wrong))


    named_after_id_import <- dplyr::if_else(condition = is.na(tidywikidatar::tw_check_qid(
      id = confirmed_humans_df$fixed_named_after_id,
      non_id_as_NA = TRUE
    )),
    true = tidywikidatar::tw_check_qid(
      id = confirmed_humans_df$named_after_id,
      non_id_as_NA = TRUE
    ),
    false = tidywikidatar::tw_check_qid(
      id = confirmed_humans_df$fixed_named_after_id,
      non_id_as_NA = TRUE
    )
    )

    confirmed_humans_fix_gender_df <- confirmed_humans_df %>%
      dplyr::filter(is.na(fixed_sex_or_gender) == TRUE & tidywikidatar::tw_check_qid(named_after_id, logical_vector = TRUE))

    if (nrow(confirmed_humans_fix_gender_df) > 0) {
      confirmed_humans_df <- confirmed_humans_df %>%
        dplyr::mutate(fixed_sex_or_gender = dplyr::case_when(
          is.na(fixed_sex_or_gender) == TRUE & tidywikidatar::tw_check_qid(named_after_id, logical_vector = TRUE) ~
          sn_get_gender_label(
            named_after_id = named_after_id,
            language = language,
            cache_connection = connection,
            cache = TRUE,
            disconnect_db = FALSE
          ),
          TRUE ~ as.character(fixed_sex_or_gender)
        ))
    }

    gender_confirmed_v <- dplyr::case_when(
      is.na(confirmed_humans_df$fixed_sex_or_gender) ~ as.character(NA),
      confirmed_humans_df$fixed_sex_or_gender == "m" | confirmed_humans_df$fixed_sex_or_gender == "male" ~ "male",
      confirmed_humans_df$fixed_sex_or_gender == "f" | confirmed_humans_df$fixed_sex_or_gender == "female" ~ "female",
      confirmed_humans_df$fixed_sex_or_gender == "o" | confirmed_humans_df$fixed_sex_or_gender == "other" ~ "other",
      confirmed_humans_df$fixed_sex_or_gender == "u" | confirmed_humans_df$fixed_sex_or_gender == "uncertain" ~ "uncertain",
      TRUE ~ as.character(NA)
    )

    confirmed_output_df <- sn_write_street_named_after_id(
      gisco_id = gisco_id,
      country = country,
      street_name = confirmed_humans_df$street_name,
      named_after_id = named_after_id_import,
      named_after_n = confirmed_humans_df$fixed_n_dedicated_to,
      category = confirmed_humans_df$fixed_category,
      gender = gender_confirmed_v,
      checked = TRUE,
      person = TRUE,
      session = session,
      return_df_only = return_df_only,
      connection = connection,
      disconnect_db = FALSE
    )

    # write fixed humans

    fixed_humans_df <- relevant_df %>%
      dplyr::filter(is.na(tick_if_wrong) == FALSE)

    person_lv <- is.na(fixed_humans_df$fixed_human) == FALSE

    named_after_id_fixed_import <- dplyr::if_else(condition = is.na(tidywikidatar::tw_check_qid(
      id = fixed_humans_df$fixed_named_after_id,
      non_id_as_NA = TRUE
    )),
    true = as.character(NA),
    false = tidywikidatar::tw_check_qid(
      id = fixed_humans_df$fixed_named_after_id,
      non_id_as_NA = TRUE
    )
    )

    fixed_humans_df <- fixed_humans_df %>%
      dplyr::mutate(fixed_sex_or_gender = dplyr::case_when(
        is.na(fixed_human) == FALSE & is.na(fixed_sex_or_gender) == TRUE & tidywikidatar::tw_check_qid(fixed_named_after_id, logical_vector = TRUE) ~
        sn_get_gender_label(
          named_after_id = fixed_named_after_id,
          language = language,
          cache_connection = connection,
          cache = TRUE,
          disconnect_db = FALSE
        ),
        TRUE ~ fixed_sex_or_gender
      ))


    gender_fixed_v <- dplyr::case_when(
      is.na(fixed_humans_df$fixed_sex_or_gender) ~ as.character(NA),
      fixed_humans_df$fixed_sex_or_gender == "m" | fixed_humans_df$fixed_sex_or_gender == "male" ~ "male",
      fixed_humans_df$fixed_sex_or_gender == "f" | fixed_humans_df$fixed_sex_or_gender == "female" ~ "female",
      fixed_humans_df$fixed_sex_or_gender == "o" | fixed_humans_df$fixed_sex_or_gender == "other" ~ "other",
      fixed_humans_df$fixed_sex_or_gender == "u" | fixed_humans_df$fixed_sex_or_gender == "uncertain" ~ "uncertain",
      TRUE ~ as.character(NA)
    )


    fixed_output_df <- sn_write_street_named_after_id(
      gisco_id = gisco_id,
      country = country,
      street_name = fixed_humans_df$street_name,
      named_after_id = named_after_id_fixed_import,
      named_after_n = fixed_humans_df$fixed_n_dedicated_to,
      category = fixed_humans_df$fixed_category,
      gender = gender_fixed_v,
      checked = TRUE,
      person = person_lv,
      session = session,
      return_df_only = return_df_only,
      disconnect_db = FALSE,
      connection = connection
    )


    return(dplyr::bind_rows(
      confirmed_output_df,
      fixed_output_df
    ))
  } else if (type == "not_humans") {
    # these are the confirmed wrong, hence id must be kept only if given in the checked column
    all_fixed_df <- relevant_df %>%
      dplyr::filter(is.na(tick_if_wrong) == FALSE)

    named_after_id_import <- tidywikidatar::tw_check_qid(
      id = all_fixed_df$fixed_named_after_id,
      non_id_as_NA = TRUE
    )

    all_fixed_df <- all_fixed_df %>%
      dplyr::mutate(fixed_sex_or_gender = dplyr::case_when(
        is.na(fixed_human) == FALSE & is.na(fixed_sex_or_gender) == TRUE & tidywikidatar::tw_check_qid(fixed_named_after_id, logical_vector = TRUE) ~
        sn_get_gender_label(
          named_after_id = fixed_named_after_id,
          language = language,
          cache_connection = connection,
          cache = TRUE,
          disconnect_db = FALSE
        ),
        TRUE ~ fixed_sex_or_gender
      ))


    gender_confirmed_v <- dplyr::case_when(
      is.na(all_fixed_df$fixed_sex_or_gender) ~ as.character(NA),
      all_fixed_df$fixed_sex_or_gender == "m" | all_fixed_df$fixed_sex_or_gender == "male" ~ "male",
      all_fixed_df$fixed_sex_or_gender == "f" | all_fixed_df$fixed_sex_or_gender == "female" ~ "female",
      all_fixed_df$fixed_sex_or_gender == "o" | all_fixed_df$fixed_sex_or_gender == "other" ~ "other",
      all_fixed_df$fixed_sex_or_gender == "u" | all_fixed_df$fixed_sex_or_gender == "uncertain" ~ "uncertain",
      TRUE ~ as.character(NA)
    )

    person_lv <- dplyr::if_else(condition = is.na(all_fixed_df$fixed_human),
      true = FALSE,
      false = TRUE
    )

    output_df <- sn_write_street_named_after_id(
      gisco_id = gisco_id,
      country = country,
      street_name = all_fixed_df$street_name,
      named_after_id = named_after_id_import,
      named_after_n = all_fixed_df$fixed_n_dedicated_to,
      category = all_fixed_df$fixed_category,
      gender = gender_confirmed_v,
      checked = TRUE,
      person = person_lv,
      session = session,
      return_df_only = return_df_only,
      connection = connection
    )

    # Confirm that non-humans are indeed non-humans
    # but no assumption that wikidata id was checked,
    # unless manually fixed

    confirmed_non_humans_df <- relevant_df %>%
      dplyr::filter(is.na(tick_if_wrong) == TRUE)


    output_confirmed_non_humans_df <- sn_write_street_named_after_id(
      gisco_id = gisco_id,
      country = country,
      street_name = confirmed_non_humans_df$street_name,
      named_after_id = tidywikidatar::tw_check_qid(
        id = confirmed_non_humans_df$fixed_named_after_id,
        non_id_as_NA = TRUE
      ) %>% as.character(),
      named_after_n = confirmed_non_humans_df$fixed_n_dedicated_to,
      category = confirmed_non_humans_df$fixed_category,
      gender = as.character(NA),
      checked = TRUE,
      person = as.numeric(0),
      session = session,
      return_df_only = return_df_only,
      connection = connection
    )

    return(dplyr::bind_rows(
      output_df,
      output_confirmed_non_humans_df
    ))
  }
}
