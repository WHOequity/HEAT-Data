.translations <- new.env(parent = emptyenv())

#' @export
translate_mapping <- function() {
  if (is.null(.translations$mapping)) {
    .translations$mapping <- readr::read_csv(
      file = fs::path_package("heatdata", "langs.csv"),
      col_types = readr::cols(.default = "c")
    ) %>%
      #
      # Using english when no translated value is found
      #
      dplyr::mutate_at(
        dplyr::vars(-namespace, -subject, -key, -`english (en)`),
        ~ ifelse(is.na(.), `english (en)`, .)
      )
  }

  value_translations <- .translations$mapping %>%
    dplyr::filter(namespace == "values")

  language_columns <- value_translations %>%
    dplyr::select(-namespace, -key) %>%
    dplyr::rename(column = subject) %>%
    dplyr::rename_at(
      dplyr::vars(-column),
      ~ stringr::str_match(., "\\(([a-z]+)\\)")[, 2]
    )

  language_columns
}

#' @export
translate_heat_raw <- function(mapping = translate_mapping()) {
  setting_mapping <- mapping %>%
    dplyr::filter(column == "settings") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("setting__", .))

  source_mapping <- mapping %>%
    dplyr::filter(column == "sources") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("source__", .))

  subgroup_mapping <- mapping %>%
    dplyr::filter(column == "subgroups") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("subgroup__", .))

  indicator_mapping <- mapping %>%
    dplyr::filter(column == "indicators") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("indicator_name__", .))

  dimension_mapping <- mapping %>%
    dplyr::filter(column == "dimensions") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("dimension__", .))

  heatdata::data_heat_raw %>%
    dplyr::rename(
      setting__en = setting,
      source__en = source,
      subgroup__en = subgroup,
      indicator_name__en = indicator_name,
      dimension__en = dimension
    ) %>%
    dplyr::left_join(setting_mapping, by = "setting__en") %>%
    dplyr::left_join(source_mapping, by = "source__en") %>%
    dplyr::left_join(subgroup_mapping, by = "subgroup__en") %>%
    dplyr::left_join(indicator_mapping, by = "indicator_name__en") %>%
    dplyr::left_join(dimension_mapping, by = "dimension__en") %>%
    #
    # Account for subgroups we can't translate (?)
    #
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("subgroup__")),
      ~ ifelse(is.na(.), `subgroup__en`, .)
    )
}

#' @export
translate_inequality_measures <- function(mapping = translate_mapping()) {
  setting_mapping <- mapping %>%
    dplyr::filter(column == "settings") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("setting__", .))

  source_mapping <- mapping %>%
    dplyr::filter(column == "sources") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("source__", .))

  indicator_mapping <- mapping %>%
    dplyr::filter(column == "indicators") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("indicator_name__", .))

  dimension_mapping <- mapping %>%
    dplyr::filter(column == "dimensions") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("dimension__", .))

  heatdata::data_inequality_measures %>%
    dplyr::rename(
      setting__en = setting,
      source__en = source,
      indicator_name__en = indicator_name,
      dimension__en = dimension
    ) %>%
    dplyr::left_join(setting_mapping, by = "setting__en") %>%
    dplyr::left_join(source_mapping, by = "source__en") %>%
    dplyr::left_join(indicator_mapping, by = "indicator_name__en") %>%
    dplyr::left_join(dimension_mapping, by = "dimension__en")
}

#' @export
translate_countries <- function(mapping = translate_mapping()) {
  setting_mapping <- mapping %>%
    dplyr::filter(column == "settings") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("setting__", .))

  regions_mapping <- mapping %>%
    dplyr::filter(column == "regions") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("whoreg6_name__", .))

  income_mapping <- mapping %>%
    dplyr::filter(column == "groups") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("wbincome_name__", .))

  heatdata::data_countries %>%
    dplyr::rename(
      setting__en = setting,
      whoreg6_name__en = whoreg6_name,
      wbincome_name__en = wbincome_name
    ) %>%
    dplyr::left_join(setting_mapping, by = "setting__en") %>%
    dplyr::left_join(regions_mapping, by = "whoreg6_name__en") %>%
    dplyr::left_join(income_mapping, by = "wbincome_name__en")
}

#' @export
translate_strata <- function(mapping = translate_mapping()) {
  # setting
  setting_mapping <- mapping %>%
    dplyr::filter(column == "settings") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("setting__", .))

  source_mapping <- mapping %>%
    dplyr::filter(column == "sources") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("source__", .))

  # dimension
  dimension_mapping <- mapping %>%
    dplyr::filter(column == "dimensions") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("dimension__", .))

  # indicator_name
  indicator_mapping <- mapping %>%
    dplyr::filter(column == "indicators") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("indicator_name__", .))

  heatdata::data_heat_strata %>%
    dplyr::rename(
      setting__en = setting, source__en = source, dimension__en = dimension,
      indicator_name__en = indicator_name
    ) %>%
    dplyr::left_join(setting_mapping, by = "setting__en") %>%
    dplyr::left_join(source_mapping, by = "source__en") %>%
    dplyr::left_join(dimension_mapping, by = "dimension__en") %>%
    dplyr::left_join(indicator_mapping, by = "indicator_name__en")
}

#' @export
translate_setting_year_source <- function(mapping = translate_mapping()) {
  setting_mapping <- mapping %>%
    dplyr::filter(column == "settings") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("setting__", .))

  source_mapping <- mapping %>%
    dplyr::filter(column == "sources") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("source__", .))

  heatdata::info_setting_yr_src %>%
    dplyr::rename(setting__en = setting, source__en = source) %>%
    dplyr::left_join(setting_mapping, by = "setting__en") %>%
    dplyr::left_join(source_mapping, by = "source__en")
}

#' @export
translate_subset <- function(data, language = "en") {
  if (missing(data)) {
    stop("Missing `data` argument")
  }

  lang_suffix <- paste0("__", language)

  data %>%
    dplyr::select(everything(), -contains("__"), ends_with(lang_suffix)) %>%
    dplyr::rename_at(
      dplyr::vars(contains("__")),
      ~ stringr::str_remove(., "__[a-z]+$")
    )
}
