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
translate_heat_raw <- function(.data, mapping = translate_mapping(), lstext = "hem") {
  
  setting_mapping <- mapping %>%
    dplyr::filter(column == "settings") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("setting__", .)) %>% 
    dplyr::distinct()

  source_mapping <- mapping %>%
    dplyr::filter(column == "sources") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("source__", .))%>% 
    dplyr::distinct()

  subgroup_mapping <- mapping %>%
    dplyr::filter(column == "subgroups") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("subgroup__", .))%>% 
    dplyr::distinct()

  indicator_mapping <- mapping %>%
    dplyr::filter(column == "indicators") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("indicator_name__", .))%>% 
    dplyr::distinct()

  dimension_mapping <- mapping %>%
    dplyr::filter(column == "dimensions") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("dimension__", .))%>% 
    dplyr::distinct()

  
  
.data %>% 
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
    dplyr::left_join(dimension_mapping, by = "dimension__en") |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("subgroup__")),
      ~ ifelse(is.na(.), `subgroup__en`, .)
    ) |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("setting__")),
      ~ ifelse(is.na(.), `setting__en`, .)
    ) |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("source__")),
      ~ ifelse(is.na(.), `source__en`, .)
    ) |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("indicator_name__")),
      ~ ifelse(is.na(.), `indicator_name__en`, .)
    ) |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("dimension__")),
      ~ ifelse(is.na(.), `dimension__en`, .)
    )
}

#' @export
translate_inequality_measures <- function(.data, mapping = translate_mapping()) {
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

  .data %>% 
    dplyr::rename(
      setting__en = setting,
      source__en = source,
      indicator_name__en = indicator_name,
      dimension__en = dimension
    ) %>%
    dplyr::left_join(setting_mapping, by = "setting__en") %>%
    dplyr::left_join(source_mapping, by = "source__en") %>%
    dplyr::left_join(indicator_mapping, by = "indicator_name__en") %>%
    dplyr::left_join(dimension_mapping, by = "dimension__en") |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("setting__")),
      ~ ifelse(is.na(.), `setting__en`, .)
    ) |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("source__")),
      ~ ifelse(is.na(.), `source__en`, .)
    ) |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("indicator_name__")),
      ~ ifelse(is.na(.), `indicator_name__en`, .)
    ) |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("dimension__")),
      ~ ifelse(is.na(.), `dimension__en`, .)
    )
}

#' @export
translate_countries <- function(.data, mapping = translate_mapping(), lstext = "hem") {
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

  .data %>%
    dplyr::rename(
      setting__en = setting,
      whoreg6_name__en = whoreg6_name,
      wbincome_name__en = wbincome_name
    ) %>%
    dplyr::left_join(setting_mapping, by = "setting__en") %>%
    dplyr::left_join(regions_mapping, by = "whoreg6_name__en") %>%
    dplyr::left_join(income_mapping, by = "wbincome_name__en") |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("setting__")),
      ~ ifelse(is.na(.), `setting__en`, .)
    )
}

#' @export
translate_strata <- function(.data, mapping = translate_mapping(), lstext = "hem") {
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

  .data %>%
    dplyr::rename(
      setting__en = setting, source__en = source, dimension__en = dimension,
      indicator_name__en = indicator_name
    ) %>%
    dplyr::left_join(setting_mapping, by = "setting__en") %>%
    dplyr::left_join(source_mapping, by = "source__en") %>%
    dplyr::left_join(dimension_mapping, by = "dimension__en") %>%
    dplyr::left_join(indicator_mapping, by = "indicator_name__en") |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("setting__")),
      ~ ifelse(is.na(.), `setting__en`, .)
    ) |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("source__")),
      ~ ifelse(is.na(.), `source__en`, .)
    ) |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("indicator_name__")),
      ~ ifelse(is.na(.), `indicator_name__en`, .)
    ) |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("dimension__")),
      ~ ifelse(is.na(.), `dimension__en`, .)
    )
}

#' @export
translate_setting_year_source <- function(.data, mapping = translate_mapping(), lstext = "hem") {
  setting_mapping <- mapping %>%
    dplyr::filter(column == "settings") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("setting__", .))

  source_mapping <- mapping %>%
    dplyr::filter(column == "sources") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("source__", .))

  .data %>%
    dplyr::rename(setting__en = setting, source__en = source) %>%
    dplyr::left_join(setting_mapping, by = "setting__en") %>%
    dplyr::left_join(source_mapping, by = "source__en") |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("setting__")),
      ~ ifelse(is.na(.), `setting__en`, .)
    ) |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("source__")),
      ~ ifelse(is.na(.), `source__en`, .)
    )
}

#' @export
translate_determinants<- function(.data, mapping = translate_mapping(), lstext = "hem") {
  
  setting_mapping <- mapping %>%
    dplyr::filter(column == "settings") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("setting__", .)) %>% 
    dplyr::distinct()

  sdhsource_mapping <- mapping %>%
    dplyr::filter(column == "sdh_sources") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("sdh_source__", .))%>% 
    dplyr::distinct()


  sdhname_mapping <- mapping %>%
    dplyr::filter(column == "sdh_names") %>%
    dplyr::select(-column) %>%
    dplyr::rename_all(~ paste0("sdh_name__", .))%>% 
    dplyr::distinct()

  
.data %>% 
    dplyr::rename(
      setting__en = setting,
      sdh_source__en = sdh_source,
      sdh_name__en = sdh_name,
    ) %>%
    dplyr::left_join(setting_mapping, by = "setting__en") %>%
    dplyr::left_join(sdhsource_mapping, by = "sdh_source__en") %>%
    dplyr::left_join(sdhname_mapping, by = "sdh_name__en") %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("setting__")),
      ~ ifelse(is.na(.), `setting__en`, .)
    ) |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("sdh_source__")),
      ~ ifelse(is.na(.), `sdh_source__en`, .)
    ) |> 
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("sdh_name__")),
      ~ ifelse(is.na(.), `sdh_name__en`, .)
    ) 
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


#' R run translations on existing datasets so you don't
#' have to run through the full data creation process
#'
#' @param directory 
#'
#' @return
#' @export
#'
#' @examples
#' re_translate('inst/datasets')
re_translate <- function(directory, minutes_since_cng = 30){
  
  
  # In a rush here so repeating for loops!
  just_in_case_path <- "~/junk/tmp_heatdata_incase"
  files <- fs::dir_ls(directory)
  fs::dir_copy(directory, just_in_case_path)
  
  start_file_info <- fs::dir_info("inst/datasets")
  
  # Paths
  files_raw <- files[grepl("data_raw", files)]
  files_ineq <- files[grepl("_inequality_measures", files)]
  files_countries <- files[grepl("data_countries", files)]
  files_strata <- files[grepl("data_strata", files)]
  files_setyrsrc <- files[grepl("info_setting_yr_src", files)]
  
  if(!all.equal(
    length(files_raw),
    length(files_ineq),
    length(files_countries),
    length(files_strata),
    length(files_setyrsrc)
  ))
    stop("Number of files is not the same")
  
  # Raw data

  
  for(i in 1:length(files_raw)){
    cur_raw_path <- files_raw[i]
    
    message("Raw: ", fs::path_file(cur_raw_path))
    
    cur_raw <- arrow::read_parquet(cur_raw_path)
    names(cur_raw) <- gsub("__en", "", names(cur_raw))
    cur_raw <- cur_raw |> 
      dplyr::select(!matches("__[a-z]{2}"))
    
    cur_raw <- translate_heat_raw(cur_raw)
    arrow::write_parquet(cur_raw, cur_raw_path)
    
  }
  
  rm(i)
  
  # Inequality measures

  
  for(j in 1:length(files_ineq)){
    cur_ineq_path <- files_ineq[j]
    
    message("Inequality: ", fs::path_file(cur_setyrsrc_path))
    
    cur_ineq <- arrow::read_parquet(cur_ineq_path)
    names(cur_ineq) <- gsub("__en", "", names(cur_ineq))
    cur_ineq <- cur_ineq |> 
      dplyr::select(!matches("__[a-z]{2}"))
    
    cur_ineq <- translate_inequality_measures(cur_ineq)
    arrow::write_parquet(cur_ineq, cur_ineq_path)
    
  }
  
  rm(j)
  
  # Countries

  
  for(k in 1:length(files_countries)){
    cur_countries_path <- files_countries[k]
    
    message("Countries: ", fs::path_file(cur_countries_path))
    
    cur_countries <- arrow::read_parquet(cur_countries_path)
    names(cur_countries) <- gsub("__en", "", names(cur_countries))
    cur_countries <- cur_countries |> 
      dplyr::select(!matches("__[a-z]{2}"))
    
    cur_countries <- translate_countries(cur_countries)
    arrow::write_parquet(cur_countries, cur_countries_path)
    
  }
  
  rm(k)
  
  # Strata

  
  for(l in 1:length(files_strata)){
    cur_strata_path <- files_strata[l]
    
    message("Strata: ", fs::path_file(cur_strata_path))
    
    cur_strata <- arrow::read_parquet(cur_strata_path)
    names(cur_strata) <- gsub("__en", "", names(cur_strata))
    cur_strata <- cur_strata |> 
      dplyr::select(!matches("__[a-z]{2}"))
    
    cur_strata <- translate_strata(cur_strata)
    arrow::write_parquet(cur_strata, cur_strata_path)
    
  }
  
  rm(l)
  
  # info_setting_yr_src

  
  for(m in 1:length(files_setyrsrc)){
    cur_setyrsrc_path <- files_setyrsrc[m]
    
    message("Setting info: ", fs::path_file(cur_setyrsrc_path))
    
    cur_setyrsrc <- arrow::read_parquet(cur_setyrsrc_path)
    names(cur_setyrsrc) <- gsub("__en", "", names(cur_setyrsrc))
    cur_setyrsrc <- cur_setyrsrc |> 
      dplyr::select(!matches("__[a-z]{2}"))
    
    cur_setyrsrc <- translate_setting_year_source(cur_setyrsrc)
    arrow::write_parquet(cur_setyrsrc, cur_setyrsrc_path)
    
  }
  
  rm(m)
  
  # n_files_changed <- fs::dir_info("inst/datasets") |> 
  #   dplyr::filter(modification_time > Sys.time() - minutes_since_cng * 60) |> 
  #   nrow()
  
  end_file_info <- fs::dir_info("inst/datasets")
  
  start_file_info$modification_after <- end_file_info$modification_time
  
  vals <- start_file_info$modification_after - start_file_info$modification_time
  should_have_changed <- length(files_raw) * 5
  
  if(length(vals != 0) != should_have_changed)
    message("Not all files got changed")
  
  # fs::dir_info("inst/datasets") |> 
  #   dplyr::filter(modification_time > Sys.time() - minutes_since_cng * 60) |> 
  #   View()
  start_file_info
  
}


