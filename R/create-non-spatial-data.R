#' Title
#'
#' @param data_heat_raw 
#' @param data_countries 
#' @param dimension_colors 
#' @param development 
#'
#' @return
#' @export
#'
#' @examples
create_non_spatial_data <- function(data_heat_raw, 
                                    data_countries, 
                                    dimension_colors, 
                                    dataset_name,
                                    development = FALSE) {


  # ├ Add strata id ---- 
  data_heat_raw <- HEAT_rename_variables(data_heat_raw, table_type = "heat_data")
  data_heat_raw <- heatmeasures::add_strata_id(data_heat_raw)

  # ├ Check for required variables ----
  has_req_vars <- heatdata::test_has_required_variables(data_heat_raw)
  
  valid_dimensions_and_subgroups <- test_dimensions_and_subgroups(data_heat_raw, dimension_colors)
  
  #browser()
  if(!valid_dimensions_and_subgroups)
     stop(stoptxt("Not all of the dimensions exist in the dimensions table"))

  if (!has_req_vars$pass) {
    stop(stoptxt("The data does not have the required variables"))
  }

  message(infotxt("Begin data fixes"))
  data_heat_raw <- HEAT_data_fixes(data_heat_raw, table_type = "heat_data") # remove missing rows
  data_heat_raw <- HEAT_force_variable_types(data_heat_raw, table_type = "heat_data")
  data_heat_raw <- HEAT_drop_defective_strata(data_heat_raw)
  data_heat_raw <- HEAT_data_add_variables(data_heat_raw, table_type = "heat_data")
  data_heat_raw <- data_heat_raw %>% ungroup()
  
  one_col <- dimension_colors %>%
    dplyr::filter(grepl("all_subgroups", tolower(subgroup_id)))
  
  multi_col <- dimension_colors %>%
    dplyr::filter(!grepl("all_subgroups", tolower(subgroup_id)))

  data_heat_raw <-  dplyr::left_join(data_heat_raw, multi_col, by = c("dimension", "subgroup")) %>% 
    dplyr::select(-color, -opacity)


  if(nrow(one_col) > 0){
    for(i in 1:nrow(one_col)){
      dimen <- one_col$dimension[i]
      dimen_id <- one_col$dimension_id[i]
      data_heat_raw$dimension_id[data_heat_raw$dimension == dimen] <- dimen_id
    }
  }
  


  message(infotxt("Creating auxiliary files"))
  data_heat_strata <- HEAT_create_strata_table(data_heat_raw) %>% ungroup()
  info_setting_yr_src <- HEAT_create_setyrsrc_table(data_heat_raw) %>% ungroup()
  info_dimensions <- HEAT_create_dimension_table(data_heat_raw, cutoff_for_single_color = 7) %>% ungroup()
  info_subregion_minmax <- HEAT_create_subregionminmax_table(data_heat_raw) %>% ungroup()


  message(infotxt("Begin working with country file"))
  data_countries <- HEAT_data_fixes(data_countries, table_type = "country_data") # remove missing rows
  data_countries <- HEAT_force_variable_types(data_countries, table_type = "country_data")
  data_countries <- HEAT_rename_variables(data_countries, table_type = "country_data")
  data_countries <- HEAT_data_add_variables(data_countries, table_type = "country_data")
  data_countries <- data_countries %>% ungroup()

  data_countries <- dplyr::distinct(data_heat_raw, setting, iso3) %>%
    dplyr::left_join(dplyr::select(data_countries, -setting), by = "iso3") %>%
    dplyr::relocate(setting) %>%
    dplyr::mutate(
      whoreg6 = dplyr::if_else(
        is.na(whoreg6_name),
        true = "No WHO region defined",
        false = whoreg6
      ),
      whoreg6_name = dplyr::if_else(
        is.na(whoreg6_name),
        true = "No WHO region defined",
        false = whoreg6_name
      ),
      wbincome = dplyr::if_else(
        is.na(wbincome_name),
        true = "No income group defined",
        false = wbincome
      ),
      wbincome_name = dplyr::if_else(
        is.na(wbincome_name),
        true = "No income group defined",
        false = wbincome_name
      )
    )




  # ├ Tests in prep for inequality measures ----
  tests <- stringr::str_extract(
    names(heatdata::HEAT_variable_descriptions),
    "^test_.*"
  ) %>% na.exclude()

  test_results <- HEAT_table_validation_tests(data_heat_raw, tests)
  test_results



  if (!all(test_results$passed_test)) {
    stop(stoptxt("Not all tests were passed"))
  }

  message(infotxt("Begin computing heat measures"))


  if (!development) {
    data_inequality_measures <- heatmeasures::HEAT_measures_full_process(data_heat_raw) %>% ungroup()
  } else {
    message("In development mode, skipping calculation of inequality measures")

    tmpfile <- glue::glue("inst/datasets/{dataset_name}_data_inequality_measures.parquet")
    if(!fs::file_exists(tmpfile))
      tmpfile <-"inst/datasets/rep_rmnch_data_inequality_measures.parquet"

    data_inequality_measures <- arrow::read_parquet(tmpfile)
    #data_inequality_measures <- get(paste0("data_", dataset_name))$data_inequality_measures
  }
  

  data_heat_raw <- data_heat_raw |> 
    appendColors(info_dimension_colors)
  
  message(infotxt("Begin translating"))
  
  data_heat_raw <- translate_heat_raw(data_heat_raw)
  

  if(!"setting__en" %in% names(data_inequality_measures)){
    data_inequality_measures <- translate_inequality_measures(data_inequality_measures)
  }

  data_countries <- translate_countries(data_countries)
  data_heat_strata <- translate_strata(data_heat_strata)
  info_setting_yr_src <- translate_setting_year_source(info_setting_yr_src)


  info_date_to_integer <- data_heat_raw %>%
    dplyr::distinct(year, year_int, year_alt, year_int_alt) |> 
    dplyr::arrange(year_int)
  
  data_inequality_measures <- dplyr::left_join(data_inequality_measures, info_date_to_integer, by = "year")
  
  data_setting_avg <- data_heat_raw |> 
    dplyr::select(
      iso3,
      year,
      year_int,
      year_alt,
      year_int_alt,
      setting_average,
      dplyr::contains("setting"),
      dplyr::contains("source"),
      indicator_abbr,
      dplyr::contains("indicator_name"),
    ) |> 
    dplyr::mutate(year = as.character(year)) |> 
    dplyr::distinct()
  
  
  message(infotxt("Returning results"))
  list(
    data_raw = data_heat_raw,
    data_inequality_measures = data_inequality_measures,
    data_countries = data_countries,
    data_strata = data_heat_strata,
    data_setting_avg = data_setting_avg,
    info_date_to_integer = info_date_to_integer,
    info_setting_yr_src = info_setting_yr_src,
    info_dimensions = info_dimensions,
    info_subregion_minmax = info_subregion_minmax
  )
}
