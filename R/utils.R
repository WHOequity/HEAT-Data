
# https://stackoverflow.com/questions/41609912/remove-rows-where-all-variables-are-na-using-dplyr
drop_allNA_rows <- function(data) {
  dplyr::filter(data, Reduce(`+`, lapply(data, is.na)) != ncol(data))
}


#' Apply validation tests to the table
#'
#' @param .data
#' @param tests_to_run
#'
#' @return
#' @export
#'
#' @examples
#' var_names <- names(heatdata::HEAT_variable_descriptions)
#' var_tests <- stringr::str_extract(var_names, "^test_.*")
#' var_results <- na.exclude(var_tests)
#'
#' # test_results <- HEAT_table_validation_tests(data_heat_raw, tests)
HEAT_table_validation_tests <- function(.data, tests_to_run) {
  .tmpdata <- .data

  purrr::map_dfr(tests_to_run, function(x) {
    message(glue::glue("Working on {x}"))
    # tictoc::tic(x)
    res <- do.call(x, list(.data = .tmpdata))
    # tictoc::toc()

    tibble(
      test_name = res$func,
      passed_test = res$pass,
      namespace = res$namespace,
      subject = res$subject,
      key = res$key,
      warning_msg = res$warning_msg
    )
  })
}




assignColorsShapes <- function(.data, cutoff_for_single_color) {
  .data <- assign_palette_type(.data, cutoff_for_single_color)

  .data_colors_nonsingle <- filter(.data, palette_type != "single") %>%
    left_join(., heatdata::HEAT_palette_table,
      by = c("palette_type",
        "palette_type_group",
        "maxn" = "color_count",
        "color_order"
      )
    )

  .data_colors_single <- filter(.data, palette_type == "single") %>%
    left_join(., heatdata::HEAT_palette_table,
      by = c(
        "palette_type",
        "palette_type_group",
        "color_order"
      )
    ) %>%
    select(-color_count)


  .data <- bind_rows(
    .data_colors_nonsingle,
    .data_colors_single
  )

  .data <- assign_shapes_type(.data)

  .data
}



assign_palette_type <- function(.data, cutoff_for_single_color) {
  .data <- .data %>%
    group_by(dimension) %>%
    mutate(palette_type = case_when(
      max(maxn) <= cutoff_for_single_color & all(subgroup_order != 0) & !all(maxn == 1) ~ "sequential",
      max(maxn) <= cutoff_for_single_color & all(subgroup_order == 0) & !all(maxn == 1) ~ "qualitative",
      max(maxn) > cutoff_for_single_color | all(maxn == 1) ~ "single"
    )) %>%
    arrange(palette_type, dimension) %>%
    ungroup()

  # Observations: 2,381
  # Variables: 7
  # $ dimension      <chr> "Age", "Age", "Place of residence", "Place of residence", ...
  # $ subgroup       <fct> 15-19 years, 20-49 years, Rural, Urban, Female, Male, Quin...
  # $ subgroup_order <dbl> 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 1, 2, 3, 0, 0, 0, 0, 0, 0...
  # $ maxn           <dbl> 2, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 3, 3, 3, 95, 95, 95, 95, ...
  # $ totn           <int> 2, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 3, 3, 3, 2367, 2367, 2367...
  # $ ordered        <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE...
  # $ palette_type   <chr> "qualitative", "qualitative", "qualitative", "qualitative"...

  # Need count/sequence by palette type
  pal_type_count <- .data %>%
    distinct(dimension, palette_type) %>%
    arrange(palette_type) %>%
    group_by(palette_type) %>%
    mutate(palette_type_group = row_number())

  .data <- .data %>%
    left_join(., pal_type_count, by = c("dimension", "palette_type"))

  # Need artificial order for non-ordered joins to color (Place of residence)
  .data <- .data %>%
    arrange(dimension, subgroup_order, subgroup) %>%
    group_by(dimension, subgroup_order) %>%
    mutate(color_order = row_number())

  .data$color_order[.data$subgroup_order != 0] <- .data$subgroup_order[.data$subgroup_order != 0]
  .data$color_order[.data$palette_type == "single"] <- 1

  .data
}

assign_shapes_type <- function(.data) {
  .data
}


#' Title
#'
#' @param str
#'
#' @return
#' @export
standardize_region_text <- function(str, tolower = TRUE) {
  str[is.na(str)] <- "Missing region name"
  res <- trimws(str) %>%
    stringi::stri_trans_general("Latin-ASCII") # %>%
  # str_remove_all("\\s+")
  if(tolower){
    res <- tolower(res)
  }
  res
}




print_subregions_iso3_year <- function(whodat, dhsdat, iso3val, year1) {
  vals <- list(
    who = filter(whodat, iso3 == iso3val, year == year1) %>% arrange(str_remove(subgroup, "^[0-9]{2} ")) %>% select(subgroup, join_source) %>% data.frame(),
    dhs = filter(dhsdat, iso3 == iso3val, year == year1) %>% select(REGNAME, DHSREGEN, REGCODE) %>% arrange(REGNAME) %>% data.frame()
  )
  print(vals)
  vals
}



sample_HEAT_data <- function(.data, n_per_dimension = 10) {
  group_list <- heatdata::HEAT_create_strata_table(data_heat_raw) %>%
    group_by(dimension) %>%
    group_split()

  tmp_strata <- purrr::map_dfr(group_list, ~ dplyr::sample_n(., size = n_per_dimension))

  semi_join(.data, tmp_strata, by = c("setting", "year", "source", "dimension", "indicator_abbr"))
}
