#' Create the strata table (unique set of strata)
#'
#' @description this function makes use of the heatmeasures::strata_variables for the
#' definition of what is a strata
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
HEAT_create_strata_table <- function(.data){

  .data %>%
    select(!!heatmeasures::strata_variables, strata_id) %>%
    distinct()
}


#' Creates the dimension table including adding the colors and shapes for dimension subgroups
#'
#' @param .data
#' @param cutoff_for_single_color if the number of subgroups in a dimension is less than
#' or equal to this number then each subgroup will get a different color. Otherwise all
#' subgroups get the same color. So if the cutoff is 7 all dimensions with 7 or fewer
#' subgroups will get different colors (either sequential or qualitative) all dimensions
#' with 8 or more subgroups will be assigned a single color
#'
#' @return
#' @export
#'
#' @examples
HEAT_create_dimension_table <- function(.data, cutoff_for_single_color = 7){

  # ZEV re-write this 2018-02-12 to make it easier to understand. The main goal
  # is to add a "maxn" field which has the highest number of subgroups for each
  # dimension. So, for example, if you have one country with 200 subnational regions
  # maxn for the dimension subnational region would be 200.

  # 1. Select a unique set of dimensions and their subgroups
  # 2. Compute a count of subgroups by country by dimension
  # 3. Then identify the max count by dimension
  # 4. Drop country and count



  dimensions <- .data %>%
    select(setting, dimension, subgroup, subgroup_order) %>%
    distinct() %>% # unique dimension/subgroups
    group_by(setting, dimension) %>%
    mutate(cnt = n()) %>% # number of subgroups by country/dimension
    ungroup() %>%
    group_by(dimension) %>%
    mutate(maxn = max(cnt)) %>% # maximum subgroups per dimension (no country)
    ungroup() %>%
    select(-setting, - cnt) %>%
    distinct()

  #.rdata[['dimension_details']] <- assignColorsShapes(.rdata[['dimension_details']])

  # TODO: need something like this

  # .rdata[['dimension_summary']] <- select(.rdata[['dimension_details']], dimension, order, maxn) %>%
  #   mutate(ordered = order != 0, just2_subgroups = maxn <= 2) %>% select(-order, -maxn) %>% ungroup %>%  distinct
  #
  #
  # .rdata[['equity_dimensions']] <- sort(unique(.rdata[['dimension_details']]$dimension))
  # .rdata[['geo_dimension']] <- unique(.rdata[['dimension_details']]$dimension[.rdata[['dimension_details']]$dimension_type=="region"])
  #

  assignColorsShapes(dimensions, cutoff_for_single_color)

}


#' Create a table with the unique set of setting, year and source
#'
#' @description the actual names of the setting, year and source variables (which
#' might be setting, year and source) are in the heatmeasures::setting_year_source_variables
#' vector
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
HEAT_create_setyrsrc_table <- function(.data){

  vars <- heatmeasures::setting_year_source_variables
  .data %>%
    dplyr::select(!!vars) %>%
    dplyr::distinct() %>% 
    dplyr::arrange(!!sym(vars[1]), !!sym(vars[2]), !!sym(vars[3]))
}


#' Compute the table of subregion min and max values for the mapping (only required for HEAT)
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
HEAT_create_subregionminmax_table <- function(.data){


  filter(.data, dimension == "Subnational region") %>%
    group_by(setting, source, indicator_abbr, indicator_name) %>%
    summarise(
      min = min(estimate, na.rm = TRUE),
      max = max(estimate, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      min = 0, # if_else(indicator_abbr %in% outcome_indicators, min, 0),
      max = if_else(indicator_abbr %in% heatdata::HEAT_indicators_max100, max, 100)
    ) %>%
    select(-indicator_name)
}




#' Title
#'
#' @param .data
#' @param use_heatdata_strata
#' @param setting_choice
#' @param dimension_choice
#' @param indicator_choice
#' @param n_per_dimension
#'
#' @return
#' @export
HEAT_create_random_sample <- function(.data = heatdata::data_heat_raw,
                                      use_heatdata_strata = TRUE,
                                      setting_choice = NULL,
                                      dimension_choice = NULL,
                                      indicator_choice = NULL,
                                      n_per_dimension = NULL,
                                      intentional_errors_to_add = NULL){

  if(use_heatdata_strata){
    .strata <- heatdata::data_heat_strata
  } else {
    .strata <- distinct(.data, setting, source, year, indicator_abbr, dimension)
  }

  if(!is.null(setting_choice)) .strata <- filter(.strata, setting %in% setting_choice)
  if(!is.null(indicator_choice)) .strata <- filter(.strata, indicator_abbr %in% indicator_choice)
  if(!is.null(dimension_choice)) .strata <- filter(.strata, dimension %in% dimension_choice)


  if(!is.null(n_per_dimension)){
    .strata <- .strata %>%
      group_by(dimension) %>%
      sample_n(n_per_dimension)
  }






  .data <- .data %>%
    semi_join(.strata, by = c("setting", "source", "year", "indicator_abbr", "dimension"))

  if(!is.null(intentional_errors_to_add)){


    if("test_key_vars_not_empty" %in% intentional_errors_to_add){
      varname <- select(heatdata::HEAT_variable_descriptions, VARIABLE, test_key_vars_not_empty) %>%
        filter(test_key_vars_not_empty) %>%
        pull(VARIABLE) %>%
        sample(1)

      .data[[varname]] <- NA
    }

    if("test_is_binary_or_NA" %in% intentional_errors_to_add){

      varname <- select(heatdata::HEAT_variable_descriptions, VARIABLE, test_is_binary_or_NA) %>%
        filter(test_is_binary_or_NA) %>%
        pull(VARIABLE) %>%
        sample(1)

      .data[sample(1:nrow(.data), 1), varname] <- sample(2:100, 1)
    }

    if("test_chars_are_chars" %in% intentional_errors_to_add){
      varname <- select(heatdata::HEAT_variable_descriptions, VARIABLE, test_chars_are_chars) %>%
        filter(test_chars_are_chars) %>%
        pull(VARIABLE) %>%
        sample(1)

      .data[sample(1:nrow(.data), 1), varname] <- sample(2:100, 1)
    }



    if("test_vars_3chars" %in% intentional_errors_to_add){
      varname <- select(heatdata::HEAT_variable_descriptions, VARIABLE, test_vars_3chars) %>%
        filter(test_vars_3chars) %>%
        pull(VARIABLE) %>%
        sample(1)

      .data[sample(1:nrow(.data), 1), varname] <- "SHOULD BE 3 DIGITS"
    }


    if("test_nums_not_char" %in% intentional_errors_to_add){
      varname <- select(heatdata::HEAT_variable_descriptions, VARIABLE, test_nums_not_char) %>%
        filter(test_nums_not_char) %>%
        pull(VARIABLE) %>%
        sample(1)

      .data[sample(1:nrow(.data), 1), varname] <- "SHOULDN'T BE CHARACTER"
    }


    if("test_vars_4chars" %in% intentional_errors_to_add){
      varname <- select(heatdata::HEAT_variable_descriptions, VARIABLE, test_vars_4chars) %>%
        filter(test_vars_4chars) %>%
        pull(VARIABLE) %>%
        sample(1)

      if(class(.data[[varname]]) == "character") val <- "SHOULD BE LENGTH 4"
      if(class(.data[[varname]]) %in% c("numeric", "integer")) val <- 999999

      .data[sample(1:nrow(.data), 1), varname] <- val
    }



    if("test_no_decimals" %in% intentional_errors_to_add){


      varname <- select(heatdata::HEAT_variable_descriptions, VARIABLE, test_no_decimals) %>%
        filter(test_no_decimals) %>%
        pull(VARIABLE) %>%
        sample(1)


      .data[sample(1:nrow(.data), 1), varname] <- pi
    }


    if("test_all_nonnegative_or_NA" %in% intentional_errors_to_add){


      varname <- select(heatdata::HEAT_variable_descriptions, VARIABLE, test_all_nonnegative_or_NA) %>%
        filter(test_all_nonnegative_or_NA) %>%
        pull(VARIABLE) %>%
        sample(1)


      .data[sample(1:nrow(.data), 1), varname] <- -10
    }


    if("test_all_positive_or_NA" %in% intentional_errors_to_add){


      varname <- select(heatdata::HEAT_variable_descriptions, VARIABLE, test_all_positive_or_NA) %>%
        filter(test_all_positive_or_NA) %>%
        pull(VARIABLE) %>%
        sample(1)


      .data[sample(1:nrow(.data), 1), varname] <- 0
    }

    # This one is hard-coded
    if("test_strata_ordered_andref" %in% intentional_errors_to_add){

      strata1 <- sample(.data$strata_id, 1)

      strata1 <- filter(.data, strata_id == strata1)

      # randomly select error to add
      if(sample(c(TRUE, FALSE), 1)){

        strata1$ordered_dimension <- 0
        strata1$subgroup_order <- 1:nrow(strata1)


      }else{

        strata1$ordered_dimension <- 1
        strata1$subgroup_order <- 1:nrow(strata1)
        strata1$reference_subgroup[1] <- 1

      }

      .data <- bind_rows(
        .data,
        strata1
      )

    }


    if("test_strata_subgroup_sequence" %in% intentional_errors_to_add){

      .data$subgroup_order[.data$subgroup_order == 2][1] <- 99

    }



    if("test_setting_iso3_match" %in% intentional_errors_to_add){

      .data$iso3[sample(1:nrow(.data), 1)] <- "ZZZ"

    }


    if("test_indicator_unique_names" %in% intentional_errors_to_add){

      .data$indicator_abbr[sample(1:nrow(.data), 1)] <- "Bad indic abbreviation"

    }

    if("test_consistent_indicinfo" %in% intentional_errors_to_add){

      .data$favourable_indicator[.data$favourable_indicator == 1][1] <- 0

    }


    if("test_consistent_ordered_dimension" %in% intentional_errors_to_add){

      .data$ordered_dimension[.data$favourable_indicator == 0][1] <- 1

    }







  }

  return(.data)

}

