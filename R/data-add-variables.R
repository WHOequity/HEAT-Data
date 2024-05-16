#' Add required new variables to the dataset
#'
#' @param .data input data
#' @param table_type either "who_data" or "country_data"
#'
#' @return
#' @export
HEAT_data_add_variables <- function(.data, table_type = "all"){


  if(table_type == "heat_data"){

    .data <- .data %>%
      dplyr::group_by(setting, year, source, indicator_abbr, dimension) %>%
      dplyr::mutate(popshare = population / sum(population, na.rm = TRUE)) |> 
      dplyr::mutate(year_alt = substring(year, 1, 4))
    
    
    # We changed year to be character so I'm adding a year_int
    # to address the distance measures in the compare section
    year_distinct <- .data %>% 
      dplyr::ungroup() %>% 
      dplyr::distinct(year) %>% 
      dplyr::arrange(year) %>% 
      dplyr::mutate(year_int = as.numeric(factor(year, levels = unique(year))))
    
    
    # git909 yuck!
    year_distinct_alt <- .data %>% 
      dplyr::ungroup() %>% 
      dplyr::distinct(year_alt) %>% 
      dplyr::arrange(year_alt) %>% 
      dplyr::mutate(year_int_alt = as.numeric(factor(year_alt, levels = unique(year_alt))))
    
    
    .data <- dplyr::inner_join(.data, year_distinct, by = "year")
    .data <- dplyr::inner_join(.data, year_distinct_alt, by = "year_alt")
    .data

  }


  if(table_type == "country_data"){

  }

  .data %>% ungroup()
}
