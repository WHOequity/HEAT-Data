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
      dplyr::mutate(popshare = population / sum(population, na.rm = TRUE))

  }


  if(table_type == "country_data"){

  }

  .data %>% ungroup()
}
