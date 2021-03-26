#' Apply specific data fixes to raw data
#'
#' @description These might include fixes that need to be applied to all data (remove rows
#' with all NA values) or fixes that should
#'
#' @param .data
#' @param table_type "country_data", "heat_data" (internal database)
#'
#' @return
#' @export
HEAT_data_fixes <- function(.data, table_type = "all"){

  .data <- drop_allNA_rows(.data)


  if(table_type == "heat_data"){
    # drop xtra variables
    keepvars <- names(.data)%in%c(heatdata::HEAT_variable_descriptions$VARIABLE, "strata_id")
    .data <- .data[,keepvars]

    # git 1167 one indicator ends with "(%"
    # .data <- .data %>%
    #   mutate(indicator_name = stringr::str_replace(indicator_name, "\\(%$", "(%)"))
  }


  if(table_type == "country_data"){

  }

  .data
}


#' Rename a table's variables
#'
#' @description Occasionally the database we've received has a slightly different name than the app expects
#' and we need to alter it to fit.
#'
#' @param .data
#' @param table_type
#'
#' @return
#' @export
HEAT_rename_variables <- function(.data, table_type = "all"){

  if(table_type == "country_data"){

    .data <- .data %>% rename(
                        whoreg6 = whoreg,
                        whoreg6_name = whoreg_name
                        )

    # TODO: Not sure if this is needed
    #countries$wbincome <- countries$wbincome_name

  }

  if(table_type == "heat_data"){

  }

  .data
}



#' Drop strata with all missing estimates
#'
#' @param .data
#'
#' @return
# © Copyright World Health Organization (WHO) 2016-2021.
# This file is part of the WHO Health Equity Assessment Toolkit 
# (HEAT and HEAT Plus), a software application for assessing 
# health inequalities in countries.
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
# 
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>. 

#' @export
HEAT_drop_defective_strata <- function(.data){


    filter(.data, !is.na(estimate)) %>%
      select(strata_id) %>%
      distinct() %>%
      semi_join(.data, ., by = "strata_id")



}


#' Force variables to be the types they're supposed to be
#'
#' @param .data
#'
#' @return
#' @export
HEAT_force_variable_types <- function(.data, table_type = "all") {
  if (table_type == "heat_data") {
    stopifnot(is.data.frame(.data))
    # if (!"data.frame"%in%class(.data)) stop(".data must be a data.frame")

    formats <- split(heatdata::HEAT_variable_descriptions$VARIABLE,
                     heatdata::HEAT_variable_descriptions$Format)
    existing_class <- purrr::map_chr(.data, class)

    types <- c("integer", "numeric", "character")
    adjusted_formats <- purrr::map(types, function(x) {
      current_vals <- names(existing_class[existing_class == x])

      formats[[x]][!(formats[[x]] %in% current_vals)]
    })

    names(adjusted_formats) <- types


    .data <- dplyr::mutate_at(.data, .vars = vars(adjusted_formats$integer), .funs = as.integer)
    .data <- dplyr::mutate_at(.data, .vars = vars(adjusted_formats$numeric), .funs = as.numeric)
    .data <- dplyr::mutate_at(.data, .vars = vars(adjusted_formats$character), .funs = funs(trimws(as.character(.))))

  }

  if (table_type == "country_data") {

  }

  .data
}
