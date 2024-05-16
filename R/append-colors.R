
#' Add colors to the data
#'
#' @param .data 
#' @param dimension_colors color table
#'
#' @return
#' @export
#'
#' @examples
appendColors <- function(.data, dimension_colors) {
  
  # To assign colors to dimensions
  dimension_colors <- dimension_colors %>%
    dplyr::select(-dimension, -subgroup)
  
  one_col <- dimension_colors %>%
    dplyr::filter(grepl("all_subgroups", tolower(subgroup_id)))
  
  multi_col <- dimension_colors %>%
    dplyr::filter(!grepl("all_subgroups", tolower(subgroup_id)))
  
  
  .data <-  dplyr::left_join(.data, multi_col, by = c("dimension_id", "subgroup_id"))
  
  for(i in 1:nrow(one_col)){
    dimen_id <- one_col$dimension_id[i]
    onecol <- one_col$color[i]
    oneopacity <- one_col$opacity[i]
    .data$color[.data$dimension_id == dimen_id] <- onecol
    .data$opacity[.data$dimension_id == dimen_id] <- oneopacity
  }
  
  .data %>%
    dplyr::mutate(
      opacity = ifelse(is.na(opacity), 1, opacity),
      color = highcharter::hex_to_rgba(color, opacity)
    )
}
