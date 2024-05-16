test_dimensions_and_subgroups <- function(.data, dimension_colors){
  
  dimensions <- unique(.data$dimension)
  

  one_subgroup_dim <- dimension_colors %>% 
    dplyr::filter(
      toupper(subgroup) == "ALL SUBGROUPS"
    ) %>% 
    dplyr::pull(dimension)
  
  subgroups <- .data %>% 
    dplyr::filter(!dimension %in% one_subgroup_dim) %>% 
    dplyr::pull(subgroup) %>% 
    unique()
  
  valid_dimensions <- all(dimensions %in% unique(dimension_colors$dimension))
  valid_subgroups <- all(subgroups %in% unique(dimension_colors$subgroup))
  
  valid_dimensions & valid_subgroups
}
