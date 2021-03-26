#' Title
#'
#' @param old_dat_path
#' @param new_dat_path
#' @param compare_var
#' @param raw_or_inequal
#' @param cutoff_for_odd
#' @param doplot
#' @param alphaval
#' @param show_example
#'
#' @return
#' @export
compare_old_and_new_data <- function(old_dat_path, new_dat_path, compare_var, raw_or_inequal = "raw", cutoff_for_odd = 1, doplot = TRUE, alphaval = 0.2, show_example = TRUE){


  old_dat <- readRDS(old_dat_path)
  new_dat <- readRDS(new_dat_path)

  join_vars <- c("country", "year", "source", "indicator_abbr", "dimension")

  if(raw_or_inequal == "raw"){
     join_vars <- c(join_vars, "subgroup")
  }else{
    old_dat$measure <- toupper(old_dat$measure)
    join_vars <- c(join_vars, "measure")
  }



  combined_dat <- inner_join(old_dat, new_dat, by = join_vars)
  message(glue::glue("Old data rows {nrow(old_dat)}, new data rows {nrow(new_dat)}; join rows {nrow(combined_dat)}"))
  message("Working on a plot")

  combined_dat$vals_from_old_data <- combined_dat[[paste0(compare_var, ".x")]]
  combined_dat$vals_from_new_data <- combined_dat[[paste0(compare_var, ".y")]]

  if(doplot){
    print(ggplot2::ggplot(combined_dat, ggplot2::aes(vals_from_old_data, vals_from_new_data)) +
            ggplot2::geom_point(alpha = alphaval)+ggplot2::ggtitle(compare_var))

  }

  combined_dat <- mutate(combined_dat, diff = (vals_from_old_data - vals_from_new_data)/vals_from_old_data)
  combined_dat$diff[is.infinite(combined_dat$diff)] <- NA
  res <- filter(combined_dat, abs(diff) > cutoff_for_odd)

  if(show_example){
    one_example <- sample_n(res, 1)

    an_example <- filter(combined_dat, country == one_example$country,
                         year == one_example$year,
                         source == one_example$source,
                         indicator_abbr == one_example$indicator_abbr,
                         dimension == one_example$dimension)

    if(raw_or_inequal == "raw"){

      an_example <- an_example %>% select(country, year, source, indicator_abbr, dimension, subgroup,  !!paste0(compare_var, ".x"), !!paste0(compare_var, ".y"))
    } else{
      an_example <- an_example %>% select(country, year, source, indicator_abbr, dimension,measure,  !!paste0(compare_var, ".x"), !!paste0(compare_var, ".y"))
    }


    print(an_example)
  }


  list(combined_dat = combined_dat, odd = res)

}





