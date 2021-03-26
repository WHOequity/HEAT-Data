#' Title
#'
#' @param sel a selenium connection sel <- rsDriver(port = 3000L, browser = "chrome")
#' @param country_id
#' @param spatialdir
#'
#' @return
#' @export
survey_boundary_prep <- function(sel, country_id, spatialdir, download_folder = "~/Downloads"){

  if(length(fs::dir_ls(download_folder, glob =  "**/sdr_*.zip"))>0)
    stop("There is already a zip file in the download folder")

  url <- sprintf("https://spatialdata.dhsprogram.com/boundaries/#view=table&countryId=%s", country_id)

  message(glue::glue("Working on country: {country_id}"))

  sel$client$navigate(url)
  sel$client$refresh()
  Sys.sleep(3)

  surveyinfo <- sel$client$findElements(using = "xpath", value = "//div[@data-bind = 'text: survey, css:{hide: !survey}']")
  surveyinfo <- purrr::map(surveyinfo, ~.$getElementText()) %>% unlist()

  message(glue::glue("The surveys listed are: {paste(surveyinfo, collapse = '; ')}"))

  # Will only create if it doesn't existy
  newpath <- fs::path(spatialdir, country_id)
  newpath %>% dir_create()




  links <- sel$client$findElements(using = "css", ".download-boundaries")

  message(glue::glue("Number of ZIP links is {length(links)}"))

  cat("Number of folders for ", country_id, "should be ", length(links), file = paste0(as.character(newpath), "/country_readme.txt"))

  if (!length(links)) {
    stop("No links to download")
  }

  if(length(links) != length(surveyinfo)) stop("Number of surveys and links do not match")

  # I separated map so that all folders are created no matter what
  map(1:length(links), function(i){
    shppath <- fs::path(newpath, surveyinfo[i]) %>% as.character()
    fs::dir_create(shppath)
  })


  map(1:length(links), function(i){
    survey_boundary_download(sel, links[[i]])
    survey_boundary_move_unzip(newpath, surveyinfo[i], download_folder = download_folder)
  })



}


#' survey_boundary_download
#'
#' @param sel
#' @param link
#'
#' @return
#' @export
survey_boundary_download <- function(sel, link){
  # a link must be visible before it can be clicked, selenium converts the
  # element reference in to a DOM element and then the script calls
  # `scrollIntoView()` on the element`

  sel$client$executeScript("arguments[0].scrollIntoView();", args = list(link))
  Sys.sleep(2)

  # The click does not always work. This was most often because of an obscure
  # selenium error. The warning or error is logged.

  msg <- tryCatch(
    link$clickElement(),
    error = function(e) e$message,
    warning = function(e) e$message
  )
  Sys.sleep(15)

  return(msg)
}


#' Title
#'
#' @param newpath
#' @param survey
#' @param download_folder
#' @param search_pattern
#'
#' @return
#' @export
survey_boundary_move_unzip <- function(newpath, survey, download_folder = "~/Downloads", search_pattern = "**/sdr_*.zip"){

  file <- fs::dir_ls(download_folder, glob =  search_pattern)
  fs::file_move(file, new_path = newpath)
  file <- fs::dir_ls(newpath, glob =  "**/sdr_*.zip") %>% as.character()

  message(file)
  shppath <- fs::path(newpath, survey) %>% as.character()
  fs::dir_create(shppath)


  if(!file_exists(file)){
    Sys.sleep(10)
  }

  if(!file_exists(file)){
    stop("the file does not exist")
  }else{
    unzip(zipfile = file, exdir = shppath)
    fs::file_delete(file)
  }



}



#' Title
#'
#' @param shp
#'
#' @return
#' @export
shp_to_tibble <- function(shp, DHS_CountryCode, surveyinfo, shpfilename, quiet = FALSE){

  n_original <- nrow(shp)
  shp <- filter(shp, DHSCC == DHS_CountryCode)

  if(!quiet){
    message(glue::glue("On {surveyinfo}: {n_original-nrow(shp)} rows removed due to inconsistent country code"))
  }

  names(shp)[names(shp) == "CNTRYNAMEE"] <- "country"
  #shp <- dplyr::rename(shp, country = CNTRYNAMEE)

  shp_as_row <- shp %>%
    st_set_geometry(., NULL) %>%
    distinct(DHSCC, SVYTYPE, SVYYEAR, country, CNTRYNAMEF, SVYNOTES) %>%
    as_tibble() %>%
    mutate(sf = NA)

  if(nrow(shp) == 0){
    if(!quiet){
      message(glue::glue("On {surveyinfo}: After removing inconsistent country codes there are no records"))
    }

    return(NULL)
  }

  shp <- shp[, c("REGCODE", "REGNAME", "DHSREGEN", "DHSREGFR", "DHSREGSP", "geometry")]
  
  # For mozambique 2015 AIS
  shp$REGNAME <- shp$REGNAME %>% stringr::str_replace("\\n", "") %>% stringr::str_replace("\\r", "")
  shp$DHSREGEN <- shp$DHSREGEN %>% stringr::str_replace("\\n", "") %>% stringr::str_replace("\\r", "")
  #shp <- select(shp, REGCODE, REGNAME, DHSREGEN, DHSREGFR, DHSREGSP, geometry)
  shp_as_row$sf <- list(shp)

  shp_as_row$web_survey_dtl <- surveyinfo
  shp_as_row$shapefile_name <- shpfilename
  #shp_as_row$country <- stringr::str_to_lower(shp_as_row$country)
  names(shp_as_row)[names(shp_as_row) == "SVYYEAR"] <- "year"
  #shp_as_row <- dplyr::rename(shp_as_row, year = SVYYEAR)
  shp_as_row$n_geometries <- nrow(shp)
  shp_as_row$n_geometries_removed <- n_original-nrow(shp)

  shp_as_row
}


#' Title
#'
#' @param .data
#'
#' @return
#' @export
spatial_fix_country_names <- function(.data){


  orig_country <- .data$country

  .data <- .data %>%
    mutate(
      country = case_when(
        country == "cote d'ivoire" ~ "côte d'ivoire",
        country == "federated states of micronesia" ~ "micronesia (federated states of)",
        country == "korea, republic of" ~ "republic of korea",
        country == "moldova" ~ "republic of moldova",
        country == "north korea" ~ "democratic people's republic of korea",
        country == "russia" ~ "russian federation",
        country == "syria" ~ "syrian arab republic",
        country == "tanzania" ~ "united republic of tanzania",
        country == "the bahamas" ~ "bahamas",
        country == "the gambia" ~ "gambia",
        country == "united states" ~ "united states of america",
        country == "venezuela, bolivarian rep. of" ~ "venezuela (bolivarian republic of)",
        country == "vietnam" ~ "viet nam",
        country == "iran" ~ "iran (islamic republic of)",
        country == "bolivia" ~ "bolivia (plurinational state of)",
        country == "brunei" ~ "brunei darussalam",
        country == "south korea" ~ "republic of korea",
        country == "venezuela" ~ "venezuela (bolivarian republic of)",
        country == "car" ~ "central african republic",
        country == "congo democratic republic" ~ "democratic republic of the congo",
        country == "kyrgyz republic" ~ "kyrgyzstan",
        country == "swaziland" ~ "eswatini",
        TRUE ~ country
      )
    )

  .data <- mutate(.data, original_country_name = orig_country)

  .data

}

#' Change the DHS shapefile year so that the year matches the 
#' WHO HEAT year -- for DHS surveys that spanned two years, DHS
#' may have assigned one year while WHO HEAT assigned the other year
#'
#' @param .data
#'
#' @return
#' @export
spatial_fix_survey_years <- function(.data){

  original_survey_year <- .data$year

  # Second number is the one you're changing TO so second should
  # be the year that is in the WHO data so from DHS year to WHO year
  .data <- mutate(.data,
                  year = case_when(
                    iso3 == "ARM" & year == 2016 ~ 2015,
                    iso3 == "BGD" & year == 1994 ~ 1993,#bangladesh
                    iso3 == "BGD" & year == 1997 ~ 1996,
                    iso3 == "BGD" & year == 2000 ~ 1999,
                    iso3 == "BEN" & year == 2012 ~ 2011,#benin
                    iso3 == "BFA" & year == 1993 ~ 1992,#burkina faso
                    iso3 == "BFA" & year == 1999 ~ 1998,#burkina faso
                    iso3 == "TCD" & year == 1997 ~ 1996, #chad
                    iso3 == "CIV" & year == 2012 ~ 2011,# cote d'ivoire
                    iso3 == "GTM" & year == 1999 ~ 1998, #
                    iso3 == "GTM" & year == 2015 ~ 2014,
                    iso3 == "HTI" & year == 2006 ~ 2005,
                    iso3 == "IND" & year == 1999 ~ 1998,
                    iso3 == "IND" & year == 2006 ~ 2005,
                    iso3 == "IDN" & year == 2003 ~ 2002, #
                    iso3 == "MDG" & year == 2004 ~ 2003, # madagascar
                    iso3 == "MLI" & year == 1996 ~ 1995, # mali
                    iso3 == "MMR" & year == 2016 ~ 2015,# myanmar
                    iso3 == "NIC" & year == 1998 ~ 1997,
                    #iso3 == "PER" & year == 2007 ~ 2008,
                    iso3 == "RWA" & year == 2015 ~ 2014, #rwanda
                    iso3 == "ZMB" & year == 2002 ~ 2001, # zambia
                    iso3 == "SEN" & year == 1993 ~ 1992,
                    iso3 == "PER" & year == 1992 ~ 1991,
                    iso3 == "TZA" & year == 1992 ~ 1991,
                    
                    TRUE ~ as.double(year)
                  )

  )

  .data <- mutate(.data, original_survey_year = original_survey_year)

  message(glue::glue("{sum(.data$year != .data$original_survey_year)} country-years changed"))

  .data

}




#' Pre-built country/years where the geography for a survey should get duplicated
#' so that it can be used for more than one year
#'
#' @param .data
#'
#' @return
#' @export
duplicate_survey_geo <- function(.data){

  # In the WHO data for Peru 2005, 2006 and 2008 do NOT
  # have associated geographic data.
  # We're making it so that 2004 geographic is associated with 2004, 2005, 2006
  # And 2008 is associated with 2007 (which looks like a better match than 2009)
  n_original <- nrow(.data)

  .data <- multi_use_survey_geo(.data, "PER", 2004, c(2005, 2006))
  .data <- multi_use_survey_geo(.data, "PER", 2007, 2008)
  .data <- multi_use_survey_geo(.data, "PER", 2012, 2013:2018) # git418
  
  message(glue::glue("{nrow(.data) - n_original} new rows added"))

  .data

}



#' Add new record in DHS survey geo dataset that is a duplicate of an existing year except includes a new year.
#'
#'
#' @details as an example, the DHS survey might include Peru 2007 but the WHO HEAT data includes Peru 2008 and 2008
#' is not available in the DHS data. Use this function to add a new recode with year 2008 that is identical
#' to 2007 except the year is changed to 2008. The default is to keep the existing record so in this case you
#' end up with two records with the same geography but one has year = 2007 and one is year = 2008.
#'
#' @param .data
#' @param setting_iso3 ISO3 of focus country
#' @param existing_year the year with real data from the DHS survey
#' @param new_years the years you want to get the same data as the existing year
#' @param keep_existing should the record for the existing year stay in the dataset
#'
#' @return
#' @export
multi_use_survey_geo <- function(.data, setting_iso3, existing_year, new_years, keep_existing = TRUE){


  n_start <- nrow(.data)

  existing <- filter(.data, iso3 == setting_iso3, year == existing_year)


  if(nrow(existing) == 0){
    return(.data)
  }

  if(nrow(existing)!=1){
    stop("The ISO3 and year combination should yield one row")
  }

  if(keep_existing){
    new_years <- c(existing_year, new_years)
  }


  .data <- filter(.data, !(iso3 == setting_iso3 & year == existing_year))
  new_records <- existing[rep(1, length(new_years)),]
  new_records$year <- new_years

  .data <- bind_rows(.data, new_records) %>%
    arrange(country, year)

  .data
}


#' Title
#'
#' @param .data
#' @param simplification_level
#' @param just_testing
#'
#' @return
#' @export
spatial_simplify_geo <- function(.data,  just_testing = FALSE){

  # file_size_cutoff <- shp_files %>%
  #   map(st_read, quiet = TRUE) %>%
  #   map_dbl(object.size)
  #
  # summary(file_size_cutoff)



  if(just_testing){
    .data$sf_simplified <- .data$sf
  }else{

    .data$sf_simplified <- furrr::future_map(.data$sf, function(x){

      file_size <- object.size(x)
      ms_simplify(
        x,
        keep = if (file_size > 812872) 0.01 else 0.75,
        method = "vis",
        keep_shapes = TRUE
      )
    }, .progress = TRUE)
  }


  .data
  #shp_as_row$sf_simplified <- list(shpsm)

  #.data$sf_size <-  suppressMessages(round(as.numeric(object.size(tmpshp))*1e-6, 3))
  #.data$sf_simplified_size <- suppressMessages(round(as.numeric(object.size(shpsm))*1e-6, 3))
}




#' Title
#'
#' @param .data
#' @param just_testing
#'
#' @return
#' @export
union_subnational_boundaries <- function(.data, just_testing = FALSE){


  if(just_testing){
    .data$sf_simplified_country <- .data$sf
  }else{
    .data$sf_simplified_country <- furrr::future_map(.data$sf_simplified, function(x){
      res <- try(st_union(x), silent = TRUE)
      if(any(class(res) == "try-error")) return(NA)
      res
    })
  }

  .data

}

#' Title
#'
#' @param .data
#'
#' @return
#' @export
drop_problematic_country_yr <- function(.data){

  # looking at who vs dhs and there are too many oddities
  .data <- filter(.data, !(iso3 == "EGY" & year == 2014))

  # only a single geography for chad this year (note the orig file was 1997, but changed to 1996)
  .data <- filter(.data, !(iso3 == "TCD" & year == 1996))
}



#' Title
#'
#' @param .data
#'
#' @return
#' @export
choose_one_shp <- function(.data){

  #!!! careful, this is the year BEFORE "fixing". For example, Guatemala (GTM, GU) the shapefile is 2015
  # and then we change to 2014 but here should be 2015
  
  #!! also careful these are the shapefiles we are REMOVING
  .data <- filter(.data, !(iso3 == "BOL" & year == 1994 & shapefile_name == "sdr_subnational_boundaries.shp"))
  .data <- filter(.data, !(iso3 == "COL" & year == 2005 & shapefile_name == "sdr_subnational_boundaries.shp"))
  .data <- filter(.data, !(iso3 == "COL" & year == 2010 & shapefile_name == "sdr_subnational_boundaries.shp"))
  .data <- filter(.data, !(iso3 == "COL" & year == 2015 & shapefile_name == "sdr_subnational_boundaries.shp"))
  .data <- filter(.data, !(iso3 == "COD" & year == 2013 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "EGY" & year == 1992 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "EGY" & year == 1995 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "EGY" & year == 2000 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "GTM" & year == 2015 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "IND" & year == 2015 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "JOR" & year == 2007 & shapefile_name == "sdr_subnational_boundaries.shp"))
  .data <- filter(.data, !(iso3 == "JOR" & year == 2012 & shapefile_name == "sdr_subnational_boundaries.shp"))
  .data <- filter(.data, !(iso3 == "KEN" & year == 2014 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "LBR" & year == 2013 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "MWI" & year == 2010 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "MWI" & year == 2015 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "NGA" & year == 2008 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "NGA" & year == 2013 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "NGA" & year == 2018 & shapefile_name == "sdr_subnational_boundaries.shp"))
  .data <- filter(.data, !(iso3 == "NPL" & year == 1996 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "NPL" & year == 1996 & shapefile_name == "sdr_subnational_boundaries3.shp"))
  .data <- filter(.data, !(iso3 == "NPL" & year == 2001 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "NPL" & year == 2001 & shapefile_name == "sdr_subnational_boundaries3.shp"))
  .data <- filter(.data, !(iso3 == "NPL" & year == 2006 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "NPL" & year == 2006 & shapefile_name == "sdr_subnational_boundaries3.shp"))
  .data <- filter(.data, !(iso3 == "NPL" & year == 2011 & shapefile_name == "sdr_subnational_boundaries.shp"))
  .data <- filter(.data, !(iso3 == "NPL" & year == 2011 & shapefile_name == "sdr_subnational_boundaries3.shp"))
  .data <- filter(.data, !(iso3 == "RWA" & year == 2015 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "SLE" & year == 2013 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "TUR" & year == 2003 & shapefile_name == "sdr_subnational_boundaries2.shp"))
  .data <- filter(.data, !(iso3 == "TZA" & year == 1992 & shapefile_name == "sdr_subnational_boundaries.shp"))
  .data <- filter(.data, !(iso3 == "TZA" & year == 2003 & shapefile_name == "sdr_subnational_boundaries.shp"))
  
  # per git 418
  # Mozambique 2015 only has one shapefile AIS, no filtering required
  .data <- filter(.data, !(iso3 == "SEN" & SVYTYPE == "DHS" & year == 2012 & shapefile_name == "sdr_subnational_boundaries.shp"))
  .data <- filter(.data, !(iso3 == "SEN" & SVYTYPE == "DHS" & year == 2014 & shapefile_name == "sdr_subnational_boundaries.shp"))

  # Now if we have multiple shapefiles remaining chose the one with more geometries
  .data <- group_by(.data, DHSCC, iso3, country, SVYTYPE, year) %>%
    mutate(shapefile_count = n()) %>%
    group_by(DHSCC, iso3, country, SVYTYPE, year) %>%
    mutate(max_geometries = max(n_geometries)) %>%
    filter(n_geometries == max_geometries) %>%
    select(-max_geometries) %>%
    group_by(DHSCC, iso3, country, SVYTYPE, year) %>%  # one has two shapefiles with the same number of geos
    mutate(row1 = row_number()) %>%
    filter(row1 == 1) %>%
    select(-row1) %>%
    ungroup()

  .data

}
