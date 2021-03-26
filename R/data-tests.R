stop_invalid_data <- function(key) {
  stop(
    structure(
      class = c("invalid_data", "error", "condition"),
      list(key = key, message = "uploaded data is invalid", call = NULL)
    )
  )
}



#' Test to confirm that all required variables are present in the dataset
#'
#' @description required variables are listed originally in an Excel file in data-raw
#' but were processed into a variable exposed by this package (`HEAT_variable_descriptions`)
#'
#' @param .data
#'
#' @return
#' @export
test_has_required_variables <- function(.data) {

  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]
  reqname <- heatdata::HEAT_variable_descriptions$VARIABLE

  X <- length(reqname[!(reqname %in% names(.data))]) == 0

  pass <- ifelse(all(X), TRUE, FALSE)

  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="variables", warning_msg = "The file does not have all the required variables"))
}


# 2 Missing values for mandatory variables (except "estimate") ----
test_key_vars_complete <- function(.data){

  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]
  key_vars <- filter(heatdata::HEAT_variable_descriptions,
                     !!sym(fname) ==  TRUE)

  X <- select(.data, !!!syms(key_vars$VARIABLE)) %>%
    purrr::map_lgl(~!any(is.na(.)))



  pass <- ifelse(all(X), TRUE, FALSE)

  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="mandatory", warning_msg = "One of the mandatory variables has missing values"))


  #   stop_invalid_data("warnmissingvals")
}




# 3 Missing values for mandatory variable "estimate" ----
test_key_vars_not_empty <- function(.data) {


  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]
  key_vars <- filter(heatdata::HEAT_variable_descriptions,
                     !!sym(fname) ==  TRUE)

  X <- select(.data, !!!syms(key_vars$VARIABLE)) %>%
    purrr::map_lgl(~!all(is.na(.)))

  pass <- ifelse(all(X), TRUE, FALSE)

  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="notmandatory", warning_msg = "There are missing values in the estimate variable. While not mandatory, if data for this variable are missing some calculations will not function in HEAT Plus."))



  #   stop_invalid_data("warnmissingestimates")

}

# 4 String variables are numeric ----
test_chars_are_chars <- function(.data) {

  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]
  key_vars <- filter(heatdata::HEAT_variable_descriptions,
                     !!sym(fname) ==  TRUE)



  # Using microbenchmark this method is a slight bit faster
  X <- select(.data, !!!syms(key_vars$VARIABLE)) %>%
    purrr::map_lgl(~all(is.na(suppressWarnings(as.numeric(.)))))

  # here we test if all characters are digits or if we have digit period digit
  # and then use !
  # X <- select(.data, !!!syms(key_vars$VARIABLE)) %>%
  #   purrr::map_lgl(~any(grepl("^\\d+(\\.\\d+)?$", .)))

  pass <- ifelse(all(X), TRUE, FALSE)

  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="characters", warning_msg = "Not all character variables contain characters (text/string)"))

  #stop_invalid_data("warncharsvars")

}

# 5 iso3 not 3 characters long ----
test_vars_3chars <- function(.data) {

  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]
  key_vars <- filter(heatdata::HEAT_variable_descriptions,
                     !!sym(fname) ==  TRUE)

  # All the non-NA values are 3 digits
  X <- select(.data, !!!syms(key_vars$VARIABLE)) %>%
    purrr::map_lgl(function(x){
      vals <- x[!is.na(x)]
      all(nchar(vals) == 3)
    })

  pass <- ifelse(all(X), TRUE, FALSE)


  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="iso3", warning_msg = "Not all ISO3 values are three characters"))

  # stop_invalid_data("warnisovars")

}

# 6 Numeric variables are text  ----
test_nums_not_char <- function(.data) {

  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]
  key_vars <- filter(heatdata::HEAT_variable_descriptions,
                     !!sym(fname) ==  TRUE)

  # Careful with NA. This regex will return FALSE for NA as we desire
  # but true with any characters
  X <- select(.data, !!!syms(key_vars$VARIABLE)) %>%
    purrr::map_lgl(~all(!grepl("[:alpha:]", .)))

  pass <- ifelse(all(X), TRUE, FALSE)
  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="numbers", warning_msg = "Variables that should be only numbers contain characters (text/string)"))
  #stop_invalid_data("warnnumsvars")

}

# 7 Year is not 4 digits long ----
test_vars_4chars <- function(.data) {

  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]
  key_vars <- filter(heatdata::HEAT_variable_descriptions,
                     !!sym(fname) ==  TRUE)


  X <- select(.data, !!!syms(key_vars$VARIABLE)) %>%
    purrr::map_lgl(~all(nchar(.) == 4))

  pass <- ifelse(all(X), TRUE, FALSE)
  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="years", warning_msg = "Not all years are four-digit numbers"))


  #    stop_invalid_data("warnyearvars")

}

# 8 Numeric variables are not integers ----
test_no_decimals <- function(.data) {

  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]
  key_vars <- filter(heatdata::HEAT_variable_descriptions,
                     !!sym(fname) ==  TRUE)


  X <- select(.data, !!!syms(key_vars$VARIABLE)) %>%
    purrr::map_lgl(~all(as.numeric(.) %% 1 == 0))

  pass <- ifelse(all(X), TRUE, FALSE)

  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="integers", warning_msg = "There are decimal numbers where integers are required"))



  #stop_invalid_data("warnintegervars")

}

# 9 Numeric variables are negative ----
test_all_nonnegative_or_NA <- function(.data) {

  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]
  key_vars <- filter(heatdata::HEAT_variable_descriptions,
                     !!sym(fname) ==  TRUE)



  # Using microbenchmark this method is a slight bit faster
  X <- select(.data, !!!syms(key_vars$VARIABLE)) %>%
    purrr::map_lgl(function(x){
      vals <- suppressWarnings(as.numeric(x))
      all(is.na(vals) | (vals >= 0))
    }
    )


  pass <- ifelse(all(X), TRUE, FALSE)

  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="negatives", warning_msg = "There are negative numbers where negative numbers are not allowed"))


  #  stop_invalid_data("warnnegativevars")

}

# 10 Population is 0 ----
test_all_positive_or_NA <- function(.data) {

  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]
  key_vars <- filter(heatdata::HEAT_variable_descriptions,
                     !!sym(fname) ==  TRUE)


  X <- select(.data, !!!syms(key_vars$VARIABLE)) %>%
    purrr::map_lgl(function(x){
      vals <- suppressWarnings(as.numeric(x))
      all(is.na(vals) | (vals > 0))
    }
    )


  pass <- ifelse(all(X), TRUE, FALSE)

  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="positives", warning_msg = "Variables that must be positive or missing are negative"))


  #  stop_invalid_data("warnpopulationvars")

}

# 11, 15
test_is_binary_or_NA <- function(.data) {


  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]
  key_vars <- filter(heatdata::HEAT_variable_descriptions,
                     !!sym(fname) ==  TRUE)


  X <- select(.data, !!!syms(key_vars$VARIABLE)) %>%
    purrr::map_lgl(function(x) all(is.na(x) | grepl("^[01]$", x)))


  pass <- ifelse(all(X), TRUE, FALSE)

  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="binary", warning_msg = "Variables that must be binary have non 1/0 values"))


  # stop_invalid_data("warnfavbinary")
  # stop_invalid_data("warnorddimbinary")
  # stop_invalid_data("warnrefbinary")
}



# 13, 16, 17
test_strata_ordered_andref <- function(.data) {
  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]

  # Pull the variable name for the variable that tells
  # you IF the variable is ordered
  is_order_var <- filter(heatdata::HEAT_variable_descriptions,
                         !!sym(fname) == "is_ordered") %>%
    pull(VARIABLE)

  # Pull the variable name for the variable that has the order
  order_var <- filter(heatdata::HEAT_variable_descriptions,
                      !!sym(fname) == "order") %>%
    pull(VARIABLE)

  # Pull the variable name for the variable that tells you which is reference
  reference_var <- filter(heatdata::HEAT_variable_descriptions,
                          !!sym(fname) == "reference") %>%
    pull(VARIABLE)

  # TODO: this is an expensive operation
  order_info <- group_by(.data, strata_id) %>%
    summarise(all_isorder_zero = all(!!sym(is_order_var) == 0),
              all_isorder_one = all(!!sym(is_order_var) == 1),
              all_order_0 = all(!!sym(order_var) == 0),
              all_order_not0 = max(!!sym(order_var)) > 0,
              reference_count = sum(!!sym(reference_var) == 1),
              no_reference = reference_count == 0,
              one_or_zero_reference = reference_count <= 1)


  # EITHER
  # All of the is_order field is 0 and all order field is 0 OR
  # All of the is_order field is 1 and all order field is not 0

  # AND

  # EITHER
  # All the is_ordered field is zero
  # OR
  # All the is_ordered field is 1 and there is no reference


  X <- (
    (order_info$all_isorder_zero & order_info$all_order_0) |
      (order_info$all_isorder_one & order_info$all_order_not0)
  ) &

    (
      order_info$all_isorder_zero |
      (order_info$all_isorder_one & order_info$no_reference)
    ) &
    order_info$one_or_zero_reference

  pass <- ifelse(all(X), TRUE, FALSE)

  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="orderedvars", warning_msg = "There is a problem with the ordered variables"))

  #stop_invalid_data("warnsubgrpvars")
  #stop_invalid_data("warnrefgrpvars")
  #stop_invalid_data("warnrefmultones")

}


# 14 Subgroup order is an increasing sequence when ordered_dimension = 1 ----
test_strata_subgroup_sequence <- function(.data) {
  # if an ordered dimension then the sequence must be 1:nrow(strata)


  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]

  is_order_var <- filter(heatdata::HEAT_variable_descriptions,
                         !!sym(fname) == "is_ordered") %>%
    pull(VARIABLE)

  order_var <- filter(heatdata::HEAT_variable_descriptions,
                      !!sym(fname) == "order") %>%
    pull(VARIABLE)

  X <- .data %>%
    filter(!!sym(is_order_var) == 1) %>%
    arrange(!!sym(order_var)) %>%
    group_by(strata_id) %>%
    mutate(tmp_seq = row_number()) %>%
    summarise(the_test = all(!!sym(order_var) == tmp_seq)) %>%
    pull(the_test)


  pass <- ifelse(all(X), TRUE, FALSE)

  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="sequence", warning_msg = "Subgroup order must be an increasing sequence of integers starting with 1 when ordered dimension = 1"))

  #  stop_invalid_data("warngrpsequence")

}




test_setting_iso3_match <- function(.data) {


  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]

  setting_var <- filter(heatdata::HEAT_variable_descriptions,
                        !!sym(fname) %in% c("setting")) %>%
    pull(VARIABLE)

  iso3_var <- filter(heatdata::HEAT_variable_descriptions,
                     !!sym(fname) %in% c("iso3")) %>%
    pull(VARIABLE)

  key_vars <- c(setting_var, iso3_var)
  tmp <- select(.data, !!!syms(key_vars)) %>%
    distinct()

  setting_once <- all(table(tmp[[setting_var]]) == 1)
  iso3_once <- all(table(tmp[[iso3_var]]) == 1)

  X <- c(setting_once, iso3_once)

  pass <- ifelse(all(X), TRUE, FALSE)
  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="settings", warning_msg = "Setting and ISO3 are not consistent"))



  # stop_invalid_data("warniso3dups")

}


test_indicator_unique_names <- function(.data) {


  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]

  indabbr_var <- filter(heatdata::HEAT_variable_descriptions,
                        !!sym(fname) %in% c("indicabbr")) %>%
    pull(VARIABLE)

  indname_var <- filter(heatdata::HEAT_variable_descriptions,
                        !!sym(fname) %in% c("indicname")) %>%
    pull(VARIABLE)

  key_vars <- c(indabbr_var, indname_var)
  tmp <- select(.data, !!!syms(key_vars)) %>%
    distinct()

  indabbr_once <- all(table(tmp[[indabbr_var]]) == 1)
  indname_once <- all(table(tmp[[indname_var]]) == 1)

  X <- c(indabbr_once, indname_once)

  pass <- ifelse(all(X), TRUE, FALSE)
  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="abbreviations", warning_msg = "Indicator abbreviation and name are not uniquely matched"))


  #stop_invalid_data("warnabbrnamemismatch")

}

# 19 Observations have the same value when grouped by setting, year, source, indicator ----
test_consistent_indicinfo <- function(.data) {


  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]

  grouping_vars <- filter(heatdata::HEAT_variable_descriptions,
                          !!sym(fname) %in% c("grouping")) %>%
    pull(VARIABLE)

  check_vars <- filter(heatdata::HEAT_variable_descriptions,
                       !!sym(fname) %in% c("check_consistency")) %>%
    pull(VARIABLE)

  key_vars <- c(grouping_vars, check_vars)
  strata_count <- .data %>%
    select(!!key_vars) %>%
    distinct() %>%
    ungroup() %>%
    group_by(!!!syms(grouping_vars)) %>%
    summarise(test = n()) %>%
    pull(test)


  X <- strata_count == 1

  pass <- ifelse(all(X), TRUE, FALSE)
  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="indicators", warning_msg = "Indicator information is not consistent"))

  # stop_invalid_data("warndupobscombo")

}

# 20 Ordered dimension must be the same for combinations of setting, year, source, indicator, dimension ----
test_consistent_ordered_dimension <- function(.data) {


  .data <- .data %>% ungroup()
  thecall <- as.character(match.call()[[1]])
  fname <- thecall[length(thecall)]

  grouping_vars <- filter(heatdata::HEAT_variable_descriptions,
                          !!sym(fname) %in% c("grouping")) %>%
    pull(VARIABLE)

  check_vars <- filter(heatdata::HEAT_variable_descriptions,
                       !!sym(fname) %in% c("check_consistency")) %>%
    pull(VARIABLE)

  key_vars <- c(grouping_vars, check_vars)
  strata_count <- .data %>%
    select(!!key_vars) %>%
    distinct() %>%
    ungroup() %>%
    group_by(!!!syms(grouping_vars)) %>%
    summarise(test = n()) %>%
    pull(test)


  X <- strata_count == 1

  pass <- ifelse(all(X), TRUE, FALSE)
  return(list(func = fname, pass = pass, namespace="manager", subject="warnings", key="ordereddims", warning_msg = "There is inconsistency in ordered dimensions"))


  #stop_invalid_data("warndupordcombo")

}

