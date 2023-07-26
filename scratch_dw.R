library(tidyverse)
library(rplanes)


# repeat value check for plausibility score #4 ####

#' Repeat component
#'
#' @description
#'
#' This function evaluates whether consecutive values in observations or forecasts are repeated a k number of times. This function takes in a [forecast][to_signal()] object that is either from an observed dataset or forecast dataset.
#'
#'
#' @param input Input signal data to be scored; object must be one of [forecast][to_signal()]
#' @param location Character vector with location code; the location must appear in input and seed
#' @param k Threshold for the number of repeats to be flagged. Default is 3.
#' @param seed Prepared [seed][plane_seed()]
#'
#' @return
#'
#' A `list` with the following values:
#'
#' - **indicator**: Logical as to whether or not the value is repeated sequentially k number of times.
#'
#' If the **indicator** returns TRUE the additional values returned:
#' - **dates**: The dates at which the consecutively repeated values occur.
#'
#'
#' @export
#'
#'
plane_repeat <- function(input, location, k = 3, seed){

  ## double check that location is in seed before proceeding
  if(!location %in% names(seed)) {
    stop(sprintf("%s does not appear in the seed object. Check that the seed was prepared with the location specified.", location))
  }
  tmp_seed <- seed[[location]]

  # check the class of the input, must be forecast or observed as created by to_signal
  if(is_observed(input)) {
    tmp_dat <- input$data %>%
      dplyr::filter(.data$location == .env$location) %>%
      dplyr::filter(.data$date > as.Date(tmp_seed$meta$cut_date, format = "%Y-%m-%d")) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(repeated = ifelse(
        slider::slide_vec(.data[[input$outcome]], var, .before = k) == 0, TRUE, FALSE) # if variation for the k consecutive points = 0 then TRUE else FALSE
      )
  } else if(is_forecast(input)) {
    tmp_dat <- input$data %>%
      dplyr::filter(.data$location == .env$location) %>%
      dplyr::filter(.data$date > as.Date(tmp_seed$meta$cut_date, format = "%Y-%m-%d")) %>%
      dplyr::arrange(date) %>%
      mutate(repeated = ifelse(
        slider::slide_vec(.data$point, var, .before = k) == 0, TRUE, FALSE) # if the variation = 0 for consecutive horizon's points return TRUE else FALSE
      )
  }
  if(any(tmp_dat$repeated %in% TRUE)){
    ind <- any(tmp_dat$repeated %in% TRUE, na.rm = T)
    loc <- tmp_dat$location[tmp_dat$repeated %in% TRUE]
    dates <- tmp_dat$date[tmp_dat$repeated %in% TRUE]

    return(list(indicator = ind, dates = dates))

  } else {
    ind <- any(tmp_dat$repeated %in% TRUE, na.rm = T)
    return(list(indicator = ind))
  }
}
# read_forecast <- function(file, pi_width=95) {
file = "inst/extdata/forecast/2022-10-31-SigSci-CREG.csv"
## use .pi_width argument to construct vector of quantiles. If quantiles not in quant_list, stop.
width <- q_boundary(95)
# list of quantiles used in forecasts
quant_list <- round(c(0.010, 0.025, 0.050, 0.100, 0.150, 0.200, 0.250, 0.300, 0.350, 0.400, 0.450, 0.500, 0.550, 0.600, 0.650, 0.700, 0.750, 0.800, 0.850, 0.900, 0.950, 0.975, 0.990), 3)
stopifnot("Quantiles unavailable for width specified." = width %in% quant_list)


# ~~~~~~~~~~~~~~~~~~~~~~~
df <- readr::read_csv(file)
tmp_data <- df %>%
  dplyr::mutate(epiweek = lubridate::epiweek(.data$target_end_date),
                epiyear = lubridate::epiyear(.data$target_end_date)) %>%
  dplyr::filter(.data$type == "point" | .data$quantile %in% width) %>%
  dplyr::mutate(quantile = ifelse(is.na(.data$quantile), 0.5, .data$quantile))  %>%
  ## str_extract between 1 to 3 digits, to get horizon from target value
  dplyr::mutate(horizon = stringr::str_extract(.data$target, pattern = "\\d{1,3}"))

if (sum(stringr::str_count(unique(df$type), "quantile|point")) == 2){
  point_test <- df %>%
    dplyr::mutate(quantile = ifelse(is.na(.data$quantile), 0.5, .data$quantile)) %>%
    dplyr::filter(quantile == 0.5) %>%
    dplyr::group_by(forecast_date, location, target) %>%
    dplyr::mutate(not_equal = ifelse(value[type == "point"] != value[type == "quantile"], TRUE, FALSE)) %>%
    dplyr::filter(type == "point" & not_equal == TRUE) %>%
    dplyr::ungroup()

  tmp_data2 <- tmp_data  %>%
    # remove rows with point types whose values don't equal the 0.5 quantile values
    anti_join(point_test, by = c("forecast_date", "target", "location", "type", "quantile")) %>%
    ## NOTE: as of tidyselect v1.2.0 the .data pronoun is deprecated for select-ing
    dplyr::select("location", date = "target_end_date", "horizon", "quantile", "value") %>%
    dplyr::arrange(.data$location,.data$date,.data$horizon,.data$quantile) %>%
    dplyr::distinct_all() %>%
    tidyr::spread(.data$quantile, .data$value) %>%
    purrr::set_names(c("location","date","horizon","lower","point","upper"))

} else {
  tmp_data2 <- tmp_data %>%
    dplyr::select("location", date = "target_end_date", "horizon", "quantile", "value") %>%
    dplyr::arrange(.data$location,.data$date,.data$horizon,.data$quantile) %>%
    dplyr::distinct_all() %>%
    tidyr::spread(.data$quantile, .data$value) %>%
    purrr::set_names(c("location","date","horizon","lower","point","upper"))


}



