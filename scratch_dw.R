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
