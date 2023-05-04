#' Difference component
#'
#' @description
#'
#' This function implements the point-to-point difference plausibility component. Differences in evaluated signals are calculated from input values iteratively subtracted from the previous values (i.e., for each x at time point i, the difference will be calculated as xi - xi-1). The plausibility analysis uses the evaluated differences to compare against the maximum difference observed and recorded in the seed.
#'
#' @param location Character vector with location code; the location must appear in input and seed
#' @param input Input signal data to be scored; object must be one of [forecast][to_signal()] or [observed][to_signal()]
#' @param seed Prepared [seed][plane_seed()]
#'
#' @return
#'
#' A `list` with the following values:
#'
#' - **indicator**: Logical as to whether or not the absolute value of any of the evaluated differences exceeds the maximum difference
#' - **values**: A vector with the values assessed including the last value in seed concatenated with the evaluated signal values
#' - **evaluated_differences**: A vector with the consecutive differences for the values
#' - **maximum_difference**: A vector with one value for the maximum difference observed in seed
#'
#' @export
#'
plane_diff <- function(location, input, seed) {

  ## double check that location is in seed before proceeding
  if(!location %in% names(seed)) {
    stop(sprintf("%s does not appear in the seed object. Check that the seed was prepared with the location specified.", location))
  }

  tmp_seed <- seed[[location]]

  ## check for class of input to see if it is observed
  ## if so ... filter on seed dates to so that we're comparing the observed of interest to seed vals
  if(is_observed(input)) {
    tmp_dat <-
      input$data %>%
      dplyr::filter(.data$location == .env$location) %>%
      dplyr::filter(.data$date > as.Date(tmp_seed$meta$cut_date, format = "%Y-%m-%d"))

    ## check that dates are valid (i.e., no observed data doesnt overlap with seed
    valid_dates(seed_date = tmp_seed$meta$date_range$max, signal_date = min(tmp_dat$date), resolution = tmp_seed$meta$resolution)

    ## pull the outcome values to be evaluated and concatenate with most recent value in seed
    tmp_vals <-
      tmp_dat %>%
      dplyr::pull(input$outcome) %>%
      c(tmp_seed$last_value, .)

  } else if(is_forecast(input)) {
    ## return the forecast data (with the filter on cut date)
    tmp_dat <-
      input$data %>%
      dplyr::filter(.data$location == .env$location) %>%
      dplyr::filter(.data$date > as.Date(tmp_seed$meta$cut_date, format = "%Y-%m-%d"))

    ## check that dates are valid (i.e., no observed data doesnt overlap with seed
    valid_dates(seed_date = tmp_seed$meta$date_range$max, signal_date = min(tmp_dat$date), resolution = tmp_seed$meta$resolution)

    ## after all the checks ...
    ## pull the point estimate and concatenate with most recent value in seed
    tmp_vals <-
      tmp_dat %>%
      dplyr::pull("point") %>%
      c(tmp_seed$last_value, .)

  }

  ## compute differences with previous values
  eval_diffs <- tmp_vals - dplyr::lag(tmp_vals,1)
  ## test for whether or not any of these
  ind <- any(abs(eval_diffs) > tmp_seed$diff$max, na.rm = TRUE)

  return(list(indicator = ind, values = tmp_vals, evaluated_differences = eval_diffs[!is.na(eval_diffs)], maximum_difference = tmp_seed$diff$max))


}


#' Title
#'
#' @param location fixme
#' @param input fixme
#' @param seed fixme
#'
#' @return
#' @export
#'
#' @examples
#'
plane_cover <- function(location, input, seed) {

  ## double check that location is in seed before proceeding
  if(!location %in% names(seed)) {
    stop(sprintf("%s does not appear in the seed object. Check that the seed was prepared with the location specified.", location))
  }

  tmp_seed <- seed[[location]]

  ## check for class of input to see if it is observed
  ## if so ... stop for now because pi width doesnt apply?
  ## TODO: add backcasting approach to allow us to use this for observed data
  if(is_observed(input)) {

    stop("Must be forecast ...")

  } else if(is_forecast(input)) {

    ## return the forecast data (with the filter on cut date)
    tmp_dat <-
      input$data %>%
      dplyr::filter(.data$location == .env$location) %>%
      dplyr::filter(.data$date > as.Date(tmp_seed$meta$cut_date, format = "%Y-%m-%d"))

    ## check that dates are valid (i.e., no observed data doesnt overlap with seed
    valid_dates(seed_date = tmp_seed$meta$date_range$max, signal_date = min(tmp_dat$date), resolution = tmp_seed$meta$resolution)

    bounds <-
      tmp_dat %>%
      dplyr::filter(.data$horizon == 1) %>%
      dplyr::select(.data$lower, .data$upper)

    ind <- dplyr::between(tmp_seed$last_value, bounds$lower, bounds$upper)

  }

  ## test whether the bounds cover the last value
  ind <- dplyr::between(tmp_seed$last_value, bounds$lower, bounds$upper)

  return(list(indicator = ind, last_value = tmp_seed$last_value, bounds = list(lower = bounds$lower, upper = bounds$upper)))

}
