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


#' Coverage component
#'
#' @description
#'
#' This function evaluates whether or not the evaluated signal interval covers the last observed value. The interval used in this plausibility component is drawn from the upper and lower bounds of the forecasted prediction interval. As such, the only accepted signal format is [forecast][to_signal()], which will include upper and lower bounds.
#'
#' @param location Character vector with location code; the location must appear in input and seed
#' @param input Input signal data to be scored; object must be one of [forecast][to_signal()]
#' @param seed Prepared [seed][plane_seed()]
#'
#' @return
#'
#' A `list` with the following values:
#'
#' - **indicator**: Logical as to whether or not the last value falls outside of the interval (e.g., not in between lower and upper bounds of prediction interval) of the evaluated signal
#' - **last_value**: A vector with the last value recorded in the seed
#' - **bounds**: A list with a two elements corresponding to the upper and lower bounds of the evaluated signal interval
#'
#' @export
#'
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

    ## pull lower and upper bounds from data
    ## NOTE: for now this only looks at horizon 1
    bounds <-
      tmp_dat %>%
      dplyr::filter(.data$horizon == 1) %>%
      ## NOTE: as of tidyselect v1.2.0 the .data pronoun is deprecated for select-ing
      dplyr::select("lower", "upper")

    ind <- dplyr::between(tmp_seed$last_value, bounds$lower, bounds$upper)

  }

  ## test whether the bounds cover the last value
  ## NOTE: the logic is flipped here with ! operator
  ## this helps standardize interpretation across components
  ## effectively asking ... is the most recent value *outside* of the prediction interval
  ind <- !dplyr::between(tmp_seed$last_value, bounds$lower, bounds$upper)

  return(list(indicator = ind, last_value = tmp_seed$last_value, bounds = list(lower = bounds$lower, upper = bounds$upper)))

}


#' Taper component
#'
#' @description
#'
#' This function evaluates whether or not the evaluated signal interval tapers (i.e., decreases in width) as horizons progress. The interval used in this plausbility component is drawn from the upper and lower bounds of the forecasted prediction interval. As such, the only accepted signal format is [forecast][to_signal()], which will include upper and lower bounds.
#'
#'
#' @param location Character vector with location code; the location must appear in input and seed
#' @param input Input signal data to be scored; object must be one of [forecast][to_signal()]
#' @param seed Prepared [seed][plane_seed()]
#'
#' @return
#'
#' A `list` with the following values:
#'
#' - **indicator**: Logical as to whether or not the prediction interval width tapers with advancing horizons
#' - **widths**: Consecutive interval widths for
#'
#' @export
#'
#'
plane_taper <- function(location, input, seed) {

  ## NOTE: do we need seed here? maybe not?

  if(is_observed(input)) {

    stop("Must be forecast ...")

  } else if(is_forecast(input)) {

    ## get the consecutive widths for prediction interval
    interval_widths <-
      input$data %>%
      dplyr::filter(.data$location == .env$location) %>%
      dplyr::mutate(width = .data$upper - .data$lower) %>%
      dplyr::arrange(date) %>%
      dplyr::pull(.data$width)

    ind <- any(interval_widths - dplyr::lag(interval_widths) < 0, na.rm = TRUE)

    return(list(indicator = ind, widths = interval_widths))

  }

}


#' Score PLANES components
#'
#' @description
#' FIXME
#'
#'
#' @param input Input signal data to be scored; object must be one of [forecast][to_signal()] or [observed][to_signal()]
#' @param seed Prepared [seed][plane_seed()]
#' @param components Character vector specifying components; default is `'all'` and will use all available components for the given signal
#'
#' @details
#'
#' FIXME
#'
#'
#' @return
#' FIXME
#'
#' @export
#'
plane_score <- function(input, seed, components = "all") {

  ## TODO: create this list as a built-in object?
  complist <-
    list(cover = list(.function = plane_cover),
         diff = list(.function = plane_diff),
         taper = list(.function = plane_taper)
    )

  ## TODO: verify components for signal type ... some won't apply to observed
  if(components == "all") {
    components <- names(complist)
  }

  ## get all possible locations
  ## TODO: make this easier to access in prepped signal by adding locations element
  locs <- unique(input$data$location)

  ## create combinations of locations and components for mapping below
  to_map <- tidyr::crossing(locs = locs, comps = components)

  ## TODO: better way do this mapping and tracking of location / components by name
  retl <-
    purrr::map2(to_map$comps, to_map$locs, ~ purrr::exec(complist[[.x]]$.function, location = .y, input = input, seed = seed)) %>%
    purrr::set_names(paste0(to_map$comps, "-", to_map$locs))

  ## pull out summary from the returned list above
  dplyr::tibble(component_loc = names(retl), indicator = purrr::map_lgl(retl, "indicator")) %>%
    tidyr::separate(.data$component_loc, into = c("component", "location"), sep = "-")
}
