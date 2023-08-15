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
#' - **widths**: Consecutive interval widths for forecasted data
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

#' Repeat component
#'
#' @description
#'
#' This function evaluates whether consecutive values in observations or forecasts are repeated a k number of times. This function takes in a [forecast][to_signal()] object that is either from an observed dataset or forecast dataset.
#'
#' @param location Character vector with location code; the location must appear in input and seed
#' @param input Input signal data to be scored; object must be one of [forecast][to_signal()]
#' @param seed Prepared [seed][plane_seed()]
#' @param tolerance Integer value for the number of allowed repeats before flag is raised. Default is `NULL` and allowed repeats will be determined from seed.
#' @param prepend Integer value for the number of values from seed to add before the evaluated signal. Default is `NULL` and the number of values will be determined from seed.
#'
#' @return
#'
#' A `list` with the following values:
#'
#' - **indicator**: Logical as to whether or not the value is repeated sequentially k number of times.
#' - **repeats**: A `tibble` with repeating values found. If there are no repeats (i.e., indicator is `FALSE`) then the `tibble` will have 0 rows.
#'
#' @export
#'
plane_repeat <- function(location, input, seed, tolerance = NULL, prepend = NULL){

  ## double check that location is in seed before proceeding
  if(!location %in% names(seed)) {
    stop(sprintf("%s does not appear in the seed object. Check that the seed was prepared with the location specified.", location))
  }
  tmp_seed <- seed[[location]]

  ## by default tolerance is NULL
  ## if so ... use seeded "max repeats" (most repeated values observed at location)
  if(is.null(tolerance)) {
    tolerance <- tmp_seed$max_repeats
  }

  ## by default prepend is NULL
  ## if so ... use seeded "max repeats" (most repeated values observed at location)
  if(is.null(prepend)) {
    prepend <- tmp_seed$max_repeats
  }

  ## get all the values to prepend
  ## tail will get the last n values
  ## note that if prepend is 0 then nothing will be prepended
  prepend_vals <- utils::tail(tmp_seed$all_values,prepend)

  ## k is used below to raise flag for repeats
  ## define k as at least as many repeats for flag
  ## i.e., 1 more than what is tolerated
  k <- tolerance + 1

  # check the class of the input, must be forecast or observed as created by to_signal
  if(is_observed(input)) {
    tmp_dat <- input$data %>%
      dplyr::filter(.data$location == .env$location) %>%
      dplyr::filter(.data$date > as.Date(tmp_seed$meta$cut_date, format = "%Y-%m-%d")) %>%
      dplyr::arrange(date) %>%
      ## add a column that we can filter on later ...
      ## ... so we dont return data with prepend in list
      dplyr::mutate(prepend_type = "evaluated") %>%
      ## add prepend vals for repeat check
      ## NOTE: have to do some machinations to get input$outcome as column name
      dplyr::bind_rows(dplyr::tibble(x1 = prepend_vals, x2 = "prepend") %>% purrr::set_names(c(input$outcome, "prepend_type")), .) %>%
      ## add an identifier for each set of repeating values
      ## consecutive_id will start counting at first value ...
      ## then keep the same id until it sees a new value ...
      ## then will iterate on id ...
      ## and repeat this procedure through the last row of the tibble
      dplyr::mutate(repeat_id = dplyr::consecutive_id(.data[[input$outcome]])) %>%
      ## using the ids created above we can count how many times each value repeats
      dplyr::add_count(.data$repeat_id, name = "n_repeats")
  } else if(is_forecast(input)) {
    tmp_dat <- input$data %>%
      dplyr::filter(.data$location == .env$location) %>%
      dplyr::filter(.data$date > as.Date(tmp_seed$meta$cut_date, format = "%Y-%m-%d")) %>%
      dplyr::arrange(date) %>%
      ## add a column that we can filter on later ...
      ## ... so we dont return data with prepend in list
      dplyr::mutate(prepend_type = "evaluated") %>%
      ## add prepend vals for repeat check
      dplyr::bind_rows(dplyr::tibble(point = prepend_vals, prepend_type = "prepend"), .) %>%
      ## add an identifier for each set of repeating values
      ## consecutive_id will start counting at first value ...
      ## then keep the same id until it sees a new value ...
      ## then will iterate on id ...
      ## and repeat this procedure through the last row of the tibble
      dplyr::mutate(repeat_id = dplyr::consecutive_id(.data$point)) %>%
      ## using the ids created above we can count how many times each value repeats
      dplyr::add_count(.data$repeat_id, name = "n_repeats")
  }

  ## filter the data with repeat counts
  ## only include any data that has *more* than allowed repeats
  repeat_tbl <-
    tmp_dat %>%
    dplyr::filter(.data$prepend_type == "evaluated") %>%
    dplyr::filter(.data$n_repeats >= k) %>%
    dplyr::select(-"repeat_id", -"n_repeats", -"prepend_type")

  ## indicator for whether or not the number of rows is > 0
  ## this would indicate that there are repeats
  ind <- nrow(repeat_tbl) > 0

  ## return list with indicator and info
  return(list(indicator = ind, repeats = repeat_tbl))
}

#' Score PLANES components
#'
#' @description
#'
#' This function wraps PLANES scoring for specified components across all locations in single step.
#'
#'
#' @param input Input signal data to be scored; object must be one of [forecast][to_signal()] or [observed][to_signal()]
#' @param seed Prepared [seed][plane_seed()]
#' @param components Character vector specifying components; Any combination of "cover", "diff", "taper" and "repeats". Default is `'all'` and will use all available components for the given signal
#'
#'
#'
#' @return
#'
#' A `list` with scoring results for all locations.
#'
#' @export
#'
plane_score <- function(input, seed, components = "all") {

  ## TODO: create this list as a built-in object?
  ## NOTE: consider using getFromNamespace to simplify this step
  complist <-
    list(cover = list(.function = plane_cover),
         diff = list(.function = plane_diff),
         taper = list(.function = plane_taper),
         repeats = list(.function = plane_repeat)
    )

  ## TODO: verify components for signal type ... some won't apply to observed
  if(is_observed(input) & any(components %in% c("cover", "taper", "all"))) {
    stop("Input must be a forecast when component contains 'cover' or 'taper'.")
  }

  if(length(components) == 1 && components == "all") {
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

  ## pull out summary tibble components and locations from the returned list above
  loc_tbl <-
    dplyr::tibble(component_loc = names(retl), indicator = purrr::map_lgl(retl, "indicator")) %>%
    tidyr::separate(.data$component_loc, into = c("component", "location"), sep = "-")

  ## convert the tibble into a list
  loc_list <-
    loc_tbl %>%
    ## count number of flags (numerator for score)
    ## count number of components (denominator for score)
    ## convert to score
    ## hold onto the names of the components used
    dplyr::group_by(.data$location) %>%
    dplyr::summarise(n_flags = sum(.data$indicator),
                     n_components = dplyr::n(),
                     score = .data$n_flags / .data$n_components,
                     components = paste0(.data$component, collapse = ";")
    ) %>%
    ## split into a list by location
    dplyr::group_split(.data$location, .keep = TRUE) %>%
    ## use the location column from the tibble in each split list element as name
    purrr::set_names(purrr::map_chr(., "location")) %>%
    ## make sure it is a list of lists (not a list of tibbles)
    purrr::map(., as.list)

  return(list(scores_summary = loc_list, scores_raw = loc_tbl))

}
