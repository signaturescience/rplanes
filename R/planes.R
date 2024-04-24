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
#' @examples
#' ## read in example observed data and prep observed signal
#' hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))
#' hosp$date <- as.Date(hosp$date, format = "%Y-%m-%d")
#' prepped_observed <- to_signal(hosp, outcome = "flu.admits", type = "observed", resolution = "weeks")
#'
#' ## read in example forecast and prep forecast signal
#' fp <- system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv", package = "rplanes")
#' prepped_forecast <- read_forecast(fp) %>%
#'   to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)
#'
#' ## prepare seed with cut date
#' prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-10-29")
#'
#' ## run plane component
#' plane_diff(location = "10", input = prepped_forecast, seed = prepped_seed)
#' plane_diff(location = "51", input = prepped_forecast, seed = prepped_seed)
#'
plane_diff <- function(location, input, seed) {

  ## double check that location is in seed and input before proceeding
  valid_location(location, input, seed)

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
#' @examples
#'
#' ## read in example observed data and prep observed signal
#' hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))
#' hosp$date <- as.Date(hosp$date, format = "%Y-%m-%d")
#' prepped_observed <- to_signal(hosp, outcome = "flu.admits", type = "observed", resolution = "weeks")
#'
#' ## read in example forecast and prep forecast signal
#' fp <- system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv", package = "rplanes")
#' prepped_forecast <- read_forecast(fp) %>%
#'   to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)
#'
#' ## prepare seed with cut date
#' prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-10-29")
#'
#' ## run plane component
#' plane_cover(location = "08", input = prepped_forecast, seed = prepped_seed)
#' plane_cover(location = "47", input = prepped_forecast, seed = prepped_seed)
plane_cover <- function(location, input, seed) {

  ## double check that location is in seed and input before proceeding
  valid_location(location, input, seed)

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
#' This function evaluates whether or not the evaluated signal interval tapers (i.e., decreases in width) as horizons progress. The interval used in this plausibility component is drawn from the upper and lower bounds of the forecasted prediction interval. As such, the only accepted signal format is [forecast][to_signal()], which will include upper and lower bounds.
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
#' @examples
#'
#' ## read in example observed data and prep observed signal
#' hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))
#' hosp$date <- as.Date(hosp$date, format = "%Y-%m-%d")
#' prepped_observed <- to_signal(hosp, outcome = "flu.admits", type = "observed", resolution = "weeks")
#'
#' ## read in example forecast and prep forecast signal
#' fp <- system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv", package = "rplanes")
#' prepped_forecast <- read_forecast(fp) %>%
#'   to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)
#'
#' ## prepare seed with cut date
#' prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-10-29")
#'
#' ## run plane component
#' plane_taper(location = "19", input = prepped_forecast, seed = prepped_seed)
#' plane_taper(location = "44", input = prepped_forecast, seed = prepped_seed)
plane_taper <- function(location, input, seed) {

  ## double check that location is in seed and input before proceeding
  valid_location(location, input, seed)

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
#' This function evaluates whether consecutive values in observations or forecasts are repeated a k number of times. This function takes in a [forecast][to_signal()] or [observed][to_signal()] object that is either from an observed dataset or forecast dataset. Note that if a signal is contant (i.e., the same value is repeated for all time points) then the repeat component will return `FALSE`.
#'
#' @param location Character vector with location code; the location must appear in input and seed
#' @param input Input signal data to be scored; object must be one of [forecast][to_signal()] or [observed][to_signal()]
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
#' @examples
#' ## read in example observed data and prep observed signal
#' hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))
#' hosp$date <- as.Date(hosp$date, format = "%Y-%m-%d")
#' prepped_observed <- to_signal(hosp, outcome = "flu.admits", type = "observed", resolution = "weeks")
#'
#' ## read in example forecast and prep forecast signal
#' fp <- system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv", package = "rplanes")
#' prepped_forecast <- read_forecast(fp) %>%
#'   to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)
#'
#' ## prepare seed with cut date
#' prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-10-29")
#'
#' ## run plane component
#' ## use defaults
#' plane_repeat(location = "12", input = prepped_forecast, seed = prepped_seed)
#' ## set tolerated repeats to 2
#' plane_repeat(location = "12", input = prepped_forecast, seed = prepped_seed, tolerance = 2)
#'
#' ## use defaults
#' plane_repeat(location = "49", input = prepped_forecast, seed = prepped_seed)
#' ## set number of values prepended for evaluation to 4
#' plane_repeat(location = "49", input = prepped_forecast, seed = prepped_seed, prepend = 4)
#'
plane_repeat <- function(location, input, seed, tolerance = NULL, prepend = NULL){

  ## double check that location is in seed and input before proceeding
  valid_location(location, input, seed)

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

  ## logic to check if data is constant in the reported signal
  ## if so ... we cannot fairly say there is a repeat so set to FALSE
  if(length(unique(tmp_seed$all_values)) == 1) {
    ind <- FALSE
  ## if not ... look at the repeat table to determine if flag is raised
  } else {
    ## indicator for whether or not the number of rows is > 0
    ## this would indicate that there are repeats
    ind <- nrow(repeat_tbl) > 0
  }

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
#' @param components Character vector specifying component; must be either `"all"` or any combination of `"cover"`, `"diff"`, `"taper"`, `"trend"`, `"repeat"`, `"shape"`, and `"zero"`; default is `"all"` and will use all available components for the given signal
#' @param args Named list of arguments for component functions. List elements must be named to match the given component and arguments passed as a nested list (e.g., `args = list("trend" = list("sig_lvl" = 0.05))`). Default is `NULL` and defaults for all components will be used
#' @param weights Named vector with weights to be applied; default is `NULL` and all components will be equally weighted; if not `NULL` then the length of the vector must equal the number of components, with each component given a numeric weight (see Examples). Specified weights must be real numbers greater than or equal to 1.
#'
#'
#'
#' @return
#'
#' A `list` with scoring results for all locations.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## read in example observed data and prep observed signal
#' hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))
#'
#' hosp$date <- as.Date(hosp$date, format = "%Y-%m-%d")
#' prepped_observed <- to_signal(hosp, outcome = "flu.admits", type = "observed", resolution = "weeks")
#'
#' ## read in example forecast and prep forecast signal
#' fp <- system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv", package = "rplanes")
#' prepped_forecast <- read_forecast(fp) %>%
#'   to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)
#'
#' ## prepare seed with cut date
#' prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-10-29")
#'
#' ## run plane scoring with all components
#' plane_score(input = prepped_forecast, seed = prepped_seed)
#'
#' ## run plane scoring with select components
#' plane_score(input = prepped_forecast, seed = prepped_seed, components = c("cover","taper"))
#'
#' ## run plane scoring with all components and additional args
#' comp_args <- list("trend" = list("sig_lvl" = 0.05), "repeat" = list("prepend" = 4, "tolerance" = 8))
#' plane_score(input = prepped_forecast, seed = prepped_seed, args = comp_args)
#'
#' ## run plane scoring with specific components and weights
#' comps <- c("cover", "taper", "diff")
#' wts <- c("cover" = 1.5, "taper" = 1, "diff" = 4)
#' plane_score(input = prepped_forecast, seed = prepped_seed, components = comps, weights = wts)
#'
#' }
plane_score <- function(input, seed, components = "all", args = NULL, weights = NULL) {

  ## TODO: create this list as a built-in object?
  ## NOTE: consider using getFromNamespace to simplify this step
  complist <-
    list(cover = list(.function = plane_cover),
         diff = list(.function = plane_diff),
         taper = list(.function = plane_taper),
         `repeat` = list(.function = plane_repeat),
         trend = list(.function = plane_trend),
         shape = list(.function = plane_shape),
         zero = list(.function = plane_zero)
    )

  ## verify components for signal type ... some won't apply to observed
  allowed_observed <- c("repeat","diff","zero")

  ## handle condition when "all" components are requested
  ## observed data will only have a subset (the allowed compoments above)
  if(length(components) == 1 && components == "all") {
    if(is_observed(input)) {
      components <- allowed_observed
    } else {
      components <- names(complist)
    }
  } else {
    ## now handle case when components have been defined as a character vector (not as "all")
    if(is_observed(input)) {
      if(any(!components %in% allowed_observed)) {
        warning(paste0("Only the following components are allowed for observed signals: ", paste0(allowed_observed, collapse = ";")))
      }
      components <- components[components %in% allowed_observed]
      ## check if components vector is empty and if so stop
      if(length(components) == 0) {
        stop("The signal is observed but none of the allowed components for observed signals were specified.")
      }
    }
  }

  ## get all possible locations
  ## TODO: make this easier to access in prepped signal by adding locations element
  locs <- unique(input$data$location)

  ## create combinations of locations and components for mapping below
  to_map <- tidyr::crossing(locs = locs, comps = components)

  ## TODO: better way do this mapping and tracking of location / components by name
  ## NOTE: the !!!args[[.x]] will look for the name of the component in the list of args ...
  ## ... if it is not there (e.g., args = NULL) then the function will proceed with no additional args ...
  ## ... if it is there then the !!! will splice the named arguments passed in a list and apply them
  retl <-
    purrr::map2(to_map$locs, to_map$comps, ~ purrr::exec(complist[[.y]]$.function, location = .x, input = input, seed = seed, !!!args[[.y]])) %>%
    purrr::set_names(paste0(to_map$locs, "-", to_map$comps))

  ## grab full list of component results
  full_results <- purrr::map(retl, purrr::pluck)

  ## separator regex
  ## this should create something like "-(?=[diff|trend|taper])"
  ## when applied below that will split *only on hyphens that are followed by diff or trend or taper
  sep_rx <- paste0("-(?=[", paste0(components,collapse = "|"), "])")

  ## pull out summary tibble components and locations from the returned list above
  loc_tbl <-
    dplyr::tibble(loc_component = names(retl), indicator = purrr::map_lgl(retl, "indicator")) %>%
    tidyr::separate(.data$loc_component, into = c("location", "component"), sep = sep_rx)

  which_flags <-
    loc_tbl %>%
    dplyr::group_by(.data$location) %>%
    dplyr::filter(.data$indicator) %>%
    dplyr::summarise(flagged = paste0(.data$component, collapse = ";"))

  ## construct a tibble with weights for components
  ## if the weights argument is NULL then apply equal weights to all components
  if(is.null(weights)) {
    weights_tbl <- dplyr::tibble(component = components, weight = 1)
  } else {
    if(any(weights < 1)) {
      stop("Weights must be a real number >= 1")
    }

    if(!all(sort(names(weights)) == sort(components))) {
      stop("Weights must be provided as a vector with all components used included by name (e.g., c('diff' = 4, 'cover' = 1))")
    }

    weights_tbl <- dplyr::tibble(component = names(weights), weight = weights)
  }

  ## convert the tibble into a list
  loc_list <-
    loc_tbl %>%
    ## join to weights tbl defined above
    dplyr::left_join(weights_tbl, by = "component") %>%
    ## count number of flags (numerator for score)
    ## count number of components (denominator for score)
    ## convert to score
    ## hold onto the names of the components used
    dplyr::group_by(.data$location) %>%
    dplyr::summarise(n_flags = sum(.data$indicator),
                     n_components = dplyr::n(),
                     n_flags_weighted = sum(.data$indicator * .data$weight),
                     weights_denominator = sum(.data$weight),
                     score = .data$n_flags_weighted / .data$weights_denominator,
                     components = paste0(.data$component, collapse = ";")
    ) %>%
    ## join back to tibble that enumerates which components were flagged
    dplyr::left_join(which_flags, by = "location") %>%
    ## split into a list by location
    dplyr::group_split(.data$location, .keep = TRUE) %>%
    ## use the location column from the tibble in each split list element as name
    purrr::set_names(purrr::map_chr(., "location")) %>%
    ## make sure it is a list of lists (not a list of tibbles)
    purrr::map(., as.list)

  return(list(scores_summary = loc_list, scores_raw = loc_tbl, full_results = full_results))

}

#' Trend component
#' @description
#'
#' This function identifies any change points in the forecast data or in the final observed data point. Change points are identified by any significant change in magnitude or direction of the slope of the time series.
#'
#' @param location Character vector with location code; the location must appear in input and seed
#' @param input Input signal data to be scored; object must be [forecast][to_signal()]
#' @param seed Prepared [seed][plane_seed()]
#' @param sig_lvl The significance level at which to identify change points (between zero and one); default is `0.1`
#'
#' @return
#' A `list` with the following values:
#'
#' - **indicator**: Logical as to whether or not the any forecast data or the final observed data point are a significant change point
#' - **output**: An n x 7 tibble. The length of the forecast plus the observed data determine the length of n. The columns are:
#'     - **Location**: A character vector with the location code
#'     - **Index**: An integer index of all observed and forecast data
#'     - **Date**: The dates corresponding to all observed and forecast data (formatted as date)
#'     - **Value**: The incidence of all observed and forecast data (e.g., hospitalization rates)
#'     - **Type**: Indicates whether the data row is observed or forecast data
#'     - **Changepoint**: Logical identifying any change point (whether in observed or forecast data). A TRUE is returned if any point is determined a change point based on the user defined significance level (sig_lvl).
#'     - **Flagged**: Logical indicating whether or not the change point was flagged. Change points are only flagged if they are in the forecast data or are the final observed data point. A TRUE is returned if the Changepoint is TRUE and is a final observed data point or any forecast point.
#' - **flagged_dates**: The date of any flagged change point(s). If there are none, NA is returned
#'
#' @details
#'
#' This function uses [e.divisive()][ecp::e.divisive()], which implements a hierarchical divisive algorithm to identify change points based on distances between segments (calculated using equations 3 and 5 in Matteson and James, 2014; the larger the distance, the more likely a change point). Then a permutation test is used to calculate an approximate p-value.
#'
#' Within e.divisive(), we use diff(x) instead of x (the raw data). This is a preference and slightly changes the way that change points are identified. When we use diff(x), the index aligns with the gap between points rather than the points themselves. Instead of identifying a change point based on the change in size between two points, it identifies change points based on the change in the change itself. For example, the dataframe below shows an example of x and diff(x):
#'
#' |**Index**|**x**| **diff(x)**|
#' | - |:--:| --:|
#' | 1 | 3  |  6 |
#' | 2 | 9  |  0 |
#' | 3 | 9  | 28 |
#' | 4 | 37 | 37 |
#' | 5 | 74 |  1 |
#' | 6 | 75 |  0 |
#' | 7 | 75 |  0 |
#'
#' Given this data, e.divisive(x) would identify index #5 (74) as the change point, because there was a jump of +37 between index 4 and 5. But e.divisive(diff(x)) would pick both index #3 (28) and #5 (1), because there was a jump of +28 from index 2 and 3, and there was a jump of -36 between index # 4 and 5. Ultimately, either way detects change points, but in this application (forecasting), diff(ex) is more discerning and less likely to identify change points haphazardly.
#'
#' Further, we specify min.size = 2, which means that we are forcing a gap of at least 2 points between detecting change points. In a roundabout way, this increases the significance level or at least decreases the number of change points identified. Should we decide to change the function so that we're not using diff(x), it probably makes sense to change min.size to 3.
#'
#' @references
#'
#'Matteson, D. S., & James, N. A. (2014). A nonparametric approach for multiple change point analysis of multivariate data. Journal of the American Statistical Association, 109(505), 334–345. https://doi.org/10.1080/01621459.2013.849605
#'
#' Matteson DS, James NA (2013). “A Nonparametric Approach for Multiple Change Point Analysis of Multivariate Data.” ArXiv e-prints. To appear in the Journal of the American Statistical Association, 1306.4933.
#'
#' Gandy, A. (2009) "Sequential implementation of Monte Carlo tests with uniformly bounded resampling risk." Journal of the American Statistical Association.
#'
#' @export
#'
#' @examples
#' ## read in example observed data and prep observed signal
#' hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))
#' tmp_hosp <-
#'   hosp %>%
#'   dplyr::select(date, location, flu.admits) %>%
#'   dplyr::mutate(date = as.Date(date))
#'
#' prepped_observed <- to_signal(tmp_hosp, outcome = "flu.admits",
#'                              type = "observed", resolution = "weeks")
#'
#' ## read in example forecast and prep forecast signal
#' prepped_forecast <- read_forecast(system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv",
#'                                                package = "rplanes")) %>%
#'    to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)
#'
#' ## prepare seed with cut date
#' prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-10-29")
#'
#' ## run plane component
#' plane_trend(location = "05", input = prepped_forecast, seed = prepped_seed, sig_lvl = .2)
#' ## change location
#' plane_trend(location = "09", input = prepped_forecast, seed = prepped_seed, sig_lvl = .2)
#' ## change sig_lvl
#' plane_trend(location = "06", input = prepped_forecast, seed = prepped_seed, sig_lvl = .05)
#'
plane_trend <- function(location, input, seed, sig_lvl = 0.1) {

  ## double check that location is in seed and input before proceeding
  valid_location(location, input, seed)

  tmp_seed <- seed[[location]]

  ## return the forecast data (with the filter on cut date)
  tmp_dat <-
    input$data %>%
    dplyr::filter(.data$location == .env$location) %>%
    dplyr::filter(.data$date > as.Date(tmp_seed$meta$cut_date, format = "%Y-%m-%d"))

  ## check that dates are valid (i.e., no observed data overlaps with seed
  valid_dates(seed_date = tmp_seed$meta$date_range$max, signal_date = min(tmp_dat$date), resolution = tmp_seed$meta$resolution)

  # Pull forecast point estimate:
  forepoint <-
    tmp_dat %>%
    dplyr::pull("point")

  ## how many observed points to "prepend" to forecasts?
  ## NOTE: this is set to 4 times as many observations as there are forecasted horizons. This number is also the index of the first relevant changepoint (i.e., the last observed data point)
  prepend_length <- 4 * length(forepoint)

  # Get dates for identifying change points
  dates <- c(tail(seq(tmp_seed$meta$date_range$min, tmp_seed$meta$date_range$max, by = tmp_seed$meta$resolution), prepend_length), tmp_dat$date)

  ## If there are fewer than 2 time stamps in forecast, abort
  if(length(forepoint) < 2) {
    stop(sprintf("%s forecast must be of length greater than one.", location))
  }

  # Pull observed points:
  obspoint <- tmp_seed$all_values

  ## If the observed/training data is not at least 4x as long as the forecast, abort
  if(length(obspoint) < prepend_length) {
    stop(sprintf("%s observed training data must be at least 4x the length of the forecast data.", location))
  }

  # Pull the last 4n observed points and concatenate with the forecasted points. Currently this only works with a 4 week horizon
  ex <- as.matrix(structure(c(tail(obspoint, prepend_length), forepoint))) # We want 4x as much training data as forecast data

  # Get break points. When k = NULL, all significant points are picked. Need to play around with the sig.lvl
  set.seed(123)
  ecp_output <- ecp::e.divisive(diff(ex), sig.lvl = sig_lvl, k = NULL, min.size = 2)
  # We use diff(ex) instead of the raw data, which is a preference and slightly changes the way the points are identified. When we use diff(ex), the index aligns with the gap between points rather than the points themselves. Instead of identifying a change point based on the change in size between two points, it identifies change points based on the change in the change itself. For example, the dataframe below shows an example of ex and diff(ex):
  # ex diff.ex.
  # 1  3        6
  # 2  9        0
  # 3  9       28
  # 4 37       37
  # 5 74        1
  # 6 75        0
  # 7 75        0

  # Given this data, e.divisive(ex) would identify index #5 (74) as the change point, because there was a jump of +37 between index 4 and 5. But e.divisive(diff(ex)) would pick both index #3 (28) and #5 (1), because there was a jump of +28 and from index #s 2 and 3, and there was a jump of -36 between index #s 4 and 5. Ultimately, either way detects change points, but diff(ex) seems to provide more information.

  # Further, we specify min.size = 2, which means that we are forcing a gap of at least 2 points between detecting change points. In a roundabout way, this increases the significance level or at least decreases the number of change points identified. Should we decide to change the function so that we're not using diff(ex), it probably makes sense to change min.size to 3.

  ecp_clean <- subset(ecp_output$estimates, subset = ecp_output$estimates > 1 & ecp_output$estimates < length(ex)) # Pulls all changepoints

  ## build tibble with all values and dates
  ## include logic to check for inflection points detected
  ## also check for "Flagged" as inflection points in the window of window of interest ...
  ## i.e., during forecast or last observed value
  output <- tibble::tibble(
    Location = location,
    Index = 1:length(ex),
    Date = dates,
    Value = ex[,1]) %>%
    dplyr::mutate(Type = dplyr::if_else(.data$Index > prepend_length, "Forecast", "Observed"),
                  Changepoint = dplyr::if_else(.data$Index %in% ecp_clean, TRUE, FALSE),
                  ## check for ind >= prepend len to ensure the last observation before forecast can be flagged
                  Flagged = dplyr::if_else(.data$Changepoint & .data$Index >= prepend_length, TRUE, FALSE))

  ## get the dates for flags
  flagged_dates <-
    output %>%
    dplyr::filter(.data$Flagged) %>%
    dplyr::pull(.data$Date)

  ## if there are none return NA
  if(length(flagged_dates) == 0) {
    flagged_dates <- NA
  }


  return(list(indicator = any(output$Flagged), output = output, flagged_dates = flagged_dates))

}


#' Shape component
#'
#' @description
#'
#' This function identifies the shape of the trajectory for a forecasted signal to compare against existing shapes in seed data. If the shape is identified as novel, a flag is raised, and the signal is considered implausible. See the Details section for further information.
#'
#' @param location Character vector with location code; the location must appear in input and seed
#' @param input Input signal data to be scored; object must be one of [forecast][to_signal()]
#' @param seed Prepared [seed][plane_seed()]
#'
#' @return
#'
#' A `list` with the following values:
#'
#' - **indicator**: Logical as to whether or not the the shape of the evaluated signal is novel (`TRUE` if shape is novel, `FALSE` if a familiar shape exists in the seed)
#'
#'
#' @details
#'
#' This function uses a Dynamic Time Warping (DTW) algorithm to identify shapes within the seed data and then compares the shape of the forecast input signal to the observed shapes. This is done in three broad steps:
#'
#' 1. The prepared [seed][plane_seed()] data is divided into a set of sliding windows with a step size of one, each representing a section of the overall time series. The length of these windows is determined by the horizon length of the input data signal (e.g., 2 weeks). If your seed data was a vector, `c(1, 2, 3, 4, 5)`, and your horizon length was 2, then the sliding windows for your observed seed data would be: `c(1, 2)`, `c(2, 3)`, `c(3, 4)`, and `c(4, 5)`. Each sliding window is a subset of the total trajectory shape of the observed data.
#'
#' 2. Shape-based DTW distances are calculated for every 1x1 combination of the observed sliding windows and are stored in a distance matrix. We use these distances to calibrate our function for identifying outlying shapes in forecast data.
#'
#'     - We find the minimum distances for each windowed time series to use as a baseline for "observed distances" between chunks of the larger observed time series.
#'     - We then calculate the maximum of those minimum distance across the observed time series. This will be our **threshold**. If the minimum of the forecast:observed distance matrix is greater than the greatest minimum observed:observed distance, then we can infer that the forecast is unfamiliar (i.e., a novel shape).
#'
#' 3. We calculate the shape-based DTW distances between the forecast signal (including the point estimate, lower, and upper bounds) and every observed sliding window. If the distance between the forecast and *any* observed sliding window is less than or equal to our threshold defined above, then this shape is not novel and no flag is raised (**indicator** = `FALSE`).
#'
#'
#' @references
#'
#' Toni Giorgino. Computing and Visualizing Dynamic Time Warping Alignments in R: The dtw Package. Journal of Statistical Software, 31(7), 1-24. doi:10.18637/jss.v031.i07
#'
#' Tormene, P.; Giorgino, T.; Quaglini, S. & Stefanelli, M. Matching incomplete time series with dynamic time warping: an algorithm and an application to post-stroke rehabilitation. Artif Intell Med, 2009, 45, 11-34. doi:10.1016/j.artmed.2008.11.007
#'
#' @export
#'
#' @examples
#' ## read in example observed data and prep observed signal
#' hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))
#'
#' tmp_hosp <-
#'  hosp %>%
#'  dplyr::select(date, location, flu.admits) %>%
#'  dplyr::mutate(date = as.Date(date))
#'
#' prepped_observed <- to_signal(tmp_hosp,
#'                                outcome = "flu.admits",
#'                                type = "observed",
#'                                resolution = "weeks")
#' ## read in example forecast and prep forecast signal
#' prepped_forecast <- read_forecast(system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv",
#'                                                 package = "rplanes")) %>%
#'    to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)
#'
#' ## prepare seed with cut date
#' prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-10-29")
#'
#' ## run plane component
#' ## this location is an example of where we expect a flag to be raised
#' plane_shape(location = "13", input = prepped_forecast, seed = prepped_seed)
#'
#' ## this location is an example of where we do not expect a flag to be raised
#' plane_shape(location = "06", input = prepped_forecast, seed = prepped_seed)
#'
#'
plane_shape <- function(location, input, seed) {

  ## double check that location is in seed and input before proceeding
  valid_location(location, input, seed)

  # The observed data:
  tmp_seed <- seed[[location]]

  ## return the forecast data (with the filter on cut date)
  tmp_dat <-
    input$data %>%
    dplyr::filter(.data$location == .env$location) %>%
    dplyr::filter(.data$date > as.Date(tmp_seed$meta$cut_date, format = "%Y-%m-%d"))

  ## check that dates are valid (i.e., no observed data overlaps with seed
  valid_dates(seed_date = tmp_seed$meta$date_range$max, signal_date = min(tmp_dat$date), resolution = tmp_seed$meta$resolution)

  # Pull forecast point estimate:
  forepoint <-
    tmp_dat %>%
    dplyr::pull("point")

  ## select only the PI and point estimate columns
  forecast <- tmp_dat[,c("lower","point","upper")]

  ## how many observed points to "prepend" to forecasts?
  ## NOTE: this is set to 4 times as many observations as there are forecasted horizons.
  prepend_length <- 4 * length(forepoint)

  # Get dates for observed and forecast data:
  dates <- c(seq(tmp_seed$meta$date_range$min, tmp_seed$meta$date_range$max, by = tmp_seed$meta$resolution), tmp_dat$date)

  ## If there are fewer than 2 time stamps in forecast, abort
  if(length(forepoint) < 2) {
    stop(sprintf("%s forecast must be of length greater than one.", location))
  }

  # Pull observed points:
  obspoint <- tmp_seed$all_values

  ## If the observed/training data is not at least 4x as long as the forecast, abort
  if(length(obspoint) < prepend_length) {
    stop(sprintf("%s observed training data must be at least 4x the length of the forecast data.", location))
  }

  # Set the window size and step size
  window_size <- input[["horizon"]]  # Adjust this based on your desired window size (horizon_length)

  # Create sliding windows and return a data frame
  obs_traj <- create_sliding_windows_df(obspoint, window_size)

  # Calculate all distances in distance matrix:
  distmat <- dtw::dtwDist(obs_traj)
  diag(distmat) <- NA

  # Find minimum distances for each time series to use as a baseline for "observed distances" between chunks of the larger observed time series:
  mins <- apply(distmat, 1, FUN = min, na.rm = TRUE)

  # Find the maximum, minimum distance across the observed time series. This will be our threshold. If the minimum of the forecast:observed distance matrix is less than or equal to the greatest, minimum observed distance, than we can infer that the forecast is not a novel shape
  threshold <- max(mins)


  ############################################################
  # Calculate forecast distances and flag any forecast that has an unusually high distance:

  ## split the observed windows matrix from above into a list
  list_obs_traj <-
    obs_traj %>%
    dplyr::mutate(index = 1:dplyr::n()) %>%
    dplyr::group_split(.data$index) %>%
    purrr::map(., ~dplyr::select(.x, -.data$index) %>% unlist(., use.names = FALSE))

  ## split the forecast components ...
  ## ... the lower bound, point estimate, upper bound ...
  ## ... into a list
  forc_list <- list(lower = forecast$lower,
                    point = forecast$point,
                    upper = forecast$upper)

  ## create all combinations of elements from these two lists
  to_map <- tidyr::crossing(obs = list_obs_traj, forc = forc_list)

  ## iterate over the combinations and compute distances
  ## NOTE: need to get the first element to get the vectors stored in the list
  dtw_distances <- purrr::map2_dbl(to_map$forc, to_map$obs, ~dtw::dtw(.x,.y)$distance)

  ## check to see if each distance is <= the defined threshold
  novel_shape_check <- purrr::map_lgl(dtw_distances, function(x) ifelse(x<= threshold, FALSE, TRUE))

  ## are any of the distances checked <= than the threshold?
  ## if so this will return TRUE and a flag should be raised
  ## if any of the shape check results are FALSE then this will return FALSE (no flag)
  novel_shape <- all(novel_shape_check)

  return(list(indicator = novel_shape))

}

#' Zero component
#'
#' @description
#'
#' This function checks for the presence of any value(s) equal to zero in the evaluated signal. If there are any zeros found, then the function assesses whether or not any zeros have been observed in the [seed][plane_seed()] for the given location. If so, the function will consider the evaluated zero plausible and no flag will be raised (i.e., indicator returned as `FALSE`). If not, the function will consider the evaluated zero implausible and a flag will be raised (i.e., indicator returned as `TRUE`).
#'
#' @param location Character vector with location code; the location must appear in input and seed
#' @param input Input signal data to be scored; object must be one of [forecast][to_signal()] or [observed][to_signal()]
#' @param seed Prepared [seed][plane_seed()]
#'
#' @return
#'
#' A `list` with the following values:
#'
#' - **indicator**: Logical as to whether or not there are zeros in evaluated signal but not in seed data
#'
#' @export
#'
#' @examples
#' ## read in example observed data and prep observed signal
#' hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))
#' hosp$date <- as.Date(hosp$date, format = "%Y-%m-%d")
#' prepped_observed <- to_signal(hosp, outcome = "flu.admits", type = "observed", resolution = "weeks")
#'
#' ## read in example forecast and prep forecast signal
#' fp <- system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv", package = "rplanes")
#' prepped_forecast <- read_forecast(fp) %>%
#'   to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)
#'
#' ## prepare seed with cut date
#' prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-10-29")
#'
#' ## run plane component
#' plane_zero(location = "10", input = prepped_forecast, seed = prepped_seed)
#' plane_zero(location = "51", input = prepped_forecast, seed = prepped_seed)
#'
plane_zero <- function(location, input, seed) {

  ## double check that location is in seed and input before proceeding
  valid_location(location, input, seed)

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

    ## pull the outcome values to be evaluated
    tmp_vals <-
      tmp_dat %>%
      dplyr::pull(input$outcome)

  } else if(is_forecast(input)) {
    ## return the forecast data (with the filter on cut date)
    tmp_dat <-
      input$data %>%
      dplyr::filter(.data$location == .env$location) %>%
      dplyr::filter(.data$date > as.Date(tmp_seed$meta$cut_date, format = "%Y-%m-%d"))

    ## check that dates are valid (i.e., no observed data doesnt overlap with seed
    valid_dates(seed_date = tmp_seed$meta$date_range$max, signal_date = min(tmp_dat$date), resolution = tmp_seed$meta$resolution)

    ## after all the checks ...
    ## pull the point estimates
    tmp_vals <-
      tmp_dat %>%
      dplyr::pull("point")

  }

  ## if there are no zeros in the seed (i.e., if any zeros is FALSE) ...
  ## ... then check if any evaluated points are 0
  ## otherwise set indicator to FALSE (and don't raise a flag)
  if(!tmp_seed$any_zeros) {
    ind <- any(tmp_vals == 0)
  } else {
    ind <- FALSE
  }

  return(list(indicator = ind))

}
