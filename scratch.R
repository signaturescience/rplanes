#######################################################################
## functions

## function to prep a probabilistic forecast format
## read_forecast()
read_forecast <- function(.file, .pi_width=95) {
  tmp_data <- readr::read_csv(.file)

  tmp_data %>%
    dplyr::mutate(epiweek = lubridate::epiweek(target_end_date),
                  epiyear = lubridate::epiyear(target_end_date)) %>%
    ## TODO: use .pi_width argument to construct vector of quantiles
    dplyr::filter(type == "point" | quantile %in% c(0.025,0.5,0.975)) %>%
    ## get target
    ## TODO: double check regex for str_extract  to get horizon from target value
    dplyr::mutate(horizon = stringr::str_extract(target, pattern = "^([0-9]|[0-9][0-9]|[0-9][0-9][0-9])")) %>%
    dplyr::mutate(quantile = ifelse(is.na(quantile), 0.5, quantile)) %>%
    dplyr::select(location,date = target_end_date, horizon, quantile, value) %>%
    dplyr::arrange(location,date,horizon,quantile) %>%
    dplyr::distinct_all() %>%
    tidyr::spread(quantile,value) %>%
    purrr::set_names(c("location","date","horizon","lower","point","upper"))
}


## .input needs location (FIPS); date (in YYYY-mm-dd format); outcome value
to_signal <- function(.input,
                      .outcome,
                      .type="observed",
                      .resolution = "weeks",
                      .horizon = NULL) {

  ## arg match if we want

  if(.type == "observed") {
    ## return special signal, observed list

    ## create exhaustive tibble with range of dates given specified resolution and all observations
    ## use this to check for data gaps
    tmp_expanded <-
      tidyr::crossing(date = seq(min(.input$date),max(.input$date),by=.resolution),
                      location = unique(.input$location))

    ## join to see if there are any gaps in data by resolution
    tmp_joined <- dplyr::left_join(tmp_expanded, .input)

    check_gaps <- any(is.na(tmp_joined[,.outcome]))

    if(check_gaps) {
      warning("There are gaps in the observed data for one or more locations.")
    }

    l <-
      list(data = .input,
           gaps = check_gaps,
           outcome = .outcome,
           resolution = .resolution)

    class(l) <- c("signal","observed")
    return(l)

  } else if (.type == "forecast") {

    ## TODO: add the forecast object prep here
    ## return special signal, forecast list with attributes (like horizon)
    ## check here that the number of horizons in input forecast matches the horizons specified

    stopifnot(!is.null(.horizon))

    l <-
      list(data = .input,
           horizon = .horizon,
           outcome = .outcome,
           resolution = .resolution)

    class(l) <- c("signal","forecast")
    return(l)

  } else {
    stop("The signal type must be one of 'observed' or 'forecast'.")
  }

}


## TODO: make sure these helpers are actually working as expected
## helpers
is_observed <- function(x) {
  all(class(x) == c("signal","observed"))
}

is_forecast <- function(x) {
  all(class(x) == c("signal","forecast"))
}

## helper function used in plane_seed()
seed_engine <- function(.input, .location, .cut_date=NULL) {

  ## check class for signal, observed
  stopifnot(is_observed(.input))
  ## if no cut date is provided then just use the max
  if(is.null(.cut_date)) {
    .cut_date <-
      .input$data %>%
      dplyr::filter(location == .location) %>%
      dplyr::pull(date) %>%
      max(.)
  } else {
    .cut_date <- as.Date(.cut_date, format = "%Y-%m-%d")
  }

  ## use cut date to get
  tmp_data <-
    .input$data %>%
    dplyr::filter(location == .location) %>%
    dplyr::filter(date <= .cut_date)

  ## get vector of observed values for the outcome
  tmp_obs <-
    tmp_data %>%
    dplyr::pull(.input$outcome)

  ## return max diff
  max_diff <-
    (tmp_obs - dplyr::lag(tmp_obs)) %>%
    abs(.) %>%
    max(., na.rm = TRUE)

  ## get range
  min_val <- min(tmp_obs, na.rm = TRUE)
  max_val <- max(tmp_obs, na.rm = TRUE)

  ## get last value
  last_val <- tail(tmp_obs, 1)

  ## TODO: return last k values (for repeat) ??
  ## TODO: return trends
  ## TODO: add other info needed for metrics downstream

  l <-
    list(
      diff = list(max = max_diff),
      range = list(min = min_val, max = max_val),
      last_value = last_val,
      ## TODO: add other metadata to this list
      meta = list(cut_date = .cut_date, resolution = .input$resolution, date_range = list(min = min(tmp_data$date), max = max(tmp_data$date)))
    )

  return(l)
}

## driver to create seeds for every location in data
plane_seed <- function(.input, .cut_date=NULL) {
  locs <- unique(.input$data$location)

  purrr::map(locs, function(x) seed_engine(.input = .input, .location = x, .cut_date = .cut_date)) %>%
    purrr::set_names(locs)
}

## plane funs take a forecast object OR observed object and a seed
plane_diff <- function(.location, .input, .seed) {

  ## TODO: add check for .location in names of seed
  tmp_seed <- .seed[[.location]]

  print(tmp_seed$meta$cut_date)
  ## check for class of input to see if it is observed
  ## if so ... filter on seed dates to so that we're comparing the observed of interest to seed vals
  ## TODO: do we need a check that the observed data doesn't overlap with seed ?
  if(is_observed(.input)) {
    tmp_dat <-
      .input$data %>%
      dplyr::filter(location == .location) %>%
      dplyr::filter(date > as.Date(tmp_seed$meta$cut_date, format = "%Y-%m-%d"))
    print(tmp_dat)

    ## pull the point estimate and concatenate with most recent value in seed
    tmp_vals <-
      tmp_dat %>%
      dplyr::pull(.input$outcome) %>%
      ## NOTE: need to pad here with repeat last value ...
      ## ... because the lag subtraction below will always return NA for the first element
      c(tmp_seed$last_value, tmp_seed$last_value, .)

  } else if(is_forecast(.input)) {
    ## check for class to see if it is forecast
    ## if so ... a couple checks for cut date
    ## we can use horizon 1 data for these checks
    tmp_dat_h1 <-
      .input$data %>%
      dplyr::filter(location == .location) %>%
      dplyr::filter(horizon == 1)

    ## first check to see if the date range in seed overlaps the forecast
    if(tmp_seed$meta$resolution == "days") {
      if((tmp_seed$meta$date_range$max - tmp_dat_h1$date) > 0) {
        stop("The seed data extends beyond the horizon forecasted ...")
      }
    } else if (tmp_seed$meta$resolution == "weeks") {
      if((tmp_seed$meta$date_range$max - tmp_dat_h1$date) > 7) {
        stop("The seed data extends beyond the horizon forecasted ...")
      }
    } else if (tmp_seed$meta$resolution == "months") {
      if((tmp_seed$meta$date_range$max - tmp_dat_h1$date) > 31) {
        stop("The seed data extends beyond the horizon forecasted ...")
      }
    }

    ## then make sure cut date immediately precedes the first forecast horizon
    ## IN PROGRESS
    ## get epiweek of cut date (and epiyear??? to handle spanning years)
    ## same for h1 date
    ## check that diff is no greater > 1
    ## or just check that dates minus each other are > 7
    if(tmp_seed$meta$resolution == "days") {
      if((tmp_dat_h1$date - tmp_seed$meta$date_range$max) > 1) {
        stop("The cut date is too far back ...")
      }
    } else if (tmp_seed$meta$resolution == "weeks") {
      if((tmp_dat_h1$date - tmp_seed$meta$date_range$max) > 7) {
        stop("The cut date is too far back ...")
        }
      } else if (tmp_seed$meta$resolution == "months") {
        if((tmp_dat_h1$date - tmp_seed$meta$date_range$max) > 31) {
          stop("The cut date is too far back ...")
        }
      }

    ## after all the checks ...
    ## return the forecast data (with the filter on cut date jic)
    tmp_dat <-
      .input$data %>%
      dplyr::filter(location == .location) %>%
      dplyr::filter(date > as.Date(tmp_seed$meta$cut_date, format = "%Y-%m-%d"))

    ## pull the point estimate and concatenate with most recent value in seed
    tmp_vals <-
      tmp_dat %>%
      dplyr::pull("point") %>%
      ## NOTE: need to pad here with repeat last value ...
      ## ... because the lag subtraction below will always return NA for the first element
      c(tmp_seed$last_value, tmp_seed$last_value, .)

  }


  all_diffs <- tmp_vals - dplyr::lag(tmp_vals,1)
  tst <- any(abs(all_diffs) > tmp_seed$diff$max, na.rm = TRUE)

  return(list(indicator = tst, all_differences = all_diffs[!is.na(all_diffs)], maximum_difference = tmp_seed$diff$max))


}

# plane_pi
# plane_trend
# plane_range
# plane_repeat
# plane_???

# plane_score() wraps all of the above

#######################################################################
## example
library(magrittr)
library(fiphde)
hosp <-
  get_hdgov_hosp(limitcols = TRUE) %>%
  prep_hdgov_hosp(., min_per_week = 0, remove_incomplete = TRUE, trim = list(epiyear = 2022, epiweek = 6))

tmp_hosp <-
  hosp %>%
  dplyr::select(date = week_start, location, flu.admits)

prepped_observed <- to_signal(tmp_hosp, .outcome = "flu.admits", .type = "observed", .resolution = "weeks")

## TODO: add to_signal logic that actually works for .type = "forecast"
prepped_forecast <- read_forecast("https://raw.githubusercontent.com/signaturescience/Flusight-forecast-data/SigSci-TSENS/data-forecasts/SigSci-TSENS/2022-05-16-SigSci-TSENS.csv") %>%
  to_signal(., .outcome = "flu.admits", .type = "forecast", .horizon = 4)

# prepped_seed <- plane_seed(prepped_observed, .cut_date = NULL)
prepped_seed <- plane_seed(prepped_observed, .cut_date = "2022-05-15")

prepped_seed

plane_diff(.location = "10", .input = prepped_observed, .seed = prepped_seed)
plane_diff(.location = "56", .input = prepped_observed, .seed = prepped_seed)
plane_diff(.location = "10", .input = prepped_forecast, .seed = prepped_seed)
