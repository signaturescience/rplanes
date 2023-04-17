
#' Check if object is observed
#'
#' @param x Input object to be checked
#'
#' @return Logical as to whether or not the input object inherits the "signal" and "observed" classes.
#' @export
#'
#' @examples
is_observed <- function(x) {
  all(class(x) == c("signal","observed"))
}

#' Check if object is forecast
#'
#' @param x Input object to be checked
#'
#' @return Logical as to whether or not the input object inherits the "signal" and "forecast" classes.
#' @export
#'
#' @examples
is_forecast <- function(x) {
  all(class(x) == c("signal","forecast"))
}

#' Read in forecast file
#'
#' @param file fixme
#' @param pi_width fixme
#'
#' @return A `tibble` with the following columns:
#'
#' - **location**
#' - **date**
#' - **horizon**
#' - **lower**
#' - **point**
#' - **upper**
#'
#' @export
#'
#' @examples
#'
read_forecast <- function(file, pi_width=95) {
  tmp_data <- readr::read_csv(file)

  tmp_data %>%
    dplyr::mutate(epiweek = lubridate::epiweek(.data$target_end_date),
                  epiyear = lubridate::epiyear(.data$target_end_date)) %>%
    ## TODO: use .pi_width argument to construct vector of quantiles
    dplyr::filter(.data$type == "point" | .data$quantile %in% c(0.025,0.5,0.975)) %>%
    ## get target
    ## TODO: double check regex for str_extract  to get horizon from target value
    dplyr::mutate(horizon = stringr::str_extract(.data$target, pattern = "^([0-9]|[0-9][0-9]|[0-9][0-9][0-9])")) %>%
    dplyr::mutate(quantile = ifelse(is.na(.data$quantile), 0.5, .data$quantile)) %>%
    dplyr::select(.data$location,date = .data$target_end_date, .data$horizon, .data$quantile, .data$value) %>%
    dplyr::arrange(.data$location,.data$date,.data$horizon,.data$quantile) %>%
    dplyr::distinct_all() %>%
    tidyr::spread(.data$quantile,.data$value) %>%
    purrr::set_names(c("location","date","horizon","lower","point","upper"))
}


#' Check that date span is valid
#'
#'
#' @param seed_date Last date available in seed object
#' @param signal_date First date available in signal object
#' @param resolution Character vector specifying the temporal resolution (e.g., "days", "weeks", "months")
#' @param warn_incomplete Logical as to whether or not the validation should warn for completeness of seed and signal; default is `FALSE`
#'
#' @return Invisible: a list of input values
#' @export
#'
#' @examples
#' seed_date <- as.Date("2023-03-08")
#' signal_date <- as.Date("2023-03-15")
#' valid_dates(seed_date = seed_date, signal_date = signal_date, resolution="weeks")
#' x <- try(valid_dates(seed_date = seed_date,
#'                           signal_date = signal_date,
#'                           resolution="days"), silent=TRUE)
#' x
#' x <- try(valid_dates(seed_date = seed_date,
#'                           signal_date = signal_date,
#'                           resolution="months"), silent=TRUE)
#' x
valid_dates <- function(seed_date, signal_date, resolution, warn_incomplete = FALSE) {

  ## handle the resolution argument
  resolution <- resolve_resolution(resolution)
  ## check that dates are formatted
  stopifnot(inherits(seed_date, "Date"))
  stopifnot(inherits(signal_date, "Date"))

  ## check that there is ...
  ## 1) no overlap of seed beyond signal
  ## 2) no gap between seed and signal
  ## these expectations vary by resolution
  ## for days
  if(resolution == "days") {
    if(seed_date > signal_date) {
      stop("Daily: seed_date extends beyond signal_date ...")
    } else if (signal_date-seed_date > 1) {
      stop("Daily: More than one day between the seed_date and signal_date ...")
    }
  ## for weeks
  } else if (resolution == "weeks") {
    ## get the week start for the signal date
    signal_epiweek_start <- epiweek_start(signal_date)
    ## get the week start for the seed date
    seed_epiweek_start <- epiweek_start(seed_date)
    ## calculate the expected week start for signal date
    expected_seed_epiweek_start <- signal_epiweek_start - 7
    if(seed_epiweek_start > expected_seed_epiweek_start) {
      stop("Weekly: seed_date extends beyond signal_date ...")
    } else if (seed_epiweek_start < expected_seed_epiweek_start) {
      stop("Weekly: More than one week between the seed_date and signal_date ...")
    }
  ## for months
  } else if (resolution == "months") {
    signal_month_start <- month_start(signal_date)
    seed_month_start <- month_start(seed_date)
    expected_seed_month_start <- signal_month_start %m-% months(1)
    if(seed_month_start > expected_seed_month_start) {
      stop("Monthly: seed_date extends beyond signal_date ...")
    } else if (seed_month_start < expected_seed_month_start) {
      stop("Monthly: More than one month between the seed_date and signal_date ...")
    }
  }

  if(warn_incomplete) {
    check_incomplete(seed_date = seed_date, signal_date = signal_date, resolution = resolution)
  }

  invisible(list(seed_date=seed_date, signal_date=signal_date, resolution=resolution))
}



#' Check completeness of seed and signal data
#'
#' @description
#'
#' This unexported helper is used internally in [valid_dates] to optionally issue a warning for potential completeness of seed and signal data based on dates provided.
#'
#' @param seed_date Last date available in seed object
#' @param signal_date First date available in signal object
#' @param resolution Character vector specifying the temporal resolution (e.g., "weeks", "months")
#'
#' @return Operates as side-effect and returns a `warning()` if there are the seed and signal dates combined indicate an incomplete week or month.
#'
#'
check_incomplete <- function(seed_date, signal_date, resolution) {

  ## handle the resolution argument
  resolution <- resolve_resolution(resolution)
  ## check that dates are formatted
  stopifnot(inherits(seed_date, "Date"))
  stopifnot(inherits(signal_date, "Date"))

  ## then make sure cut date immediately precedes the first forecast horizon
  ## NOTE: only week and month resolution can be considered "incomplete"
  if (resolution == "weeks") {
    if(signal_date-seed_date != 7) {
      warning("Weekly: The signal date and seed date are from subsequent weeks. The difference between the two dates does not equal 7 days, which could indicate incomplete seed and/or signal data.")
    }
  } else if (resolution == "months") {

    expected_diff <- as.numeric(month_start(signal_date) - month_start(seed_date))
    if(signal_date-seed_date != expected_diff) {
      warning(sprintf("Monthly: The signal date and seed date are from subsequent months. The difference between the two dates does not equal %d days, which could indicate incomplete seed and/or signal data.", expected_diff))
    }
  }
}

#' Resolve start of week from a given date
#'
#' @param date Date to be queried
#'
#' @return Date of the first day of the epiweek for the input date.
#'
#'
epiweek_start <- function(date) {

  ## get the epiyear and epiweek from the input date
  ey <- lubridate::epiyear(date)
  ew <- lubridate::epiweek(date)

  ## create the epiyear-epiweek format
  ## this will pad leading zeros for epiweek
  ## e.g., epiweek 1 in 2012 would be "2012-01"
  eyew <- paste0(ey, "-", sprintf("%02d", ew))

  ## use the built-in lookup object to find the first day (sunday) of the given eyew
  return(eyew_lookup[eyew_lookup$epiyear_epiweek == eyew,]$sunday)
}


#' Resolve start of month from a given date
#'
#' @param date Date to be queried
#'
#' @return
#'
month_start <- function(date) {

  tmp_year <- lubridate::year(date)
  tmp_month <- lubridate::month(date)

  return(as.Date(paste0(tmp_year, "-", tmp_month, "-01")))
}


#' Resolve resolution
#'
#' @description
#'
#' This helper function uses argument matching to resolve the resolution from input. The function also handles casing. This will allow, for example, an input resolution of "daily" or "day" to be resolved to "days".
#'
#' @param resolution Character vector specifying the temporal resolution (e.g., "days", "weeks", "months")
#'
#' @return If the resolution matches to "days", "weeks", or "months" then the match will be returned. If not, the function will throw an error.
#'
resolve_resolution <- function(resolution) {
  ## handle casing
  resolution <- tolower(resolution)
  ## get first two characters
  resolution <- strtrim(resolution, 2)
  ## match the first two characters to "days", "weeks", or "months"
  match.arg(resolution, choices = c("days","weeks","months"))
}
