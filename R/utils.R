
#' Title
#'
#' @param x fixme
#'
#' @return
#' @export
#'
#' @examples
is_observed <- function(x) {
  all(class(x) == c("signal","observed"))
}

#' Title
#'
#' @param x fixme
#'
#' @return
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
#' @return
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
#' Description FIXME
#'
#' Details here FIXME
#'
#' @param seed_date fixme
#' @param signal_date fixme
#' @param resolution fixme
#' @param warn_incomplete FIXME
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

  resolution <- match.arg(resolution, choices = c("days","weeks","months"))
  # Sanity checks
  stopifnot(inherits(seed_date, "Date"))
  stopifnot(inherits(signal_date, "Date"))
  stopifnot(resolution %in% c("days", "weeks", "months"))

  ## first check to see if the date range in seed overlaps the forecast
  ## days
  if(resolution == "days") {
    if(seed_date > signal_date) {
      stop("Daily: seed_date extends beyond signal_date ...")
    } else if (signal_date-seed_date > 1) {
      stop("Daily: More than one day between the seed_date and signal_date ...")
    }
  ## weeks
  } else if (resolution == "weeks") {
    signal_epiweek_start <- epiweek_start(signal_date)
    seed_epiweek_start <- epiweek_start(seed_date)
    expected_seed_epiweek_start <- signal_epiweek_start - 7
    if(seed_epiweek_start > expected_seed_epiweek_start) {
      stop("Weekly: seed_date extends beyond signal_date ...")
    } else if (seed_epiweek_start < expected_seed_epiweek_start) {
      stop("Weekly: More than one week between the seed_date and signal_date ...")
    }
  ## months
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



#' Title
#'
#' @param seed_date FIXME
#' @param signal_date FIXME
#' @param resolution FIXME
#'
#' @return
#' @export
#'
#' @examples
#'
check_incomplete <- function(seed_date, signal_date, resolution) {

  resolution <- match.arg(resolution, choices = c("days","weeks","months"))
  ## TODO: add some arg matching for resolution to get day/daily, week/weekly, etc.
  # Sanity checks
  stopifnot(inherits(seed_date, "Date"))
  stopifnot(inherits(signal_date, "Date"))
  stopifnot(resolution %in% c("days", "weeks", "months"))

  # ## then make sure cut date immediately precedes the first forecast horizon
  # if(resolution == "days") {
  #   if(signal_date-seed_date > 1) {
  #     warning("Daily: More than one day between the seed_date and signal_date ...")
  #   }
  # } else if (resolution == "weeks") {
  #   if(clock::add_weeks(signal_date, 1)-seed_date > 1) {
  #     warning("Weekly: More than one week between the seed_date and signal_date ...")
  #   }
  # } else if (resolution == "months") {
  #   if(clock::add_months(signal_date, 1, invalid="previous")-seed_date > 1) {
  #     warning("Monthly: More than one month between the seed_date and signal_date ...")
  #   }
  # }

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
#' @param date FIXME
#'
#' @return
#' @export
#'
#' @examples
#'
epiweek_start <- function(date) {

  ey <- lubridate::epiyear(date)
  ew <- lubridate::epiweek(date)

  eyew <- paste0(ey, "-", sprintf("%02d", ew))

  return(eyew_lookup[eyew_lookup$epiyear_epiweek == eyew,]$sunday)
}


#' Resolve start of month from a given date
#'
#' @param date FIXME
#'
#' @return
#' @export
#'
#' @examples
month_start <- function(date) {

  tmp_year <- lubridate::year(date)
  tmp_month <- lubridate::month(date)

  return(as.Date(paste0(tmp_year, "-", tmp_month, "-01")))
}
