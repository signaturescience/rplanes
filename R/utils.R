
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

#' Title
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
valid_dates <- function(seed_date, signal_date, resolution) {

  # Sanity checks
  stopifnot(inherits(seed_date, "Date"))
  stopifnot(inherits(signal_date, "Date"))
  stopifnot(resolution %in% c("days", "weeks", "months"))

  ## first check to see if the date range in seed overlaps the forecast
  if(resolution == "days") {
    if(seed_date > signal_date) {
      stop("Daily: seed_date extends beyond signal_date ...")
    }
  } else if (resolution == "weeks") {
    if (clock::add_weeks(seed_date, 1) > signal_date) {
      stop("Weekly: Less than one week between the seed_date and signal_date ...")
    }
  } else if (resolution == "months") {
    if(clock::add_months(seed_date, 1, invalid="previous") > signal_date) {
      stop("Monthly: Less than one month between the seed_date and signal_date ...")
    }
  }

  ## then make sure cut date immediately precedes the first forecast horizon
  if(resolution == "days") {
    if(signal_date-seed_date > 1) {
      stop("Daily: More than one day between the seed_date and signal_date ...")
    }
  } else if (resolution == "weeks") {
    if(clock::add_weeks(seed_date, 1)-signal_date > 1) {
      stop("Weekly: More than one week between the seed_date and signal_date ...")
    }
  } else if (resolution == "months") {
    if(clock::add_months(seed_date, 1, invalid="previous")-signal_date > 1) {
      stop("Monthly: More than one month between the seed_date and signal_date ...")
    }
  }
  invisible(list(seed_date=seed_date, signal_date=signal_date, resolution=resolution))
}
