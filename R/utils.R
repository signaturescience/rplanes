#' Check observed
#'
#' @description
#'
#' This function checks if the object is of class `signal` and `observed`.
#'
#'
#' @param x Input object to be checked
#'
#' @return Logical as to whether or not the input object inherits the "signal" and "observed" classes.
#'
#' @export
#'
#' @examples
#' hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))
#' hosp$date <- as.Date(hosp$date, format = "%Y-%m-%d")
#' sig <- to_signal(hosp, outcome = "flu.admits", type = "observed", resolution = "weeks")
#' is_observed(sig)
is_observed <- function(x) {
  all(class(x) == c("signal","observed"))
}

#' Check forecast
#'
#' @description
#'
#' This function checks if the object is of class `signal` and `forecast`.
#'
#' @param x Input object to be checked
#'
#' @return Logical as to whether or not the input object inherits the "signal" and "forecast" classes.
#' @export
#'
is_forecast <- function(x) {
  all(class(x) == c("signal","forecast"))
}

#' Read in forecast file
#'
#' @description
#'
#' This function reads a probabilistic ("quantile") forecast csv file and prepares it for the [to_signal] function and downstream plausibility analysis. The object returned is a tibble with summarized forecast data (i.e., prediction interval) for each location and horizon in the original file.
#'
#'
#' @param file Path to csv file to read
#' @param pi_width Width of prediction interval as integer; default `95` corresponds to 95% prediction interval
#'
#' @return A `tibble` with the following columns:
#'
#' - **location**: Geographic unit such as FIPS code
#' - **date**: Date corresponding the forecast horizon
#' - **horizon**: Forecast horizon
#' - **lower**: Lower limit of the prediction interval for the forecast. If forecast contains quantile predictions.
#' - **point**: Point estimate for the forecast
#' - **upper**: Upper limit of the prediction interval for the forecast. If forecast contains quantile predictions.
#'
#' @export
#'
#'
read_forecast <- function(file, pi_width=95) {
  ## use .pi_width argument to construct vector of quantiles. If quantiles not in quant_list, stop.
  width <- q_boundary(pi_width)
  # list of quantiles used in forecasts
  quant_list <- round(c(0.010, 0.025, 0.050, 0.100, 0.150, 0.200, 0.250, 0.300, 0.350, 0.400, 0.450, 0.500, 0.550, 0.600, 0.650, 0.700, 0.750, 0.800, 0.850, 0.900, 0.950, 0.975, 0.990), 3)
  stopifnot("Quantiles unavailable for width specified." = width %in% quant_list)

  ## read in csv with probabilistic forecast
  ## suppress message about readr guessing column types
  df <- readr::read_csv(file, show_col_types = FALSE)

  tmp_data <- df %>%
    dplyr::mutate(quantile = ifelse(is.na(.data$quantile), 0.5, .data$quantile))  %>%
    dplyr::mutate(epiweek = lubridate::epiweek(.data$target_end_date),
                  epiyear = lubridate::epiyear(.data$target_end_date)) %>%
    dplyr::filter(.data$type == "point" | .data$quantile %in% width) %>%
    ## str_extract between 1 to 3 digits, to get horizon from target value
    dplyr::mutate(horizon = stringr::str_extract(.data$target, pattern = "\\d{1,3}"))

  if (sum(stringr::str_count(unique(df$type), "quantile|point")) == 2){
    point_test <- df %>%
      dplyr::mutate(quantile = ifelse(is.na(.data$quantile), 0.5, .data$quantile)) %>%
      dplyr::filter(.data$quantile == 0.5) %>%
      dplyr::group_by(.data$forecast_date, .data$location, .data$target) %>%
      dplyr::mutate(not_equal = ifelse(.data$value[.data$type == "point"] != .data$value[.data$type == "quantile"], TRUE, FALSE)) %>%
      dplyr::filter(.data$type == "quantile" & .data$not_equal == TRUE) %>%
      dplyr::ungroup()

    tmp_data2 <- tmp_data  %>%
      # remove rows with quantile types whose values don't equal the point values, keeping the point value.
      dplyr::anti_join(point_test, by = c("forecast_date", "target", "location", "type", "quantile")) %>%
      ## NOTE: as of tidyselect v1.2.0 the .data pronoun is deprecated for select-ing
      dplyr::select("location", date = "target_end_date", "horizon", "quantile", "value") %>%
      dplyr::arrange(.data$location,.data$date,.data$horizon,.data$quantile) %>%
      dplyr::distinct_all() %>%
      tidyr::spread(.data$quantile, .data$value) %>%
      purrr::set_names(c("location","date","horizon","lower","point","upper"))

  } else {
    tmp_data2 <- tmp_data %>%
      dplyr::select("location", date = "target_end_date", "horizon", "quantile", "value") %>%
      dplyr::arrange(.data$location, .data$date, .data$horizon, .data$quantile) %>%
      dplyr::distinct_all() %>%
      tidyr::spread(.data$quantile, .data$value) %>%
      purrr::set_names(c("location","date","horizon","point"))
  }

  return(tmp_data2)
}


#' Validate dates
#'
#' @description
#'
#' This function validates that there are no gaps or overlaps between dates specified in the "seed_date" and "signal_date". During plausibility component analyses, the function is called to validate the seed against the evaluated signal.
#'
#'
#'
#' @param seed_date Last date available in seed object
#' @param signal_date First date available in signal object
#' @param resolution Character vector specifying the temporal resolution (e.g., "days", "weeks", "months")
#' @param warn_incomplete Logical as to whether or not the validation should warn for completeness of seed and signal; default is `FALSE`
#'
#' @return The validation will return with a `stop()` if there is an overlap or gap between seed and signal dates. Otherwise the function will invisibly return `TRUE` indicating that the date span is valid.
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
      stop("Daily: The seed date overlaps with signal date.")
    } else if (signal_date-seed_date > 1) {
      stop("Daily: There is a gap of more than one day between the seed date and signal date.")
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
      stop("Weekly: The seed date overlaps with signal date.")
    } else if (seed_epiweek_start < expected_seed_epiweek_start) {
      stop("Weekly: There is a gap of more than one week between the seed date and signal date.")
    }
  ## for months
  } else if (resolution == "months") {
    signal_month_start <- month_start(signal_date)
    seed_month_start <- month_start(seed_date)
    expected_seed_month_start <- signal_month_start %m-% months(1)
    if(seed_month_start > expected_seed_month_start) {
      stop("Monthly: The seed date overlaps with signal date.")
    } else if (seed_month_start < expected_seed_month_start) {
      stop("Monthly: There is a gap of more than one month between the seed date and signal date.")
    }
  }

  ## adding a step to optionally warn if the seed and signal data is "incomplete"
  ## this calls the unexported check_incomplete function
  ## effectively this function looks to see if there is ...
  ## != 7 days between seed and signal (for weekly)
  ## != n days in the given month between seed and signal (for monthly)
  if(warn_incomplete) {
    check_incomplete(seed_date = seed_date, signal_date = signal_date, resolution = resolution)
  }

  ## if the validation proceeds this far return TRUE
  return(invisible(TRUE))
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

#' Epiweek start
#'
#' @description
#'
#' This unexported helper identifies the date of the first day for the epiweek of the given date. The function is used internally inside of [valid_dates].
#'
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


#' Month start
#'
#' @description
#'
#' This unexported helper identifies the date of the first day of the month for the given date. The function is used internally inside of [valid_dates].
#'
#' @param date Date to be queried
#'
#' @return Date of the first day of the month for the input date.
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

#' Quantile boundary
#'
#' @description
#'
#' This unexported helper generates a vector of lower bound, median, and upper bound for the prediction interval of specified width. The function is used internally inside of [read_forecast].
#'
#'
#' @param pi_width Interval width as an integer
#'
#' @return Vector of quantiles corresponding to lower and upper bounds centered on median.
#'
#'
q_boundary <- function(pi_width) {
  half_width <- (pi_width/2)/100
  lower_upper <- 0.5 + (c(-1,1)*half_width)
  round(c(lower_upper[1], 0.5, lower_upper[2]), 3)
}
