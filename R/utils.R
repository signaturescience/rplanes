
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


#' Check date spans
#'
#' Description FIXME
#'
#' Details here FIXME
#'
#' @param seed_date fixme
#' @param this_date fixme
#' @param resolution fixme
#'
#' @return Invisible: a list of input values
#' @export
#'
#' @examples
#' seed_date <- as.Date("2023-03-08")
#' this_date <- as.Date("2023-03-15")
#' check_date_spans(seed_date = seed_date, this_date = this_date, resolution="weeks")
#' x <- try(check_date_spans(seed_date = seed_date,
#'                           this_date = this_date,
#'                           resolution="days"), silent=TRUE)
#' x
#' x <- try(check_date_spans(seed_date = seed_date,
#'                           this_date = this_date,
#'                           resolution="months"), silent=TRUE)
#' x
check_date_spans <- function(seed_date, this_date, resolution) {

  # Sanity checks
  stopifnot(inherits(seed_date, "Date"))
  stopifnot(inherits(this_date, "Date"))
  stopifnot(identical(length(resolution), 1L))
  stopifnot(inherits(resolution, "character"))
  stopifnot(resolution %in% c("days", "weeks", "months"))

  ## first check to see if the date range in seed overlaps the forecast
  if(resolution == "days") {
    if(seed_date > this_date) {
      stop("Daily: seed_date extends beyond this_date ...")
    }
  } else if (resolution == "weeks") {
    if (clock::add_weeks(seed_date, 1) > this_date) {
      stop("Weekly: Less than one week between the seed_date and this_date ...")
    }
  } else if (resolution == "months") {
    if(clock::add_months(seed_date, 1, invalid="previous") > this_date) {
      stop("Monthly: Less than one month between the seed_date and this_date ...")
    }
  }

  ## then make sure cut date immediately precedes the first forecast horizon
  if(resolution == "days") {
    if(this_date-seed_date > 1) {
      stop("Daily: More than one day between the seed_date and this_date ...")
    }
  } else if (resolution == "weeks") {
    if(clock::add_weeks(seed_date, 1)-this_date > 1) {
      stop("Weekly: More than one week between the seed_date and this_date ...")
    }
  } else if (resolution == "months") {
    if(clock::add_months(seed_date, 1, invalid="previous")-this_date > 1) {
      stop("Monthly: More than one month between the seed_date and this_date ...")
    }
  }
  invisible(list(seed_date=seed_date, this_date=this_date, resolution=resolution))
}
