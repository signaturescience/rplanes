
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
