#' helper function used in plane_seed()
#'
#' @param .input fixme
#' @param .location fixme
#' @param .cut_date fixme
#'
#' @return
#' @export
#'
#' @examples
seed_engine <- function(.input, .location, .cut_date=NULL) {

  ## check class for signal, observed
  stopifnot(is_observed(.input))
  ## if no cut date is provided then just use the max
  if(is.null(.cut_date)) {
    .cut_date <-
      .input$data %>%
      dplyr::filter(.data$location == .location) %>%
      dplyr::pull(date) %>%
      max(.)
  } else {
    .cut_date <- as.Date(.cut_date, format = "%Y-%m-%d")
  }

  ## use cut date to get
  tmp_data <-
    .input$data %>%
    dplyr::filter(.data$location == .location) %>%
    dplyr::filter(.data$date <= .cut_date)

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
  last_val <- utils::tail(tmp_obs, 1)

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

#' driver to create seeds for every location in data
#'
#' @param .input fixme
#' @param .cut_date fixme
#'
#' @return
#' @export
#'
#' @examples
plane_seed <- function(.input, .cut_date=NULL) {
  locs <- unique(.input$data$location)

  purrr::map(locs, function(x) seed_engine(.input = .input, .location = x, .cut_date = .cut_date)) %>%
    purrr::set_names(locs)
}
