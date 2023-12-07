#' Seed engine
#'
#' @description
#'
#' This helper function is used inside of [plane_seed] to evaluate characteristics of observed data to use for downstream plausibility analysis.
#'
#' @param input Input signal data used for seeding; must be an observed signal object
#' @param location Character vector with location code
#' @param cut_date Maximum date (inclusive) for which seeding should be performed; default is `NULL` and the entire input will be used for seeding
#'
#' @return A `list` of length 1 with multiple elements corresponding to seed characteristics and metadata for the given location.
#'
#'
seed_engine <- function(input, location, cut_date=NULL) {

  ## check class for signal, observed
  stopifnot(is_observed(input))
  ## if no cut date is provided then just use the max
  if(is.null(cut_date)) {
    cut_date <-
      input$data %>%
      dplyr::filter(.data$location == .env$location) %>%
      dplyr::pull(date) %>%
      max(.)
  } else {
    cut_date <- as.Date(cut_date, format = "%Y-%m-%d")
  }

  ## use cut date to restrict data to appropriate time window
  tmp_data <-
    input$data %>%
    dplyr::filter(.data$location == .env$location) %>%
    ## NOTE: date *must* be arranged in ascending order for seeding
    dplyr::arrange(.data$date) %>%
    dplyr::filter(.data$date <= cut_date)

  ## get vector of observed values for the outcome
  tmp_obs <-
    tmp_data %>%
    dplyr::pull(input$outcome)

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

  ## get all values for repeat
  all_vals <- tmp_obs

  ## get max repeats
  max_repeats <-
    tmp_data %>%
    ## add an identifier for each set of repeating values
    ## consecutive_id will start counting at first value ...
    ## then keep the same id until it sees a new value ...
    ## then will iterate on id ...
    ## and repeat this procedure through the last row of the tibble
    dplyr::mutate(repeat_id = dplyr::consecutive_id(.data[[input$outcome]])) %>%
    ## using the ids created above we can count how many times each value repeats
    dplyr::add_count(.data$repeat_id, name = "n_repeats") %>%
    dplyr::pull("n_repeats") %>%
    max(.)


  ## TODO: return trends
  ## TODO: add other info needed for metrics downstream

  l <-
    list(
      diff = list(max = max_diff),
      range = list(min = min_val, max = max_val),
      all_values = all_vals,
      max_repeats = max_repeats,
      last_value = last_val,
      ## TODO: add other metadata to this list
      meta = list(cut_date = cut_date, resolution = input$resolution, date_range = list(min = min(tmp_data$date), max = max(tmp_data$date)))
    )

  return(l)
}

#' Create seed
#'
#' @description
#'
#' This function wraps the [seed_engine] to operate across all locations in the input signal.
#'
#'
#' @param input Input signal data used for seeding; must be an observed signal object
#' @param cut_date Maximum date (inclusive) for which seeding should be performed; default is `NULL` and the entire input will be used for seeding
#'
#' @return A named `list` of length *n*, where multiple elements corresponding to seed characteristics and metadata for each of the *n* locations are nested in independent lists.
#' @export
#'
#' @examples
#' ## read in example observed data and prep observed signal
#' hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))
#' hosp$date <- as.Date(hosp$date, format = "%Y-%m-%d")
#' prepped_observed <- to_signal(hosp, outcome = "flu.admits", type = "observed", resolution = "weeks")
#'
#' ## prepare seed with no cut date
#' plane_seed(prepped_observed)
#'
#' ## prepare seed with cut date
#' plane_seed(prepped_observed, cut_date = "2022-10-29")
#'
plane_seed <- function(input, cut_date=NULL) {
  locs <- unique(input$data$location)

  purrr::map(locs, function(x) seed_engine(input = input, location = x, cut_date = cut_date)) %>%
    purrr::set_names(locs)
}
