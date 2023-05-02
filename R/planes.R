#' Title
#'
#' @param location fixme
#' @param input fixme
#' @param seed fixme
#'
#' @return
#' @export
#'
#' @examples
#'
plane_diff <- function(location, input, seed) {

  ## TODO: add check for location in names of seed
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

    print(tmp_dat)

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
