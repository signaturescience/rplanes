#' Title
#'
#' @param .location fixme
#' @param .input fixme
#' @param .seed fixme
#'
#' @return
#' @export
#'
#' @examples
#'
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
      dplyr::filter(.data$location == .location) %>%
      dplyr::filter(.data$date > as.Date(tmp_seed$meta$cut_date, format = "%Y-%m-%d"))
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
      dplyr::filter(.data$location == .location) %>%
      dplyr::filter(.data$horizon == 1)

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
      dplyr::filter(.data$location == .location) %>%
      dplyr::filter(.data$date > as.Date(tmp_seed$meta$cut_date, format = "%Y-%m-%d"))

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
