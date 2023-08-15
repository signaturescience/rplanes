#' Create signal object
#'
#' @description
#'
#' This function creates an object of the S3 class "signal". The user can conditionally specify either a "forecast" or "observed" signal.
#'
#' @param input Data to be converted to signal; see "Details" for more information
#' @param outcome Name of the outcome column in the input data
#' @param type Signal type; must be one of "observed" or "forecast"; default is "observed"
#' @param resolution The temporal resolution of the signal; data can be aggregated daily, weekly, or monthly; default is `"weeks"`
#' @param horizon Number of time steps ahead for forecast signals; only used if `type="forecast"`; default is `NULL`
#'
#' @details
#'
#' The input signal data may be either "observed" or "forecast" data. Depending on the type, the input data must conform to certain format prior to submission. In both cases, the data must be passed as a data frame.
#'
#' For "observed" data the data frame must at minimum include columns for **location** (geographic unit such as FIPS code) and **date** (date of reported value; must be `date` class). The data should also include a column that contains the outcome (e.g., case count).
#'
#' For "forecast" data the data frame must include columns for **location** (geographic unit such as FIPS code), **date** (date corresponding to forecast horizon; must be `date` class), **horizon** (forecast horizon), **lower** (the lower limit of the prediction interval for the forecast), **point** (the point estimate for the forecast), and **upper** (the upper limit of the prediction interval for the forecast). Note that the [read_forecast] function returns data in this format.
#'
#' @return An object of the class `signal`. The object will have a second class of either `observed` or `forecast` depending on the value passed to the "type" argument.
#' @export
#'
#' @examples
#' hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))
#' hosp$date <- as.Date(hosp$date, format = "%Y-%m-%d")
#' to_signal(hosp, outcome = "flu.admits", type = "observed", resolution = "weeks")
to_signal <- function(input,
                      outcome,
                      type="observed",
                      resolution = "weeks",
                      horizon = NULL) {

  ## arg match if we want

  if(type == "observed") {
    ## return special signal, observed list

    ## create exhaustive tibble with range of dates given specified resolution and all observations
    ## use this to check for data gaps
    tmp_expanded <-
      tidyr::crossing(date = seq(min(input$date),max(input$date),by=resolution),
                      location = unique(input$location))

    ## join to see if there are any gaps in data by resolution
    tmp_joined <- dplyr::left_join(tmp_expanded, input, by = c("date","location"))

    check_gaps <- any(is.na(tmp_joined[,outcome]))

    if(check_gaps) {
      warning("There are gaps in the observed data for one or more locations.")
    }

    l <-
      list(data = input,
           gaps = check_gaps,
           outcome = outcome,
           resolution = resolution)

    class(l) <- c("signal","observed")
    return(l)

  } else if (type == "forecast") {

    ## TODO: add the forecast object prep here
    ## return special signal, forecast list with attributes (like horizon)
    ## check here that the number of horizons in input forecast matches the horizons specified

    stopifnot(!is.null(horizon))

    l <-
      list(data = input,
           horizon = horizon,
           outcome = outcome,
           resolution = resolution)

    class(l) <- c("signal","forecast")
    return(l)

  } else {
    stop("The signal type must be one of 'observed' or 'forecast'.")
  }

}
