#' Title
#' @param .input needs location (FIPS); date (in YYYY-mm-dd format); outcome value
#' @param .outcome fixme
#' @param .type fixme
#' @param .resolution fixme
#' @param .horizon fixme
#'
#' @return
#' @export
#'
#' @examples
to_signal <- function(.input,
                      .outcome,
                      .type="observed",
                      .resolution = "weeks",
                      .horizon = NULL) {

  ## arg match if we want

  if(.type == "observed") {
    ## return special signal, observed list

    ## create exhaustive tibble with range of dates given specified resolution and all observations
    ## use this to check for data gaps
    tmp_expanded <-
      tidyr::crossing(date = seq(min(.input$date),max(.input$date),by=.resolution),
                      location = unique(.input$location))

    ## join to see if there are any gaps in data by resolution
    tmp_joined <- dplyr::left_join(tmp_expanded, .input)

    check_gaps <- any(is.na(tmp_joined[,.outcome]))

    if(check_gaps) {
      warning("There are gaps in the observed data for one or more locations.")
    }

    l <-
      list(data = .input,
           gaps = check_gaps,
           outcome = .outcome,
           resolution = .resolution)

    class(l) <- c("signal","observed")
    return(l)

  } else if (.type == "forecast") {

    ## TODO: add the forecast object prep here
    ## return special signal, forecast list with attributes (like horizon)
    ## check here that the number of horizons in input forecast matches the horizons specified

    stopifnot(!is.null(.horizon))

    l <-
      list(data = .input,
           horizon = .horizon,
           outcome = .outcome,
           resolution = .resolution)

    class(l) <- c("signal","forecast")
    return(l)

  } else {
    stop("The signal type must be one of 'observed' or 'forecast'.")
  }

}
