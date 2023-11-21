#' Shape Component
#'
#' @description
#'
#' This function ...
#'
#' @param location Character vector with location code; the location must appear in input and seed
#' @param input Input signal data to be scored; object must be one of [forecast][to_signal()] or [observed][to_signal()]
#' @param seed Prepared [seed][plane_seed()]
#'
#' @return
#'
#' A `list` with the following values:
#'
#' - **indicator**: Logical as to whether or not the shape of the evaluated signal is novel
#' - **output**: An n x 5 tibble. The columns are:
#'     - **date**: The dates corresponding to all observed and forecast data (formatted as date)
#'     - **point**: The incidence of all observed and forecast data (e.g., hospitalization rates)
#'     - **type**: Indicates whether the data row is observed or forecast data
#'     - **indicator**: Logical as to whether or not the shape of the evaluated signal is novel
#'     - **location**: A character vector with the location code
#'
#' @details
#' asdfadsf
#' @references
#'asdfasfa
#' @export
#'
#' @examples
#'adfsadfa
#'
plane_shape <- function(location, input, seed) {

  if(!location %in% names(seed)) {
    stop(sprintf("%s does not appear in the seed object. Check that the seed was prepared with the location specified.", location))
  }

  # The observed data:
  tmp_seed <- seed[[location]]

  ## return the forecast data (with the filter on cut date)
  tmp_dat <-
    input$data %>%
    dplyr::filter(.data$location == .env$location) %>%
    dplyr::filter(.data$date > as.Date(tmp_seed$meta$cut_date, format = "%Y-%m-%d"))

  ## check that dates are valid (i.e., no observed data overlaps with seed
  valid_dates(seed_date = tmp_seed$meta$date_range$max, signal_date = min(tmp_dat$date), resolution = tmp_seed$meta$resolution)

  # Pull forecast point estimate:
  forepoint <-
    tmp_dat %>%
    dplyr::pull("point")

  ## select only the PI and point estimate columns
  forecast <- tmp_dat[,c("lower","point","upper")]

  ## how many observed points to "prepend" to forecasts?
  ## NOTE: this is set to 4 times as many observations as there are forecasted horizons.
  prepend_length <- 4 * length(forepoint)

  # Get dates for observed and forecast data:
  dates <- c(seq(tmp_seed$meta$date_range$min, tmp_seed$meta$date_range$max, by = tmp_seed$meta$resolution), tmp_dat$date)

  ## If there are fewer than 2 time stamps in forecast, abort
  if(length(forepoint) < 2) {
    stop(sprintf("%s forecast must be of length greater than one.", location))
  }

  # Pull observed points:
  obspoint <- tmp_seed$all_values

  ## If the observed/training data is not at least 4x as long as the forecast, abort
  if(length(obspoint) < prepend_length) {
    stop(sprintf("%s observed training data must be at least 4x the length of the forecast data.", location))
  }

  # Set the window size and step size
  window_size <- prepped_forecast[["horizon"]]  # Adjust this based on your desired window size (horizon_length)

  # Create sliding windows and return a data frame
  obs_traj <- create_sliding_windows_df(obspoint, window_size)

  # Calculate all distances in distance matrix:
  distmat <- dtw::dtwDist(obs_traj)
  diag(distmat) <- NA

  # Find minimum distances for each time series to use as a baseline for "observed distances" between chunks of the larger observed time series:
  mins <- apply(distmat, 1, FUN = min, na.rm = TRUE)

  # Find the maximum, minimum distance across the observed time series. This will be our threshold. If the minimum of the forecast:observed distance matrix is less than or equal to the greatest, minimum observed distance, than we can infer that the forecast is not a novel shape
  threshold <- max(mins)


  ############################################################
  # Calculate forecast distances and flag any forecast that has an unusually high distance:

  ## split the observed windows matrix from above into a list
  list_obs_traj <-
    obs_traj %>%
    dplyr::mutate(index = 1:dplyr::n()) %>%
    dplyr::group_split(index) %>%
    map(., ~dplyr::select(.x, -index) %>% unlist(., use.names = FALSE))

  ## split the forecast components ...
  ## ... the lower bound, point estimate, upper bound ...
  ## ... into a list
  forc_list <- list(lower = forecast$lower,
                    point = forecast$point,
                    upper = forecast$upper)

  ## create all combinations of elements from these two lists
  to_map <- tidyr::crossing(obs = list_obs_traj, forc = forc_list)

  ## iterate over the combinations and compute distances
  ## NOTE: need to get the first element to get the vectors stored in the list
  dtw_distances <- purrr::map2_dbl(to_map$forc, to_map$obs, ~dtw::dtw(.x,.y)$distance)

  ## check to see if each distance is <= the defined threshold
  novel_shape_check <- purrr::map_lgl(dtw_distances, function(x) ifelse(x<= threshold, FALSE, TRUE))

  ## are any of the distances checked <= than the threshold?
  ## if so this will return TRUE and a flag should be raised
  ## if any of the shape check results are FALSE then this will return FALSE (no flag)
  novel_shape <- all(novel_shape_check)

  output <- tibble::tibble("date" = dates,
                           "point" = c(obspoint, forepoint),
                           "type" = c(rep("observed", length(obspoint)), rep("forecast", length(forepoint))),
                           "indicator" = rep(novel_shape, length(dates)),
                           "location" = rep(location, length(dates)))

  return(list(indicator = novel_shape, output = output))


}
