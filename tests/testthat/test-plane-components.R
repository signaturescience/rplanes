## prep data for tests below
hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))

tmp_hosp <-
  hosp %>%
  dplyr::select(date, location, flu.admits) %>%
  dplyr::mutate(date = as.Date(date))

prepped_observed <- to_signal(tmp_hosp, outcome = "flu.admits", type = "observed", resolution = "weeks")

prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-05-07")

test_that("plane_diff flags large jump", {

  ## create some data to test
  ## make large point estimates to ensure a big jump => trigger diff flag
  point_est <- c(100,120,140,160)
  prepped_forecast <-
    dplyr::tibble(
      location = "01",
      date = seq(as.Date("2022-05-14"), as.Date("2022-06-04"), by = 7),
      horizon = 1:4,
      lower = point_est - 20,
      ## make a large jump in hospitalizations to trigger diff component
      point = point_est,
      upper = point_est + 20
    ) %>%
    to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

  expect_true(plane_diff("01", prepped_forecast, prepped_seed)$indicator)

  ## make point estimates that do not have a large jump
  point_est <- c(28,31,34,37)
  prepped_forecast <-
    dplyr::tibble(
      location = "01",
      date = seq(as.Date("2022-05-14"), as.Date("2022-06-04"), by = 7),
      horizon = 1:4,
      lower = point_est - 10,
      ## make a large jump in hospitalizations to trigger diff component
      point = point_est,
      upper = point_est + 10
    ) %>%
    to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

  expect_false(plane_diff("01", prepped_forecast, prepped_seed)$indicator)

})

test_that("plane_taper detects narrowing PI", {

  ## create some data to test
  point_est <- c(100,100,100,100)
  prepped_forecast <-
    dplyr::tibble(
      location = "01",
      date = seq(as.Date("2022-05-14"), as.Date("2022-06-04"), by = 7),
      horizon = 1:4,
      ## make the lower and upper bounds get narrower as horizon increases
      lower = point_est - c(20,15,10,5),
      point = point_est,
      upper = point_est + c(20,15,10,5)
    ) %>%
    to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

  expect_true(plane_taper("01", prepped_forecast, prepped_seed)$indicator)

  point_est <- c(100,100,100,100)
  prepped_forecast <-
    dplyr::tibble(
      location = "01",
      date = seq(as.Date("2022-05-14"), as.Date("2022-06-04"), by = 7),
      horizon = 1:4,
      ## make the lower and upper bounds get wider as horizon increases
      lower = point_est - 20,
      point = point_est,
      upper = point_est + 20
    ) %>%
    to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

  expect_false(plane_taper("01", prepped_forecast, prepped_seed)$indicator)

})

test_that("plane_cover identifies 1 week-ahead PI miss", {

  ## create some data to test
  ## make the 1 week-ahead point estimate and PI (below) miss the last reported obs
  point_est <- c(100,120,140,160)
  prepped_forecast <-
    dplyr::tibble(
      location = "01",
      date = seq(as.Date("2022-05-14"), as.Date("2022-06-04"), by = 7),
      horizon = 1:4,
      lower = point_est - 5,
      point = point_est,
      upper = point_est + 5
    ) %>%
    to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

  expect_true(plane_cover("01", prepped_forecast, prepped_seed)$indicator)

  ## create some data to test
  ## make the 1 week-ahead point estimate and PI (below) cover the last reported obs
  point_est <- c(28,31,34,37)
  prepped_forecast <-
    dplyr::tibble(
      location = "01",
      date = seq(as.Date("2022-05-14"), as.Date("2022-06-04"), by = 7),
      horizon = 1:4,
      lower = point_est - 28,
      point = point_est,
      upper = point_est + 28
    ) %>%
    to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

  expect_false(plane_cover("01", prepped_forecast, prepped_seed)$indicator)

})


test_that("plane_repeat detects too many repeating values", {

  ## create some data to test
  ## make sure the point estimates repeat
  point_est <- c(100,100,100,100)
  prepped_forecast <-
    dplyr::tibble(
      location = "01",
      date = seq(as.Date("2022-05-14"), as.Date("2022-06-04"), by = 7),
      horizon = 1:4,
      lower = point_est - 20,
      point = point_est,
      upper = point_est + 20
    ) %>%
    to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

  ## check with forecast alone (no prepend)
  expect_true(plane_repeat(input = prepped_forecast, location = "01", seed =  prepped_seed, tolerance = 3, prepend = 0)$indicator)
  ## check with forecast and default prepend behavior
  expect_true(plane_repeat(input = prepped_forecast, location = "01", seed =  prepped_seed,tolerance = 3, prepend = NULL)$indicator)
  ## check with a high tolerance
  expect_false(plane_repeat(input = prepped_forecast, location = "01", seed =  prepped_seed, tolerance = 4, prepend = NULL)$indicator)

  ## create some data to test
  ## make sure the point estimates do not repeat
  point_est <- c(100,120,140,160)
  prepped_forecast <-
    dplyr::tibble(
      location = "01",
      date = seq(as.Date("2022-05-14"), as.Date("2022-06-04"), by = 7),
      horizon = 1:4,
      lower = point_est - 20,
      point = point_est,
      upper = point_est + 20
    ) %>%
    to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

  expect_false(plane_repeat(input = prepped_forecast, location = "01", seed =  prepped_seed)$indicator)

  ## create some data to test
  ## make sure the forecast repeats the last value
  point_est <- c(prepped_seed$`01`$last_value,prepped_seed$`01`$last_value,30,35)
  prepped_forecast <-
    dplyr::tibble(
      location = "01",
      date = seq(as.Date("2022-05-14"), as.Date("2022-06-04"), by = 7),
      horizon = 1:4,
      lower = point_est - 5,
      point = point_est,
      upper = point_est + 5
    ) %>%
    to_signal(outcome = "flu.admits", type = "forecast", horizon = 4)

  expect_true(plane_repeat(input = prepped_forecast, location = "01", seed =  prepped_seed, tolerance = 2)$indicator)

})


test_that("plane_score returns summary based on components specified", {

  prepped_forecast <-
    read_forecast(system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv", package = "rplanes")) %>%
    to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)

  prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-10-29")

  ## check that the score function returns an overall object and that all locs are present
  res <- plane_score(prepped_forecast, prepped_seed)
  expect_s3_class(res$scores_raw, "data.frame")
  expect_type(res$scores_raw$indicator, "logical")
  expect_equal(length(unique(res$scores_raw$location)), length(unique(prepped_forecast$data$location)))

  ## check that the score function can be filtered to certain components
  res <- plane_score(prepped_forecast, prepped_seed, components = c("taper"))

  scored_comps <-
    res$scores_raw$component %>%
    unique(.) %>%
    sort(.)

  expect_equal(scored_comps, "taper")

  ## check that the score function can be filtered to certain components
  res <- plane_score(prepped_forecast, prepped_seed, components = c("cover", "diff", "taper"))

  scored_comps <-
    res$scores_raw$component %>%
    unique(.) %>%
    sort(.)

  expect_equal(scored_comps, c("cover", "diff", "taper"))

  ## check that the score function is inheriting parameters
  ## looking at this by increasing sig_level for trend (should increase sensitivity for flagging)
  ## and comparing to a lower sig_level to see if there are more flags
  res_high_sens <- plane_score(prepped_forecast, prepped_seed, components = c("trend"), args = list(trend = list(sig_lvl = 0.99)))
  res_low_sens <- plane_score(prepped_forecast, prepped_seed, components = c("trend"), args = list(trend = list(sig_lvl = 0.01)))

  expect_gt(sum(res_high_sens$scores_raw$indicator), sum(res_low_sens$scores_raw$indicator))

})

test_that("plane_score handles components for signals appropriately", {

  prepped_seed <- plane_seed(prepped_observed, cut_date = "2023-05-20")

  ## check that the score function only uses diff component if selected which should be fine for observed
  res <- plane_score(prepped_observed, prepped_seed, components = "diff")
  expect_equal(unique(res$scores_raw$component), "diff")

  ## check that score function can pick out relevant components
  ## in this case only diff should be used because this is an observed signal
  res <- plane_score(prepped_observed, prepped_seed, components = c("diff","taper"))
  expect_equal(unique(res$scores_raw$component), "diff")

  ## check that score errors if forecast-only components are used with observed
  expect_error(plane_score(prepped_observed, prepped_seed, components = c("taper","cover")))

})


test_that("plane_trend flags known changepoints and is sensitive to changes in sig.lvl", {

  prepped_seed2 <- plane_seed(prepped_observed, cut_date = "2022-10-29") # need this cut date to test plane_trend

  prepped_forecast <- read_forecast(system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv", package = "rplanes")) %>%
    to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)

  ## We know there is a changepoint at location 5 that should be flagged:
  expect_true(plane_trend(location = "05", input = prepped_forecast, seed = prepped_seed2, sig_lvl = .2)$indicator)

  ## We know that location 2 doesn't have any changepoints that should be flagged:
  expect_false(plane_trend(location = "02", input = prepped_forecast, seed = prepped_seed2, sig_lvl = .2)$indicator)

  ## Check that increasing the sensitivity by decreasing the significance level should produce different results:
  expect_false(identical(plane_trend(location = "05", input = prepped_forecast, seed = prepped_seed2, sig_lvl = .2)$indicator,
                         plane_trend(location = "05", input = prepped_forecast, seed = prepped_seed2, sig_lvl = .05)$indicator))

})


test_that("plane_shape flags novel shapes", {

  prepped_forecast <- read_forecast(system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv",
                                                package = "rplanes")) %>%
    to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)

  prepped_seed3 <- plane_seed(prepped_observed, cut_date = "2022-10-29") # need this cut date to test plane_shape

  ## We know there is a novel shape at location 13 that should be flagged:
  expect_true(plane_shape(location = "13", input = prepped_forecast, seed = prepped_seed3))

  ## We know that location 2 doesn't have any novel shapes that should be flagged:
  expect_false(plane_shape(location = "02", input = prepped_forecast, seed = prepped_seed3))

})
