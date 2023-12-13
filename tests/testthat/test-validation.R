test_that("Basic date math works", {
  expect_true(valid_dates(seed_date = as.Date("2023-01-04"), signal_date = as.Date("2023-02-11"), resolution = "months"))
  expect_true(valid_dates(seed_date = as.Date("2023-01-01"), signal_date = as.Date("2023-01-08"), resolution = "weeks"))
  expect_true(valid_dates(seed_date = as.Date("2023-01-01"), signal_date = as.Date("2023-01-02"), resolution = "days"))
})

test_that("Date math works when time spans over years", {
  expect_true(valid_dates(seed_date = as.Date("2022-12-25"), signal_date = as.Date("2023-01-01"), resolution = "months"))
  expect_true(valid_dates(seed_date = as.Date("2022-12-28"), signal_date = as.Date("2023-01-01"), resolution = "weeks"))
  expect_true(valid_dates(seed_date = as.Date("2022-12-31"), signal_date = as.Date("2023-01-01"), resolution = "days"))
})

test_that("Error handling works for date math", {
  expect_error(valid_dates(seed_date = 99999, signal_date = as.Date("2023-01-01"), resolution = "weeks"))
  expect_error(valid_dates(seed_date = as.Date("2023-01-01"), signal_date = 99999, resolution = "weeks"))
  expect_error(valid_dates(seed_date = as.Date("2023-01-01"), signal_date = as.Date("2023-01-08"), resolution = "foo"))
  expect_error(valid_dates(seed_date = as.Date("2022-12-21"), signal_date = as.Date("2023-01-01"), resolution = "weeks"))
  expect_warning(valid_dates(seed_date = as.Date("2023-01-01"), signal_date = as.Date("2023-01-09"), resolution = "weekly", warn_incomplete = TRUE))

})

test_that("Error handling works for location validation", {

  ## prep data for tests below
  hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))

  tmp_hosp <-
    hosp %>%
    dplyr::select(date, location, flu.admits) %>%
    dplyr::mutate(date = as.Date(date))

  prepped_observed <- to_signal(tmp_hosp, outcome = "flu.admits", type = "observed", resolution = "weeks")

  prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-05-07")

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


  expect_error(valid_location("10", prepped_forecast, prepped_seed))
  expect_error(valid_location("Park Place", prepped_forecast, prepped_seed))
  expect_error(plane_diff("10", prepped_forecast, prepped_seed))
  expect_error(plane_diff("Park Place", prepped_forecast, prepped_seed))

})



