## prep data for tests below
hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes"))

tmp_hosp <-
  hosp %>%
  dplyr::select(date, location, flu.admits) %>%
  dplyr::mutate(date = as.Date(date))

prepped_observed <- to_signal(tmp_hosp, outcome = "flu.admits", type = "observed", resolution = "weeks")

prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-05-07")


test_that("plane_diff works", {

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
