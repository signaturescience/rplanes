

test_that("Check that signal object types work", {

  hosp <-
    read.csv(system.file("extdata/observed/hdgov_hosp_daily.csv", package = "rplanes")) %>%
    dplyr::select(date, location, flu.admits) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    to_signal(., outcome = "flu.admits", type = "observed", resolution = "days")

  expect_true(is_observed(hosp))
  expect_false(is_forecast(hosp))

  tst_forecast <-
    read_forecast(system.file("extdata/forecast/2022-10-31-SigSci-TSENS.csv", package = "rplanes")) %>%
    to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4)

  expect_true(is_forecast(tst_forecast))
  expect_false(is_observed(tst_forecast))
})


test_that("Check to_signal requires proper signal type", {

  hosp <-
    read.csv(system.file("extdata/observed/hdgov_hosp_daily.csv", package = "rplanes")) %>%
    dplyr::select(date, location, flu.admits) %>%
    dplyr::mutate(date = as.Date(date))

  expect_error(to_signal(hosp, outcome = "flu.admits", type = "other", resolution = "weeks"))
})

test_that("Check to_signal does the gap checking", {

  hosp <-
    read.csv(system.file("extdata/observed/hdgov_hosp_weekly.csv", package = "rplanes")) %>%
    dplyr::select(date, location, flu.admits) %>%
    dplyr::mutate(date = as.Date(date))

  ## NOTE: to_signal() should warn about the gaps in the data so go ahead and expect warning
  expect_warning({
    obs_sig <- to_signal(hosp, outcome = "flu.admits", type = "observed", resolution = "days")}
  )

  expect_true(obs_sig$gaps)
})
