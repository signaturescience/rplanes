## prep data for tests below
hosp <- read.csv(system.file("extdata/observed/hdgov_hosp_daily.csv", package = "rplanes"))

tmp_hosp <-
  hosp %>%
  dplyr::select(date, location, flu.admits) %>%
  dplyr::mutate(date = as.Date(date))

prepped_observed <- to_signal(tmp_hosp, outcome = "flu.admits", type = "observed", resolution = "days")

test_that("plane_seed creates seed for all locations at cut date specified", {

  prepped_seed <- plane_seed(prepped_observed, cut_date = "2022-04-29")

  expect_equal(length(prepped_seed), length(unique(tmp_hosp$location)))
  expect_lte(as.Date(prepped_seed$`51`$meta$date_range$max), as.Date("2022-04-29"))

})



test_that("plane_seed gets last value by date even if dates are not ordered ascending", {

  ## set a seed since we are sampling random values below
  set.seed(321)
  ## create mock surveillance data
  surv_data <-
    dplyr::tibble(
      ## set a location
      location = rep("United States", 21),
      ## randomly sample values
      cases = seq(300,200, by = -5),
      ## create vector of dates (descending)
      date =  as.Date("2023-01-01") - (7*(1:21))
    )

  ## convert mock surveillance data to an observed signal
  observed_signal <- to_signal(input = surv_data, outcome = "cases", type = "observed", resolution = "weeks", horizon = NULL)

  ## create the seed
  prepped_seed <- plane_seed(observed_signal)

  ## what is the last value in the seed?
  last_value_in_seed <- prepped_seed$`United States`$last_value

  ## what is the value on the last available date?
  last_value_by_date <-
    surv_data %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::pull(cases)

  expect_equal(last_value_in_seed, last_value_by_date)

})

