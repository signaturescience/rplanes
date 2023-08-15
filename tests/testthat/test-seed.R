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
