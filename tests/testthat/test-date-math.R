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

test_that("Error handling works", {
  expect_error(valid_dates(seed_date = 99999, signal_date = as.Date("2023-01-01"), resolution = "weeks"))
  expect_error(valid_dates(seed_date = as.Date("2023-01-01"), signal_date = 99999, resolution = "weeks"))
  expect_error(valid_dates(seed_date = as.Date("2023-01-01"), signal_date = as.Date("2023-01-08"), resolution = "foo"))
  expect_error(valid_dates(seed_date = as.Date("2022-12-21"), signal_date = as.Date("2023-01-01"), resolution = "weeks"))
  expect_warning(valid_dates(seed_date = as.Date("2023-01-01"), signal_date = as.Date("2023-01-09"), resolution = "weekly", warn_incomplete = TRUE))

})

